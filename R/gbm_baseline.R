#-------------------------------------------------------------------------------
# RMV2.0 (version 1.1.0)
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#
#     Gradient Boosting machine baseline model function
#
#-------------------------------------------------------------------------------
#' Gradient boosting machine baseline model function.
#'
#' \code{gbm_baseline} This function builds a baseline model using gradient boosting machine algorithm.
#'
#'
#' @param train_path The path of the file from which the training data are to be read.
#' @param pred_path The path of the file from which the prediction data are to be read.
#' @param days_off_path The path of the file from which the date data of days off (e.g., holidays) are to be read.
#' @param train_Data A dataframe, of the training period, where the columns correspond to the time steps (time), the energy load (eload) and to the Temperature (Temp).
#' @param pred_Data A dataframe, of the prediction period, where the columns correspond to the time steps (time), the energy load (eload) and to the Temperature (Temp).
#' @param k_folds An integer that corresponds to the number of CV folds.
#' @param variables A vector that contains the names of the variables that will be considered by the function
#' as input variables.
#' @param ncores Number of threads used for the parallelization of the cross validation.
#' @param cv_blocks Type of blocks for the cross validation; Default is "none", which correspond
#' to the standard k-fold cross validation technique.
#' @param iter A vector with combination of the number of iterations.
#' @param depth A vector with combination of the maximum depths.
#' @param lr A vector with combination of the learning rates.
#' @param subsample A vector with combination of subsamples.
#'
#' @return a gbm_baseline object, which is a list with the following components:
#' \describe{
#'   \item{gbm_model}{an object that has been created by the function xgboost,
#'    and which correspond to the optimal gbm model.}
#'   \item{train}{a dataframe that correspond to the training data after the
#'   cleaning and filtering function were applied.}
#'   \item{fitting}{the fitted values.}
#'   \item{goodness_of_fit}{a dataframe that contains the goodness of fitting metrics.}
#'   \item{gbm_cv_results}{a dataframe the training accuracy metrics (R2,
#'   RMSE and CVRMSE) and values of the tuning hype-parameters.}
#'   \item{tuned_parameters}{a list of the best hyper-parameters}
#'   \item{pred}{a dataframe that correspond to the prediction data after the
#'   cleaning and filtering function were applied.}
#'   \item{prediction}{the predicted values.}
#' }
#' @export

gbm_baseline <- function(train_path = NULL,
                         pred_path = NULL,
                         days_off_path = NULL,
                         train_Data = NULL,
                         pred_Data = NULL,
                         k_folds=5,
                         variables = c("Temp","tow"),
                         ncores = parallel::detectCores(logical =F),
                         cv_blocks = "weeks",
                         iter = seq(from=50,to=300,by=25),
                         depth = c(3:7),
                         lr = c(0.05,0.1),
                         subsample=c(0.5),
                         verbose = FALSE){
  # train read and preprocess
  if (!is.null(train_path)){
    train <- read.csv(file = train_path, header=T,
                      row.names = NULL, stringsAsFactors = F)
  }
  else {train <- train_Data}
  train <- time_features(train)
  if (!is.null(days_off_path)) {
   train <- days_off_var(days_off_path,train)
   variables <- c(variables,"days_off")
  }
  train <- clean_Temp(train)
  train <- clean_eload(train)
  if (verbose){
    cat('"================================="\n')
    cat('"Model Tuning"\n')
    cat('"================================="\n')
  }
  tune_results <- gbm_tune(train,
                           k_folds = k_folds,
                           variables = variables,
                           ncores = ncores,
                           cv_blocks = cv_blocks,
                           iter = iter,
                           depth = depth,
                           lr = lr,
                           subsample = subsample)

  tuned_parameters <- tune_results$tuned_parameters
  gbm_cv_results <- tune_results$grid_results

  # Final gbm model
  train_output <- train$eload
  train_input <- train[,variables]
  if (verbose){
    cat('"================================="\n')
    cat('"Final Model Training"\n')
    cat('"================================="\n')
  }
  gbm_model <- xgboost::xgboost(data = as.matrix(train_input),
                                label = train_output,
                                max_depth = tuned_parameters$best_depth,
                                eta = tuned_parameters$best_lr,
                                nrounds = tuned_parameters$best_iter,
                                subsample = tuned_parameters$best_subsample,
                                verbose = 0,
                                nthread = 1,
                                save_period = NULL)

  # Fitting:
  y_fit <- predict(gbm_model, as.matrix(train_input))
  fit_residuals <- train_output - y_fit

  goodness_of_fit <- as.data.frame(matrix(nr=1,nc=3))
  names(goodness_of_fit) <- c("fit_R2","fit_CVRMSE","fit_NMBE")
  goodness_of_fit$fit_R2 <- 100*(1-mean((fit_residuals)^2)/var(train_output))
  goodness_of_fit$fit_CVRMSE <- 100*sqrt(mean((fit_residuals)^2))/mean(train_output)
  goodness_of_fit$fit_NMBE <- 100*mean((fit_residuals))/mean(train_output)

  res <- NULL
  res$gbm_model <- gbm_model
  res$train <- train
  res$fitting <- y_fit
  res$goodness_of_fit <- goodness_of_fit
  res$gbm_cv_results <- gbm_cv_results
  res$tuned_parameters <- tuned_parameters

  # Prediction:
  if (!is.null(pred_path) | !is.null(pred_Data)) {
    if (verbose){
      cat('"================================="\n')
      cat('"Prediction"\n')
      cat('"================================="\n')
    }
    # pred read and preprocessing
    if (!is.null(pred_path)) {
      pred <- read.csv(file = pred_path, header=T,
                       row.names = NULL, stringsAsFactors = F)
    }
    else {pred <- pred_Data}
    pred <- time_features(pred)
    if (!is.null(days_off_path)) {
      pred <- days_off_var(days_off_path,pred)
    }
    pred <- clean_Temp(pred)
    pred_input <- pred[,variables]
    y_pred <- predict(gbm_model, as.matrix(pred_input))
    res$pred <- pred
    res$prediction <- y_pred
  }
  return(res)
}



#-------------------------------------------------------------------------------
#
#     Gradient Boosting machine tuning function
#
#-------------------------------------------------------------------------------
#' Gradient boosting machine tuning function.
#'
#' \code{gbm_tune} Function used to tune the hyper parameters of the GBM model.
#'
#'
#' @param Data A dataframe.
#' @param k_folds An integer that corresponds to the number of CV folds.
#' @param variables A vector that contains the names of the variables that will be considered by the function
#' as input variables.
#' @param ncores Number of threads used for the parallelization of the cross validation
#' @param cv_blocks type of blocks for the cross validation; Default is "none", which corresponds
#' to the standard cross validation technique
#' @param iter A vector with combination of the number of iterations.
#' @param depth A vector with combination of the maximum depths.
#' @param lr A vector with combination of the learning rates.
#' @param subsample A vector with combination of subsamples.
#'
#' @return a list with the two following components:
#' \describe{
#'   \item{grid_results}{a dataframe the training accuracy metrics (R2,
#'   RMSE and CVRMSE) and values of the tuning hype-parameters }
#'   \item{tuned_parameters}{a list of the best hyper-parameters}
#' }
#'
#' @export

gbm_tune <- function(Data,
                     k_folds,
                     variables = c("Temp","tow"),
                     ncores,
                     cv_blocks = "none",
                     iter,
                     depth,
                     lr,
                     subsample){
  cl <- parallel::makeCluster(ncores)
  output <- Data$eload
  input <- Data[,variables]
  if (cv_blocks=="days"){
    list_train <- k_dblocks_cv(Data,k_folds)
  }
  if (cv_blocks=="weeks"){
    list_train <- k_wblocks_cv(Data,k_folds)
  }
  if (cv_blocks=="none"){
    list_train <- caret::createFolds(y = output,k = k_folds,list = T)
  }
  gbm_grid <-  expand.grid(nrounds = iter,
                           max_depth = depth,
                           eta = lr,
                           subsample = subsample)

  n_grid <- dim(gbm_grid)[1]
  tab_grid_res <- data.frame(matrix(ncol = 10, nrow = n_grid))
  names(tab_grid_res) <- c("iter","depth","lr","subsample",
                           "R2","RMSE","CVRMSE",
                           "R2_sd","RMSE_sd","CVRMSE_sd")
  for (i in 1:n_grid){
    nrounds_i <- gbm_grid$nrounds[i]
    max_depth_i <- gbm_grid$max_depth[i]
    eta_i <- gbm_grid$eta[i]
    subsample_i <- gbm_grid$subsample[i]
    list_res <- parallel::parLapply(cl,
                                    list_train,
                                    gbm_cv_parallel,
                                    as.matrix(input),
                                    output,
                                    nrounds_i,
                                    max_depth_i,
                                    eta_i,
                                    subsample_i)
    tab_cv_res <- do.call("rbind", list_res)
    tab_grid_res$iter[i] <- nrounds_i
    tab_grid_res$depth[i] <- max_depth_i
    tab_grid_res$lr[i] <- eta_i
    tab_grid_res$subsample[i] <- subsample_i
    tab_grid_res$R2[i] <- mean(tab_cv_res$R2)
    tab_grid_res$RMSE[i] <- mean(tab_cv_res$RMSE)
    tab_grid_res$CVRMSE[i] <- mean(tab_cv_res$CVRMSE)
    tab_grid_res$R2_sd[i] <- sd(tab_cv_res$R2)
    tab_grid_res$RMSE_sd[i] <- sd(tab_cv_res$RMSE)
    tab_grid_res$CVRMSE_sd[i] <- sd(tab_cv_res$CVRMSE)
  }
  idx_best_param <- which(tab_grid_res$RMSE == min(tab_grid_res$RMSE))
  best_param <- list(best_iter = tab_grid_res$iter[idx_best_param],
                     best_depth = tab_grid_res$depth[idx_best_param],
                     best_lr = tab_grid_res$lr[idx_best_param],
                     best_subsample = tab_grid_res$subsample[idx_best_param])
  res <- NULL
  res$grid_results <- tab_grid_res
  res$tuned_parameters <- best_param
  return(res)
}


#' @export
gbm_cv_parallel <- function(idx_train,input,output,nrounds,max_depth,eta,subsample){
  tab_res <- as.data.frame(matrix(nr=1,nc=3))
  names(tab_res) <- c("R2","RMSE","CVRMSE")
  train <- input[-idx_train,]
  train_output <- output[-idx_train]
  test <- input[idx_train,]
  test_output <- output[idx_train]
  xgb_fit <- xgboost::xgboost(data = train,
                              label = train_output,
                              max_depth = max_depth,
                              eta = eta,
                              nrounds = nrounds,
                              objective = "reg:linear",
                              alpha=0,
                              colsample_bytree=1,
                              subsample=subsample,
                              verbose = 0,
                              nthread = 1,
                              save_period = NULL)
  yhat <- predict(xgb_fit, test)
  tab_res$R2[1] <- 1-mean((yhat - test_output)^2)/var(test_output)
  tab_res$RMSE[1] <- sqrt(mean((yhat - test_output)^2))
  tab_res$CVRMSE[1] <- 100*sqrt(mean((yhat - test_output)^2))/mean(test_output)
  return(tab_res)
}




#-------------------------------------------------------------------------------
#
#     K-dblocks-CV
#
#-------------------------------------------------------------------------------

#' K-fold-day cross validation function.
#'
#' \code{k_dblocks_cv} splits the data into k folds by randomly selecting blocks of data,
#' where each block correspond to a calendar day.
#'
#'
#' @param Data A dataframe.
#' @param k_folds An integer that corresponds to the number of CV folds.
#'
#' @return A list of row indexes corresponding to the training data.
#'
#' @export

k_dblocks_cv <- function(Data,k_folds){
  dates <- unique(Data$date)
  list_blocks_dates <- caret::createFolds(y = dates,k = k_folds,list = T)
  list_blocks_train <- list()
  for (i in 1:length(list_blocks_dates)){
    train_i <- NULL
    dates_i <- dates[list_blocks_dates[[i]]]
    for (j in 1:length(dates_i)){
      date_j <- dates_i[j]
      train_i <- c(train_i,which(Data$date == date_j))
    }
    list_blocks_train[[i]] <- train_i
  }
  return(list_blocks_train)
}

#-------------------------------------------------------------------------------
#
#     K-wblocks-CV
#
#-------------------------------------------------------------------------------
#' K-fold-week cross validation function.
#'
#' \code{k_wblocks_cv} splits the data into k folds by randomly selecting blocks of data,
#' where each block correspond to a calendar week.
#'
#'
#' @param Data A dataframe.
#' @param k_folds An integer that corresponds to the number of CV folds.
#'
#' @return A list of row indexes corresponding to the training data.
#'
#' @export

k_wblocks_cv <- function(Data,k_folds){
  nweeks <- unique(Data$week)
  list_blocks_weeks <- caret::createFolds(y = nweeks,k = k_folds,list = T)
  list_blocks_train <- list()
  for (i in 1:length(list_blocks_weeks)){
    train_i <- NULL
    weeks_i <- nweeks[list_blocks_weeks[[i]]]
    for (j in 1:length(weeks_i)){
      dweek_j <- weeks_i[j]
      train_i <- c(train_i,which(Data$week == dweek_j))
    }
    list_blocks_train[[i]] <- train_i
  }
  return(list_blocks_train)
}
