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

#12/9
pdf("results.pdf")
var="011"
print(var)  
train_path = paste("/Users/gillianchu/lbnl/pre_12_months/12_T_Gridium_", var, ".csv", sep="")
pred_path = paste("/Users/gillianchu/lbnl/post_12_months/12_P_Gridium_", var, ".csv", sep="")
res1 = gbm_baseline(train_path, pred_path, weight_strat = 1) # Default
results(res_pred = res1, title="Trial")
dev.off()

mask = whichpartrev(res_pred$pred$eload, n=50)
dates_df = head(unique(data.frame(res_pred$pred$date[mask])), 10)
mask3 = which(res_pred$pred$date %in% dates_df[ , ])
mask4 = which(res_pred$pred[mask3 ,]$hour %in% seq(9, 21))

pred_residuals <- res_pred$pred$eload[mask4] - res_pred$prediction[mask4]
pred_R2 <- 100*(1-mean((pred_residuals)^2)/var(res_pred$pred$eload[mask4]))
pred_CVRMSE <- 100*sqrt(mean((pred_residuals)^2))/mean(res_pred$pred$eload[mask4])
pred_NMBE <- 100*mean((pred_residuals))/mean(res_pred$pred$eload[mask4])

#train_path = paste("/Users/gillianchu/lbnl/pre_12_months/12_T_Gridium_", var, ".csv", sep="")
#pred_path = paste("/Users/gillianchu/lbnl/post_12_months/12_P_Gridium_", var, ".csv", sep="")
#building_list = c("011", "017", "022", "023" , "026", "028", "031", "044", "048")

train_path = "/Users/gillianchu/lbnl/pre_12_months"
pred_path = "/Users/gillianchu/lbnl/post_12_months"

building_list = list.files(path = train_path, pattern="*.csv") # lists out all the files in the train_path dir

run_with_weights(building_list)
run_with_weights <- function(building_list) {
  for (var in building_list){
    print(var)
    res1 = gbm_baseline(train_path, pred_path, weight_strat = 1) # Default
    results(res_pred = res1, title = paste("Default: ", var), filename = paste("default_", var, sep=""))
    
    res2 = gbm_baseline(train_path, pred_path, weight_strat = 2) # class reweighting
    results(res_pred = res2, title = paste("Class: ", var), filename = paste("c_", var, sep=""))
    
    res3a = gbm_baseline(train_path, pred_path, weight_strat = 3, weight = 0.05) # temp & energy
    results(res_pred = res3a, title = paste("Temp & Energy 0.05: ", var), filename = paste("t&e05_", var, sep=""))
    res3b = gbm_baseline(train_path, pred_path, weight_strat = 3, weight = 0.35)
    results(res_pred = res3b, title = paste("Temp & Energy 0.35: ", var), filename = paste("t&e35_", var, sep=""))
    res3c = gbm_baseline(train_path, pred_path, weight_strat = 3, weight = 0.5)
    results(res_pred = res3c, title = paste("Temp & Energy 0.5: ", var), filename = paste("t&e50_", var, sep=""))
    
    res4a = gbm_baseline(train_path, pred_path, weight_strat = 4, weight = 0.05) # energy 
    results(res_pred = res4a, title = paste("Energy 0.05: ", var), filename = paste("e05_", var, sep=""))
    res4b = gbm_baseline(train_path, pred_path, weight_strat = 4, weight = 0.35)
    results(res_pred = res4b, title = paste("Energy 0.35: ", var), filename = paste("e35_", var, sep=""))
    res4c = gbm_baseline(train_path, pred_path, weight_strat = 4, weight = 0.5)
    results(res_pred = res4c, title = paste("Energy 0.5: ", var), filename = paste("e50_", var, sep=""))
    
    res5a = gbm_baseline(train_path, pred_path, weight_strat = 5, weight = 3) # weighting
    results(res_pred = res5a, title = paste("Weight 3: ", var), filename = paste("w3_", var, sep=""))
    res5b = gbm_baseline(train_path, pred_path, weight_strat = 5, weight = 5)
    results(res_pred = res5b, title = paste("Weight 5: ", var), filename = paste("w5_", var, sep=""))
    res5c = gbm_baseline(train_path, pred_path, weight_strat = 5, weight = 10)
    results(res_pred = res5c, title = paste("Weight 10: ", var), filename = paste("w10_", var, sep=""))
  }
  
}
whichpartrev <- function(x, n=30) {
  which(x >= -sort(-x, partial=n)[n])
}

results <- function(res_pred, title, filename){
  mask = whichpartrev(res_pred$pred$eload, n=50)
  dates_df = head(unique(data.frame(res_pred$pred$date[mask])), 10)
  mask3 = which(res_pred$pred$date %in% dates_df[ , ])
  mask4 = which(res_pred$pred[mask3 ,]$hour %in% seq(9, 21))
  
  pred_residuals <- res_pred$pred$eload[mask4] - res_pred$prediction[mask4]
  pred_R2 <- 100*(1-mean((pred_residuals)^2)/var(res_pred$pred$eload[mask4]))
  pred_CVRMSE <- 100*sqrt(mean((pred_residuals)^2))/mean(res_pred$pred$eload[mask4])
  pred_NMBE <- 100*mean((pred_residuals))/mean(res_pred$pred$eload[mask4])
  print(pred_R2)
  print(pred_CVRMSE)
  print(pred_NMBE)
  
  print(filename)
  post_gillian(res_pred, mask = mask4, title = title, name = filename)
}

gbm_baseline <- function(train_path = NULL,
                         pred_path = NULL,
                         days_off_path = NULL,
                         train_Data = NULL,
                         pred_Data = NULL,
                         weights = seq(from=0.05,to=0.3,by=0.025),
                         weight_strat = 1,  # default
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
  tune_results <- gbm_tune_gillian(train,
                           k_folds = k_folds,
                           variables = c("Temp","tow", "eload"),
                           ncores = ncores,
                           cv_blocks = cv_blocks,
                           iter = iter,
                           depth = depth,
                           lr = lr,
                           subsample = subsample,
                           weights = weights,
                           weight_strat = weight_strat)

  tuned_parameters <- tune_results$tuned_parameters
  gbm_cv_results <- tune_results$grid_results

  # Final gbm model
  train_output <- train$eload
  train_input <- train[,variables]
  
  weight <- tuned_parameters$best_weight
  
  if (weight_strat == 1) {
    # Default setting
    weightsData <- ifelse(train$eload > 0.0, 1, 1)
  } else if (weight_strat == 2) {
    # Class Reweighting
    mask = whichpartrev(train$eload, n=100)
    max_samples = c(train$eload[mask])
    high_eload_weight = NROW(train$eload) / NROW(max_samples) * 1/2
    weightsData <- ifelse(train$eload %in% max_samples,high_eload_weight, 1)   
  } else if (weight_strat == 3) {
    # Temperature and Energy 
    weightsData <- 1 + (max(train$Temp) - train$Temp + max(train$eload) - train$eload) * weight # bigger temp is heavier
  } else if (weight_strat == 4) {
    # Energy Load
    weightsData <- 1 + (max(train$eload) - train$eload) * weight 
  } else if (weight_strat == 5) {
    # Weighting 
    mask = whichpartrev(train$eload, n=100)
    max_samples = c(train$eload[mask])
    weightsData <- ifelse(train$eload %in% max_samples, weight, 1)
  } else {
    print("Weighting strategy provided is not set, using default")
    weightsData <- ifelse(train$eload > 0.0, 1, 1)
  }
  
  if (verbose){
    cat('"================================="\n')
    cat('"Final Model Training"\n')
    cat('"================================="\n')
  }
  gbm_model <- xgboost::xgboost(data = as.matrix(train_input),
                                label = train_output,
                                weight = weightsData, # new
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
    write.csv(pred, "/Users/gillianchu/lbnl/results/pred.csv", row.names = TRUE) # you can change this to whatever path name
    res$prediction <- y_pred
    write.csv(y_pred, "/Users/gillianchu/lbnl/results/prediction.csv", row.names = TRUE) # likewise, you can change this one
  }
  return(res)
}


# TUNING GILLIAN 
gbm_tune_gillian <- function(Data,
                     k_folds,
                     variables = c("Temp","tow", "eload"),
                     ncores,
                     cv_blocks = "none",
                     iter,
                     depth,
                     lr,
                     subsample,
                     weights,
                     weight_strat){
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
                           subsample = subsample,
                           weights = weights)
  
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
    weights_i <- gbm_grid$weights[i]
    
    list_res <- parallel::parLapply(cl,
                                    list_train,
                                    gbm_cv_parallel_gillian,
                                    as.matrix(input),
                                    output,
                                    nrounds_i,
                                    max_depth_i,
                                    eta_i,
                                    subsample_i,
                                    weights_i, 
                                    weight_strat)
    tab_cv_res <- do.call("rbind", list_res)
    tab_grid_res$iter[i] <- nrounds_i
    tab_grid_res$depth[i] <- max_depth_i
    tab_grid_res$lr[i] <- eta_i
    tab_grid_res$subsample[i] <- subsample_i
    tab_grid_res$weights[i] <- weights_i
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
                     best_subsample = tab_grid_res$subsample[idx_best_param],
                     best_weight = tab_grid_res$weights[idx_best_param])
  res <- NULL
  res$grid_results <- tab_grid_res
  res$tuned_parameters <- best_param
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
gbm_cv_parallel_gillian <- function(idx_train,input,output,nrounds,max_depth,eta,subsample,weights_i,weight_strat){
  #print("Hello")
  #print(idx_train)
  tab_res <- as.data.frame(matrix(nr=1,nc=3))
  names(tab_res) <- c("R2","RMSE","CVRMSE")
  train <- input[-idx_train,]
  train_output <- output[-idx_train]
  test <- input[idx_train,]
  test_output <- output[idx_train]
  
  if (weight_strat == 1) {
    # Default setting
    weightsData <- ifelse(train[,"eload"] > 0.0, 1, 1)
  } else if (weight_strat == 2) {
    # Class Reweighting
    mask = whichpartrev(train[,"eload"], n=100)
    max_samples = c(train[,"eload"][mask])
    high_eload_weight = NROW(train[,"eload"]) / NROW(max_samples) * 1/2
    weightsData <- ifelse(train[,"eload"] %in% max_samples,high_eload_weight, 1)   
  } else if (weight_strat == 3) {
    # Temperature and Energy 
    weightsData <- 1 + (max(train[,"Temp"]) - train[,"Temp"] + max(train[,"eload"]) - train[,"eload"]) * weights_i # bigger temp is heavier
  } else if (weight_strat == 4) {
    # Energy Load
    weightsData <- 1 + (max(train[,"eload"]) - train[,"eload"]) * weights_i 
  } else if (weight_strat == 5) {
    # Weighting 
    mask = whichpartrev(train[,"eload"], n=100)
    max_samples = c(train[,"eload"][mask])
    weightsData <- ifelse(train[,"eload"] %in% max_samples, weights_i, 1)
  } else {
    print("Weighting strategy provided is not set, using default")
    weightsData <- ifelse(train[,"eload"] > 0.0, 1, 1)
  }
  
  xgb_fit <- xgboost::xgboost(data = train,
                              label = train_output,
                              weight = weightsData,
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



#' @export
gbm_cv_parallel <- function(idx_train,input,output,nrounds,max_depth,eta,subsample){
  print("Hello")
  print(idx_train)
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
