#-------------------------------------------------------------------------------
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
# savings.R
#-------------------------------------------------------------------------------



#' Function to calculate the savings of the post period.
#'
#' \code{savings} This function compute the estimated savings using the predicted data of the post period.
#'
#'
#' @param baseline_obj  A baseline object, which is produced by the baseline function (e.g., towt_baseline)
#'
#' @return a savings object, which is a list with the following components:
#' \describe{
#'   \item{savings}{numeric corresponding to the estimated savings}
#'   \item{frac_savings}{numeric corresponding to the fractional savings}
#' }
#' @export

savings <- function(baseline_obj){
  pred <- baseline_obj$pred
  # actual eload during prediction period = post-measure period
  act_post <- pred$eload
  # prediction of eload during prediction period = post period
  pred_post <- baseline_obj$prediction
  sum_pred_post <- sum(pred_post, na.rm = TRUE)

  # savings:
  savings <- sum((pred_post - act_post), na.rm = TRUE)
  frac_savings <- (savings/sum_pred_post)

  results <-  list(savings = savings,
                   frac_savings = frac_savings)
  return(results)
}

##  -----  Deprecated  -----
#
# #' Function to estimate the savings uncertainties using ASHRAE 14 definition
# #'
# #' \code{savings_uncert} This function compute the estimated savings uncertainties using ASHRAE 14 definition
# #'
# #'
# #' @param baseline_obj  A baseline object, which is produced by the baseline function (e.g., towt_baseline)
# #' @param savings_obj  savings_obj object produced by the savings function
# #' @param CL  a numeric value that defines the confidence level at which the uncertainty is estimated
# #'
# #' @return a savings_uncert object, which is a list with the following components:
# #' \describe{
# #'   \item{delta_sav}{numeric corresponding to the estimated savings uncertainty}
# #'   \item{fsu}{numeric corresponding to the estimated fractional savings uncertainty}
# #'   \item{rho}{the estimated autocorrelation}
# #' }
# #' @export
#
# savings_uncert <- function(baseline_obj,savings_obj,CL){
#   train <- baseline_obj$train
#   pred <- baseline_obj$pred
#   # actual eload during training (baseline) period = pre-measure period
#   act_pre <- train$eload
#   # fitting
#   y_fit <- baseline_obj$fitting
#   # actual eload during prediction period = post-measure period
#   act_post <- pred$eload
#   # prediction of eload during prediction period = post period
#   pred_post <- baseline_obj$prediction
#
#   mean_act_pre <- mean(act_pre,na.rm=T)
#   mean_pred_post <- mean(pred_post,na.rm=T)
#   frac_savings <- savings_obj$frac_savings
#
#   # savings_uncertainty
#   goodness_of_fit <- baseline_obj$goodness_of_fit
#   rmse <- goodness_of_fit$fit_RMSE
#   m <- length(act_post)
#   n <- length(act_pre)
#   rho <- as.numeric(acf(act_pre-y_fit,lag.max=1,plot=F)$acf)[2] # autocorelation
#   n2 <- ((1-rho)/(1+rho))*n
#   t_alpha <- qnorm((1-CL/100)/2 + CL/100)
#   delta_sav <- 1.26*t_alpha*(mean_pred_post/mean_act_pre)*rmse*sqrt(n/n2*(1+2/n2)*m)
#   fsu <- 1.26*t_alpha*(rmse/(mean_act_pre*frac_savings))*sqrt(n/n2*(1+2/n2)*1/m)
#   results <-  list(delta_sav = delta_sav,
#                    fsu = fsu,
#                    rho = rho)
#   return(results)
# }



##  -----  Deprecated  -----
#
# #' @export
#
# savings_UCalc <- function(Data_pre = NULL,
#                           frac_savings = NULL,
#                           cv_rmse = NULL,
#                           CL = NULL,
#                           m = NULL){
#   # actual eload during training (baseline) period = pre-measure period
#   act_pre <- Data_pre$eload
#   mean_act_pre <- mean(act_pre,na.rm=T)
#   cv_rmse <- cv_rmse/100
#   frac_savings <- frac_savings/100
#   # savings_uncertainty
#   m <- m
#   n <- length(act_pre)
#   # autocorelation
#   rho <- as.numeric(acf(act_pre,lag.max=1,plot=F)$acf)[2]
#   n2 <- ((1-rho)/(1+rho))*n
#   t_alpha <- qnorm((1-CL/100)/2 + CL/100)
#   fsu <- 1.26*t_alpha*(cv_rmse/(frac_savings))*sqrt(n/n2*(1+2/n2)*1/m)
#   return(fsu)
# }



#' Savings summary
#'
#' \code{savings_summary} This function is used by the shiny application
#' to produce the savings and the uncertainties summary table
#'
#' @param sav_out a shiny reactiveValues object the baseline object and the pre/post data are stored
##' @param inCL a numerical value corresponding to the user specified confidence level
#' @return a dataframe of the savings summary
#'
#' @export


savings_summary <- function(sav_out){
  model_obj_list <- sav_out$model_obj_list
  model_list <- model_obj_list$models_list
  files_names <- sav_out$files_names_mod
  res_tab <- as.data.frame(matrix(nr=length(files_names),nc=6))
  names(res_tab) <-  c("Name","Savings","FS","R2","CVRMSE","NMBE")
  for (i in 1:length(files_names)){
    res_tab[i,1] <- files_names[i]
    model_i <- model_list[[i]]
    sav_res_i <- savings(model_i)
    res_tab[i,2] <- round(sav_res_i$savings)
    res_tab[i,3] <- round(100*sav_res_i$frac_savings,digits = 1)
    goodness_of_fit_i <- model_i$goodness_of_fit
    res_tab[i,4] <- round(goodness_of_fit_i$fit_R2,digits = 2)
    res_tab[i,5] <- round(goodness_of_fit_i$fit_CVRMSE,digits = 2)
    res_tab[i,6] <- round(goodness_of_fit_i$fit_NMBE,digits = 2)
  }
  return(res_tab)
}

##  -----  Deprecated  -----
## exclude the uncertainty calculation
#
# savings_summary <- function(sav_out, inCL){
#   model_obj_list <- sav_out$model_obj_list
#   model_list <- model_obj_list$models_list
#   files_names <- sav_out$files_names_mod
#   res_tab <- as.data.frame(matrix(nr=length(files_names),nc=10))
#   names(res_tab) <-  c("Name","Savings","FS","Delta_Uncertainty",
#                        "FSU","Savings_Range" ,"FS_Range",
#                        "R2","CVRMSE","NMBE")
#   for (i in 1:length(files_names)){
#     res_tab[i,1] <- files_names[i]
#     model_i <- model_list[[i]]
#     sav_res_i <- savings(model_i)
#     sav_uq_res_i <- savings_uncert(model_i,sav_res_i,inCL)
#     res_tab[i,2] <- round(sav_res_i$savings)
#     res_tab[i,3] <- round(100*sav_res_i$frac_savings,digits = 1)
#     res_tab[i,4] <- round(sav_uq_res_i$delta_sav)
#     res_tab[i,5] <- round(100*sav_uq_res_i$fsu)
#     res_tab[i,6] <- paste0("[",round(sav_res_i$savings - sav_uq_res_i$delta_sav),
#                            "; ",res_tab[i,2], "; ",
#                            round(sav_res_i$savings + sav_uq_res_i$delta_sav), "]")
#     res_tab[i,7] <- paste0("[",round(100*(sav_res_i$frac_savings - sav_res_i$frac_savings*sav_uq_res_i$fsu),digits = 1),
#                            "; ",res_tab[i,3], "; ",
#                            round(100*(sav_res_i$frac_savings + sav_res_i$frac_savings*sav_uq_res_i$fsu),digits = 1), "]")
#     goodness_of_fit_i <- model_i$goodness_of_fit
#     res_tab[i,8] <- round(goodness_of_fit_i$fit_R2,digits = 2)
#     res_tab[i,9] <- round(goodness_of_fit_i$fit_CVRMSE,digits = 2)
#     res_tab[i,10] <- round(goodness_of_fit_i$fit_NMBE,digits = 2)
#   }
#   return(res_tab)
# }


#' Savings summary
#'
#' \code{savings_summary} This function is used by the shiny application
#' to produce results summary at the portfolio level
#'
#' @param sav_out a shiny reactiveValues object where the baseline object and the pre/post data are stored
#' @param screened a Boolean, which defines if the all the data are included in the portfolio analysis (screened = FALSE) or if only the data that have passed the screeneing are used (screened = TRUE)
#' @return a portfolio_savings object which is a list  with the following components:
#' \describe{
#'   \item{Savings_portfolio}{numerical value corresponding to the estimated savings at portfolio level}
#'   \item{Delta_portfolio}{numerical value corresponding to the estimated savings uncertainty at portfolio level}
#'   \item{Savings_Range}{a text string corresponding to the estimated savings range at the portfolio level}
#'   \item{FS_portfolio}{numerical value corresponding to the estimated fractional savings at portfolio level}
#'   \item{FSU_portfolio}{numerical value corresponding to the estimated fractional savings uncertainty at portfolio level}
#'   \item{FS_Range}{a text string corresponding to the estimated fractional savings range at the portfolio level}
#' }
#'
#' @export

portfolio_savings <- function(sav_out, screened = F){
  model_obj_list <- sav_out$model_obj_list
  model_list <- model_obj_list$models_list
  if (screened == F){
    files_names <- sav_out$files_names_mod
    sav_est_tab <- sav_out$sav_est_tab
  }
  else{
    screen_summary_list <- sav_out$screen_summary_list
    win_tab <- screen_summary_list$win_tab
    files_names <- win_tab$Name
    sav_est_tab <- sav_out$sav_est_tab
    sav_est_tab <- dplyr::filter(sav_est_tab, Name %in% files_names)
  }
  res_tab <- as.data.frame(matrix(nr=length(files_names),nc=2))
  for (i in 1:length(files_names)){
    name_i <- files_names[i]
    model_i <- model_list[[name_i]]
    pred <- model_i$pred
    # actual eload during prediction period = post-measure period
    act_post <- pred$eload
    # prediction of eload during prediction period = post period
    pred_post <- model_i$prediction
    sum_pred_post <- sum(pred_post, na.rm = T)
    # savings:
    savings <- sum((pred_post - act_post), na.rm = T)
    res_tab[i,1] <- savings
    res_tab[i,2] <- sum_pred_post
  }
  Savings_portfolio <- round(sum(res_tab[,1]),digit=1)
  FS_portfolio <- round(sum(res_tab[,1])/sum(res_tab[,2]),digit=2)
  results <-  list(Savings_portfolio = Savings_portfolio,
                   FS_portfolio = FS_portfolio)
  return(results)
}

##  -----  Deprecated  -----
## exclude the uncertainty calculation
#
# portfolio_savings <- function(sav_out, screened = F){
#   model_obj_list <- sav_out$model_obj_list
#   model_list <- model_obj_list$models_list
#   if (screened == F){
#     files_names <- sav_out$files_names_mod
#     sav_est_tab <- sav_out$sav_est_tab
#   }
#   else{
#     screen_summary_list <- sav_out$screen_summary_list
#     win_tab <- screen_summary_list$win_tab
#     files_names <- win_tab$Name
#     sav_est_tab <- sav_out$sav_est_tab
#     sav_est_tab <- dplyr::filter(sav_est_tab, Name %in% files_names)
#   }
#   res_tab <- as.data.frame(matrix(nr=length(files_names),nc=2))
#   for (i in 1:length(files_names)){
#     name_i <- files_names[i]
#     model_i <- model_list[[name_i]]
#     pred <- model_i$pred
#     # actual eload during prediction period = post-measure period
#     act_post <- pred$eload
#     # prediction of eload during prediction period = post period
#     pred_post <- model_i$prediction
#     sum_pred_post <- sum(pred_post, na.rm = T)
#     # savings:
#     savings <- sum((pred_post - act_post), na.rm = T)
#     res_tab[i,1] <- savings
#     res_tab[i,2] <- sum_pred_post
#   }
#   Savings_portfolio <- round(sum(res_tab[,1]),digit=1)
#   Delta_portfolio <- round(sqrt(sum((sav_est_tab$Delta_Uncertainty)^2)),digit=2)
#   Savings_Range <- paste0("[",round((Savings_portfolio - Delta_portfolio), digits = 1),
#                              "; ",Savings_portfolio, "; ",
#                              round((Savings_portfolio - Delta_portfolio), digits = 1), "]")
#   FS_portfolio <- round(sum(res_tab[,1])/sum(res_tab[,2]),digit=2)
#   FSU_portfolio <- round(Delta_portfolio/sum(res_tab[,1]),digit=2)
#   FS_Range <- paste0("[",round(100* (FS_portfolio - FS_portfolio * FSU_portfolio), digits = 1),
#                          "; ",100 * FS_portfolio, "; ",
#                          round(100 * (FS_portfolio + FS_portfolio * FSU_portfolio), digits = 1), "]")
#   results <-  list(Savings_portfolio = Savings_portfolio,
#                    Delta_portfolio = Delta_portfolio,
#                    Savings_Range = Savings_Range,
#                    FS_portfolio = FS_portfolio,
#                    FSU_portfolio = 100 * FSU_portfolio,
#                    FS_Range = FS_Range)
#   return(results)
# }



##  -----  Deprecated  -----
## exclude the uncertainty calculation
#
# #' Potential fractional savings
# #'
# #' \code{savings_summary} This function is used by the shiny application
# #' to produce an estimation of potential fractional savings uncertainties when the screening analysis is performed
# #'
# #' @param screen_out a shiny reactiveValues object where the baseline object and the pre data are stored
# #' @param d_post a numerical value corresponding to the number of required days in the post period
# #' @param frac_savings a numerical value corresponding to the user defined fractional savings
# #' @return a dataframe of the estimated fractional savings uncertainties at three different confidence levels
# #'
# #' @export
#
# fsu_estimation_sc <- function(screen_out, d_post, frac_savings){
#   model_obj_list <- screen_out$model_obj_list
#   model_list <- model_obj_list$models_list
#   Data_pre_summary <- screen_out$Data_pre_summary
#   files_names <- screen_out$files_names_mod
#   Data_pre_list <- screen_out$Data_pre
#   res_tab <- as.data.frame(matrix(nr=length(files_names),nc=4))
#   names(res_tab) <-  c("Name","FSU @ 68 CL","FSU @ 80 CL","FSU @ 95 CL")
#   for (i in 1:length(files_names)){
#     res_tab[i,1] <- files_names[i]
#     model_i <- model_list[[i]]
#     goodness_of_fit_i <- model_i$goodness_of_fit
#     cvrmse_i <- goodness_of_fit_i$fit_CVRMSE
#     time_steps_i <- round(60/Data_pre_summary[i,6]) * d_post
#     pre_Data_i <- Data_pre_list[[files_names[i]]]
#     res_tab[i,2] <- round(100 * savings_UCalc(Data_pre = pre_Data_i,
#                                               frac_savings = frac_savings,
#                                               cv_rmse = cvrmse_i,
#                                               CL = 68,
#                                               m = time_steps_i),digits = 2)
#      res_tab[i,3] <- round(100 * savings_UCalc(Data_pre = pre_Data_i,
#                                                frac_savings = frac_savings,
#                                                cv_rmse = cvrmse_i,
#                                                CL = 80,
#                                                m = time_steps_i),digits = 2)
#      res_tab[i,4] <- round(100 * savings_UCalc(Data_pre = pre_Data_i,
#                                                frac_savings = frac_savings,
#                                                cv_rmse = cvrmse_i,
#                                                CL = 95,
#                                                m = time_steps_i),digits = 2)
#   }
#   return(res_tab)
#
# }
