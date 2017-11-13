#-------------------------------------------------------------------------------
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
# nrevent.R
#-------------------------------------------------------------------------------

#' Function to detect non-routine events
#'
#' \code{cpt_det} This function identify non-routine events in the savings
#' time series it uses cpt.meanvar frunctin from changepoint package to identify
#' changes in mean and variance
#'
#' @param baseline_obj  A baseline object, which is produced by the baseline
#' function (e.g., towt_baseline)
#'
#' @return a cpt_det object, which is a list with the following components:
#' \describe{
#'   \item{savings}{vector corresponding to the estimated savings time series}
#'   \item{nre}{Boolean indicating if non-routine events were identified}
#'   \item{cpts}{changepoints indices}
#'   \item{interval_means}{mean values of detected intervals}
#'   \item{interval_vars}{variance values of detected intervals}
#' }
#' @export

cpt_det <- function(baseline_obj, interval){
  pred <- baseline_obj$pred
  # actual eload during prediction period = post-measure period
  act_post <- pred$eload
  # prediction of eload during prediction period = post period
  pred_post <- baseline_obj$prediction
  # savings
  savings <- pred_post - act_post
  # change points detection
  cpt_res <- changepoint::cpt.meanvar(savings,
                                      method="PELT",
                                      penalty="Manual",
                                      pen.value = "n/10",
                                      minseglen = interval)
  nre <- length(cpt_res@cpts) > 1
  results <-  list(savings = savings,
                   nre = nre,
                   cpts = cpt_res@cpts,
                   interval_means = cpt_res@param.est$mean,
                   interval_vars = cpt_res@param.est$variance)
  return(results)
}


#' Function to identify change points in the whole dataset
#'
#' \code{train_model} This function is used by the shiny application to train
#' the baseline models for all the data that are stored in the var_out object
#'
#' @param var_out a shiny reactiveValues object where the data are stored
#' @return a list
#'
#' @export


nre_eval <- function(var_out){
  res_list <- NULL
  Data_pre_summary <- var_out$Data_pre_summary
  files_names <- var_out$files_names
  model_obj_list <- var_out$model_obj_list
  sav_est_tab_2 <- var_out$sav_est_tab
  sav_est_tab_2$NRE <- 0
  model_list <- model_obj_list$models_list
  withProgress(message = 'Calculation in progress',
    value = 0,
    {
    for(i in 1:length(files_names)){
       name_i <- files_names[i]
       incProgress(1/length(files_names),
                   detail = paste("Training a model for",
                                  Data_pre_summary[i,1]))
       try({
         # 1440 == # of minutes in one day
         interval_i <- 1440 / Data_pre_summary[i,6]
         model_i <- model_list[[i]]
         cpt_det_obj_i <- cpt_det(model_i, interval_i)
         if (cpt_det_obj_i$nre){
           sav_est_tab_2$NRE[i] <- 1
           sav_est_tab_2$cpts[i] <- length(cpt_det_obj_i$cpts)-1
         }
       }, silent =T)
       if (!is.character(cpt_det_obj_i)){
         res_list[[name_i]] <- cpt_det_obj_i
       }
    }
  })
  sav_est_tab_2 <- dplyr::filter(sav_est_tab_2, NRE == 1)

  return(list(cbt_obj_list = res_list, sav_est_tab_2 = sav_est_tab_2))
}
