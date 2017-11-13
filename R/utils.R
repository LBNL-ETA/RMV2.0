#-------------------------------------------------------------------------------
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
# utils.R
#-------------------------------------------------------------------------------

###############################################################################
#                         utils functions                                     #
###############################################################################


#' Save a session
#'
#' \code{save_session} This function is used by the shiny application to save the
#'  results of the session
#'
#' @param save_dir the path of the folder where the session will be saved
#' @param project_name the name of the project (given by teh user)
#' @param the shiny reactiveValues object where the results are stored
#'
#' @export

save_session <- function(save_dir,project_name,results){
  isolate({
    withProgress(message = 'Project Saving in progress',
                 detail = 'This may take a while...',
                 value = 0,
      {
        results_list <- reactiveValuesToList(results)
        path_save <- paste(save_dir,.Platform$file.sep,project_name, ".rds",sep="")
        saveRDS(results_list, file = path_save)
    })
  })
}


#' load a session
#'
#' \code{load_session} This function is used by the shiny application to load
#' a saved session
#'
#' @param load_path the path of a saved session
#'
#' @export


load_session <- function(load_path){
  isolate({
    withProgress(message = 'Project Loading in progress',
                 detail = 'This may take a while...',
                 value = 0,
      {
        savedSession <- readRDS(file = load_path)
    })

  })
}

#' Compute number of days in the data
#'
#' \code{number_of_days} This function computes the number of days for which data
#' is available
#'
#' @param Data A data frame that contains time column
#'
#' @export

number_of_days <- function(Data){
  Data$time <- as.POSIXct(strptime(Data$time,"%m/%d/%y %H:%M"))
  Data$date <- as.Date(Data$time)
  num <- length(unique(Data$date))
  return(num)
}

#' Compute the granularity of the data
#'
#' \code{detect_interval} This function computes the granularity of the data
#'
#' @param Data A data frame that contains time column
#' @return A value that correspond to the interval between the observations in minutes
#'
#' @export

detect_interval <- function(Data){
  Data$time <- as.POSIXct(strptime(Data$time,"%m/%d/%y %H:%M"))
  intervals <- diff(Data$time,1)
  intervals <- as.numeric(intervals, units = "mins")
  interval <- as.numeric(names(which.max(table(intervals))))[1]
  return(interval)
}


#' Estimate the ration of missing data
#'
#' \code{ratio_missing} This function estimates the ratio of missing observations
#' by calculating the missing time steps
#'
#' @param Data A data frame that contains time column
#' @return A value that correspond to the missing values ratio
#'
#' @export

ratio_missing <- function(Data,interval){
  Data$time <- as.POSIXct(strptime(Data$time,"%m/%d/%y %H:%M"))
  interval <- paste(interval,"min")
  seq_time <-seq(Data$time[1], Data$time[length(Data$time)], by = interval)
  ratio <- round(100 * (1 - length(Data$time)/length(seq_time)))
  return(ratio)
}


#' Load data into the shiny application
#'
#' \code{data_load} This function is used by the shiny application to load
#' pre or post dataset and store it within a shiny reactiveValues object
#'
#' @param files_path a list that contains all the paths of the considered data files
#' @param files_names a list that contains all the files names
#' @param var_out a shiny reactiveValues object where the data are stored
#' @param Post Boolean that determine if the data is post-installation or
#' post-installation. If true then the data is considered as post-installation.
#' @param clean Boolean that determine if an automatic data cleaning is performed.
#' @return var_out reactiveValues object with a new object where the loaded data are stored
#'
#' @export

data_load <- function(files_path, files_names, var_out, Post = T, clean = T){
  Data <- list()
  summary_tab <- as.data.frame(matrix(nr=length(files_names),nc=9))
  names(summary_tab) <-  c("Name",
                           "Start Date",
                           "End Date",
                           "Numbers of Days",
                           "Percentage of Missing Data",
                           "Time Interval",
                           "elaod_max",
                           "eload_min",
                           "Input Variables")
  withProgress(message = 'Data load in progress',
    value = 0,
    {
    cat('"================================="\n')
    if (Post){cat('"Load Post Data"\n')}
    else{cat('"Load Pre Data"\n')}
    cat('"================================="\n')
    for (i in 1:length(files_path)){
      incProgress(1/length(files_path))
      print(files_path[i])
      Data_i <- read.csv(file = files_path[i],
                         header=T,
                         row.names = NULL,
                         stringsAsFactors = F)
      if (clean){
        Data_i <- clean_Temp(Data_i)
        Data_i <- clean_eload(Data_i)
      }
      file_name <- files_names[i]
      Data[[file_name]] <- Data_i
      summary_tab[i,1] <- files_names[i]
      summary_tab[i,2] <- as.character(as.Date(strptime(Data_i$time[1],
                                                        "%m/%d/%y %H:%M")))
      summary_tab[i,3] <- as.character(as.Date(strptime(Data_i$time[nrow(Data_i)],
                                                        "%m/%d/%y %H:%M")))
      summary_tab[i,4] <- number_of_days(Data_i)
      interval <- detect_interval(Data_i)
      summary_tab[i,5] <- ratio_missing(Data_i,interval)
      summary_tab[i,6] <- interval
      summary_tab[i,7] <- max(Data_i$eload,na.rm = T)
      summary_tab[i,8] <- min(Data_i$eload,na.rm = T)
      summary_tab[i,9] <- dim(Data_i)[2]
      }
  })

  if (Post){
    var_out$Data_post_summary_0 <- summary_tab
    var_out$Data_post <- Data
  }
  else{
    var_out$Data_pre_summary_0 <- summary_tab
    var_out$Data_pre <- Data
  }

  return(var_out)
}

#' Train Baseline models
#'
#' \code{train_model} This function is used by the shiny application to train
#' the baseline models for all the data that are stored in the var_out object
#'
#' @param var_out a shiny reactiveValues object where the taining data are stored
#' @param screen Boolean that determines if the analysis is a screening or a
#' savings analysis. If it's a screening then the baseline model will return only
#' the fitting results of the pre-installation. While if it's savings analysis the
#' baseline model will also return the prediction for the post-installation period
#' @return train_model a list
#'
#' @export


train_model <- function(var_out,
                        screen = T,
                        Model = "TOWT",
                        pam_list = NULL,
                        days_off_path = NULL){
  res_list <- NULL
  failures <- NULL
  Data_pre_summary <- var_out$Data_pre_summary
  files_names <- var_out$files_names
  Data_pre_list <- var_out$Data_pre
  if (!screen){
    Data_post_list <- var_out$Data_post
  }
  withProgress(message = 'Calculation in progress',
    value = 0,
    {
    for (i in 1:length(files_names)){
       name_i <- files_names[i]
       incProgress(1/length(files_names),
                   detail = paste("Training a model for",
                                  Data_pre_summary[i,1]))
       res_baseline <- as.character(Data_pre_summary[i,1])
       if (screen){
         pre_Data_i <- Data_pre_list[[files_names[i]]]
         variables_i <- c(c("Temp","tow"),
                          names(pre_Data_i)[names(pre_Data_i) %nin%
                                            c("time","eload","Temp")])
         switch(Model,
           "TOWT" = try(res_baseline <- towt_baseline(train_Data = pre_Data_i,
                                           pred_Data = pre_Data_i,
                                           timescaleDays = pam_list$timescaleDays,
                                           intervalMinutes = Data_pre_summary[i,6]
                                           ), silent = T),
           "GBM" = try(res_baseline <- gbm_baseline(train_Data = pre_Data_i,
                                                    pred_Data = pre_Data_i,
                                                    days_off_path = days_off_path,
                                                    k_folds = pam_list$k_folds,
                                                    variables = variables_i,
                                                    ncores = pam_list$ncores,
                                                    iter=seq(from=pam_list$iter[1],
                                                             to=pam_list$iter[2],
                                                             by=50),
                                                    depth=seq(from=pam_list$depth[1],
                                                              to=pam_list$depth[2],
                                                              by=1),
                                                    lr = as.numeric(pam_list$lr)
                                                    ), silent = F)

         )

       }
       else{
         pre_Data_i <- Data_pre_list[[files_names[i]]]
         post_Data_i <- Data_post_list[[files_names[i]]]
         variables_i <- c(c("Temp","tow"),
                          names(pre_Data_i)[names(pre_Data_i) %nin%
                                            c("time","eload","Temp")])
         switch(Model,
           "TOWT" = try(res_baseline <- towt_baseline(train_Data = pre_Data_i,
                                           pred_Data = post_Data_i,
                                           timescaleDays = pam_list$timescaleDays,
                                           intervalMinutes = Data_pre_summary[i,6]
                                           ), silent = T),
           "GBM" = try(res_baseline <- gbm_baseline(train_Data = pre_Data_i,
                                                    pred_Data = post_Data_i,
                                                    days_off_path = days_off_path,
                                                    k_folds = pam_list$k_folds,
                                                    variables = variables_i,
                                                    ncores = pam_list$ncores,
                                                    iter=seq(from=pam_list$iter[1],
                                                             to=pam_list$iter[2],
                                                             by=50),
                                                    depth=seq(from=pam_list$depth[1],
                                                              to=pam_list$depth[2],
                                                              by=1),
                                                    lr = as.numeric(pam_list$lr)
                                                    ), silent = F)
         )
       }

       if (is.character(res_baseline)){
         failures <- c(failures,i)
       }
       else{res_list[[name_i]] <- res_baseline}
    }
  })
  return(list(res_list = res_list, failures = failures))
}

#' Baseline models results summary
#'
#' \code{train_model_summary} This function is used by the shiny application
#' to summarise the baseline models results
#'
#' @param model_obj_list a list of the baselne models results for all the data
#' @param files_names a list of names of all the data files for which a baseline models have been built
#' @return A data frame containing the results summary
#'
#' @export

train_model_summary <- function(model_obj_list, files_names){
  summary_tab <- as.data.frame(matrix(nr=length(files_names),nc=4))
  names(summary_tab) <-  c("Name","R2","CVRMSE","NMBE")
  for (i in 1:length(model_obj_list)){
    try({
      summary_tab[i,1] <- files_names[i]
      model_i <- model_obj_list[[i]]
      goodness_of_fit <- model_i$goodness_of_fit
      summary_tab[i,2] <- round(goodness_of_fit$fit_R2,digits = 2)
      summary_tab[i,3] <- round(goodness_of_fit$fit_CVRMSE,digits = 2)
      summary_tab[i,4] <- round(goodness_of_fit$fit_NMBE,digits = 2)
    }, silent =T)
  }
  return(summary_tab)
}

#' Screening summary
#'
#' \code{screen_summary} This function is used by the shiny application
#' to perform a screening of the baseline models results
#'
#' @param summary_tab a dataframe produced by \code{train_model_summary} function
#' @param R2_trsh a numerical value corresponding to the R2 threshold
#' @param CVRMSE_trsh a numerical value corresponding to the CV(RMSE) threshold
#' @param NMBE_trsh a numerical value corresponding to the NMBE threshold
#' @return a screen_summary object, which is a list with the following components:
#' \describe{
#'   \item{win_tab}{a summary_tab dataframe containing only the results for the data files that passed the screening}
#'   \item{los_tab}{a summary_tab dataframe containing only the results for the data files that did not passe the screening}
#'   \item{win_ratio}{a numerical value corresponding to the ratio of data files that passed the screening}
#'   \item{los_ratio}{a numerical value corresponding to the ratio of data files that did not passe the screening}
#' }
#'
#' @export

screen_summary <-  function(summary_tab, R2_trsh, CVRMSE_trsh, NMBE_trsh){
  summary_list <-  NULL
  N <- dim(summary_tab)[1]
  summary_list$win_tab <- dplyr::filter(summary_tab,
                                        R2 >= R2_trsh & CVRMSE <= CVRMSE_trsh & abs(NMBE) <= NMBE_trsh)
  summary_list$los_tab <- dplyr::filter(summary_tab,
                                        R2 < R2_trsh | CVRMSE > CVRMSE_trsh | abs(NMBE) > NMBE_trsh)
  summary_list$win_ratio <- round(100*dim(summary_list$win_tab)[1]/N,
                                 digits = 1)
  summary_list$los_ratio <- round(100*dim(summary_list$los_tab)[1]/N,
                                 digits = 1)
  return(summary_list)

}

#' Extract features from the time
#'
#' \code{number_of_days} This function computes the number of days for which data
#' is available
#'
#' @param Data A data frame that contains time column
#'
#' @return Data data frame that contains new columns corresponding to the extract time features:
#' \describe{
#'   \item{dts}{POSIXct object of the time stamps}
#'   \item{month}{the month of the time stamps}
#'   \item{wday}{the day of the week of the time stamps}
#'   \item{hour}{the hour of the time stamps}
#'   \item{minute}{the minute  of the time stamps}
#'   \item{tod}{the time of the day of the time stamps}
#'   \item{tow}{the time of the week of the time stamps}
#'   \item{date}{the date of the time stamps}
#'   \item{week}{the week of the year of the time stamps}
#'   \item{week_date}{the date of the sunday of the time stamps week}

#' }
#'
#' @export

time_features <- function(data){
  dts <- as.POSIXct(strptime(data$time, format = "%m/%d/%y %H:%M"))
  data$dts <- dts
  data$month <- lubridate::month(data$dts)
  data$wday <- as.POSIXlt(data$dts)$wday
  data$hour <- lubridate::hour(data$dts) +1
  data$minute <- lubridate::minute(data$dts)
  # time of the day
  data$tod <- data$hour + lubridate::minute(data$dts)/60
  # time of the week
  data$tow <- data$hour + lubridate::minute(data$dts)/60 + data$wday*24
  data$date <- as.Date(dts)
  data$week <- lubridate::week(dts)
  data$week_date <- lubridate::floor_date(data$dts, unit="week")
  data <- data[complete.cases(data),]
  return(data)
}


#' Create a new binary variable based on the dates of days off (e.a., holidays).
#'
#' \code{days_off_var} This function create a new binary variable that correspond to the dates of days off,
#' which holidays or days when the building is not occupied.
#'
#'
#' @param days_off_path The path of the file from which the date data of days off (e.g., holidays) are to be read.
#' @param Data A dataframe of training or prediction data.
#'
#' @return A dataframe of training or prediction data including the new varable that correspond
#' to the days off.
#'
#' @export

days_off_var <- function(days_off_path,Data){
  days_off<-read.csv(days_off_path,header =T)
  dts <- as.POSIXct(strptime(days_off$date, format = "%Y/%m/%d"))
  h_dts <- as.Date(dts)
  Data$days_off <- 0
  Data$days_off[Data$date %in% h_dts] <- 1
  return(Data)
}


#' Prediction accuracy metrics computation
#'
#' \code{pred_accuracy} This function compute the following prediction accuracy metrics:  R2, CV(RMSE) and NBME
#'
#'
#' @param towt_baseline_obj A towt_baseline object, which is produced by the towt_baseline function
#' @return A dataframe with where the columns correspond to R2, CV(RMSE) and NMBE
#'
#' @export

pred_accuracy <- function(baseline_obj){
 y_pred <- towt_baseline_obj$prediction
 pred_output <- dplyr::select(towt_baseline_obj$pred,eload)
 pred_residuals <- pred_output$eload - y_pred
 pred_metrics <- as.data.frame(matrix(nr=1,nc=3))
 names(pred_metrics) <- c("pred_R2","pred_CVRMSE","pred_NMBE")
 pred_metrics$pred_R2 <- 100*(1-mean((pred_residuals)^2)/var(pred_output$eload))
 pred_metrics$pred_CVRMSE <- 100*sqrt(mean((pred_residuals)^2))/mean(pred_output$eload)
 pred_metrics$pred_NMBE <- 100*mean((pred_residuals))/mean(pred_output$eload)
 return(pred_metrics)
}
