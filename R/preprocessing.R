#-------------------------------------------------------------------------------
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
# preprocessing.R
#-------------------------------------------------------------------------------


#' Convert the timestamps into the default format
#'
#' \code{time_format} This function convert the actual timestamps format into "%m/%d/%y %H:%M" format
#'
#'
#' @param data A data frame that contains time column
#' @param format A character string that define the actual format of teh timestamps.
#' Use the description of the base R function \emph{strptime} to define the format.
#' @return A data frame with timestamps converted into "%m/%d/%y %H:%M" format
#'
#' @export


time_format <- function(Data,format){
  if (is.null(format)){
    stop("The original time format is not indicated")
  }
  dts <- as.POSIXct(strptime(Data$time, format = format))
  Data$time <- format(dts,"%m/%d/%y %H:%M")
}




#' Convert 15 minute interval data into hourly interval data
#'
#' \code{convert_15min_to_1_hour} This function convert 15 minute interval data into hourly interval data
#'
#'
#' @param Data A dataframe that contains 15 minutes interval data
#' @param kWh A logical if TRUE it means that the interval data are energy data
#' @return A dataframe with hourly interval data
#'
#' @export

convert_15min_to_1_hour <- function(Data,kWh=TRUE){
  missing_hours <- 0
  dts <- as.POSIXct(strptime(Data$time, format = "%m/%d/%y %H:%M"))
  Data$dts <- dts
  dts_1 <- as.POSIXct(strptime(Data$time, format = "%m/%d/%y %H"))
  Data$days_hours <- dts_1
  days_hours <- unique(Data$days_hours)
  N_days_hours <- length(days_hours)
  Data_hourly <-as.data.frame(matrix(nr=N_days_hours, nc=3))
  names(Data_hourly)<- c("days_hours","eload","Temp")
  Data_hourly$days_hours <- days_hours
  for (k in 1:N_days_hours){
    k_day_hour <- days_hours[k]
    idx_k_day_hour <- which(Data$days_hours == k_day_hour)
    if (length(idx_k_day_hour)==4){
      if (kWh){
       Data_hourly$eload[k] <- sum(Data$eload[idx_k_day_hour])
      }
      else{Data_hourly$eload[k] <- mean(Data$eload[idx_k_day_hour])}
      Data_hourly$Temp[k] <- mean(Data$Temp[idx_k_day_hour])
    }
    else{missing_hours <- missing_hours + 1}
  }
  Data_hourly$days_hours<-format(Data_hourly$days_hours,format="%m/%d/%y %H:%M")
  names(Data_hourly)<-c("time","eload","Temp")
  warning_text <- paste("This data has",missing_hours,"hours with missing values", sep = " ")
  print(warning_text)
  return(Data_hourly)
}


#' Exclude data for given intervals
#'
#' \code{to_exclude} This function exclude data for given intervals
#'
#'
#' @param Data A dataframe
#' @param intervals_to_exclude a path to the file with intervals to exclude
#' @return A dataframe containing the remaining observations
#'
#' @export

to_exclude <- function(Data,intervals_to_exclude){
  intervals <- read.csv(file = intervals_to_exclude, header=T, row.names = NULL, stringsAsFactors = F)
  Data$dts <- as.POSIXct(strptime(Data$time, format = "%m/%d/%y %H:%M"))
  if (dim(intervals)[1]>=1){
   for (i in 1:dim(intervals)[1]){
    start_time <- as.POSIXct(strptime(intervals$start[i], format = "%m/%d/%y %H:%M"))
    end_time <- as.POSIXct(strptime(intervals$end[i], format = "%m/%d/%y %H:%M"))
    if (length(which(Data$dts > start_time & Data$dts < end_time))!=0){
     Data <- Data[-which(Data$dts > start_time & Data$dts < end_time),]
    }
   }
  }
  Data <- dplyr::select(Data,time,eload,Temp)
  return(Data)
}

#' Extract data for given intervals
#'
#' \code{to_extract} This function extract data for given intervals
#'
#'
#' @param Data A dataframe containing all observations
#' @param intervals_to_extract A path to the file with intervals to extract
#' @param start The start date of teh interval to exclude
#' @param end The end date of teh interval to exclude
#' @return a to_extract object, which is a list with the following components:
#' \describe{
#'   \item{Data}{a dataframe containing the remaining observations}
#'   \item{Data_new}{a dataframe containing the extracted observations}
#' }
#'
#' @export

to_extract <- function(Data,
                       intervals_to_extract = NULL,
                       start = NULL,
                       end = NULL){
  if (length(intervals_to_extract) != 0){
    intervals <- read.csv(file = intervals_to_extract,
                          header=T, row.names = NULL,
                          stringsAsFactors = F)
  }
  else if (length(start) != 0 && length(end) != 0) {
    intervals <- as.data.frame(matrix(nr=1,nc=2))
    names(intervals) <-  c("start","end")
    intervals$start <- start
    intervals$end <- end
  }
  else(return())

  Data$dts <- as.POSIXct(strptime(Data$time, format = "%m/%d/%y %H:%M"))
  Data_new <- as.data.frame(matrix(nr=1,nc=4))
  names(Data_new) <- c("time","eload","Temp","dts")
  if (dim(intervals)[1]>=1){
   for (i in 1:dim(intervals)[1]){
    start_time <- as.POSIXct(strptime(intervals$start[i],
                                      format = "%m/%d/%y %H:%M"))
    end_time <- as.POSIXct(strptime(intervals$end[i],
                                    format = "%m/%d/%y %H:%M"))
    if (length(which(Data$dts > start_time & Data$dts < end_time))!=0){
     Data_i <- Data[which(Data$dts >= start_time & Data$dts <= end_time),]
     Data <- Data[-which(Data$dts > start_time & Data$dts < end_time),]
     Data_new <- rbind(Data_new,Data_i)
    }
   }
   Data_new <- Data_new[order(Data_new$dts),]
  }
  Data <- dplyr::select(Data,time,eload,Temp)
  Data_new <- dplyr::select(Data_new,time,eload,Temp)
  results <- list(Data=Data,Data_new=Data_new)
  return(results)
}

#' Create an input variable corresponding to given intervals
#'
#' \code{create_date_var} This function Create a binary input variable corresponding
#' to given intervals if the time steps correspond to a date within the given
#' interval then the value of the input variable will be equal to 1 and if not it
#' will be equal to 0
#'
#'
#' @param Data A dataframe of training or prediction data.
#' @param intervals_path A path to the file with intervals to extract
#' @param start A vector of start date of each interval
#' @param end A vector of end date of each interval note that start vector and
#' end vector should have the same length
#' @param var_name A string character that correspond to the name of the new
#' input variable
#' @return a dataframe with the additional input variable
#'
#' @export

create_date_var <- function(Data,
                            intervals_path = NULL,
                            start = NULL,
                            end = NULL,
                            var_name = "date_off"){
  Data$var_add <- 0
  if (length(intervals_path) != 0){
    intervals <- read.csv(file = intervals_path,
                          header=T, row.names = NULL,
                          stringsAsFactors = F)
  }
  else if (length(start) != 0 && length(end) != 0) {
    intervals <- as.data.frame(matrix(nr=length(start),nc=2))
    names(intervals) <-  c("start","end")
    intervals$start <- start
    intervals$end <- end
  }
  else(return())

  Data$dts <- as.POSIXct(strptime(Data$time, format = "%m/%d/%y %H:%M"))
  if (dim(intervals)[1]>=1){
   for (i in 1:dim(intervals)[1]){
    start_time <- as.POSIXct(strptime(intervals$start[i],
                                      format = "%m/%d/%y %H:%M"))
    end_time <- as.POSIXct(strptime(intervals$end[i],
                                    format = "%m/%d/%y %H:%M"))
    if (length(which(Data$dts > start_time & Data$dts < end_time))!=0){
     Data$var_add[which(Data$dts >= start_time & Data$dts <= end_time)] <- 1
    }
   }
  }
  Data <- dplyr::select(Data,time,eload,Temp,var_add)
  names(Data) <- c("time","eload","Temp",var_name)
  return(Data)
}


# #' Count the number of days
# #'
# #' \code{number_of_days} This function compute the number of days for  which the data are available
# #'
# #'
# #' @param Data A dataframe that contains time as column
# #' @return a numeric corresponding to the number of days
# #'
# #' @export
#
#
# number_of_days <- function(Data){
#   Data$time <- as.POSIXct(strptime(Data$time,"%m/%d/%y %H:%M"))
#   Data$date <- as.Date(Data$time)
#   num <- length(unique(Data$date))
#   return(num)
# }


#' Clean the elaod data
#'
#' \code{clean_eload} This function remove the observations which have negative eload values or with eload
#' values higher or lower than some upper and lower thresholds. The upper threshold is defined as
#' \emph{n} percent higher than the quantile corresponding to the given upper probability \emph{U_ptresh} and the lower
#'  threshold is defined as \emph{n} percent lower than the quantile corresponding to the given lower probability \emph{L_ptresh}.
#'
#'
#' @param data A dataframe of training or prediction data.
#' @param n An integer that correspond to a multiplicative coefficient that is used
#' to define the thresholding value. The default value is 0.2 which correspond to \emph{20} percent.
#' @param L_ptresh A numeric that correspond to the probability of the lower quantile used for filtering
#' @param U_ptresh A numeric that correspond to the probability of the upper quantile used for filtering
#'

#' @return A dataframe that correspond to the cleaned data
#'
#' @export

clean_eload <- function(Data,n = .2, L_ptresh = 0.005, U_ptresh = 0.995){
  # exclude negative values
  Data <- dplyr::filter(Data, eload >= 0)
  # exclude values higher than n*ptresh
  U_tresh <- as.numeric(quantile(Data$eload,probs = c(U_ptresh),na.rm = T))
  Data <- dplyr::filter(Data, eload < (1+n)*U_tresh)
  # exclude the observations when the eload is n times lower than L_tresh
  L_tresh <- as.numeric(quantile(Data$eload,probs = c(L_ptresh),na.rm = T))
  Data <- dplyr::filter(Data, eload > (L_tresh-(L_tresh*n)))
  return(Data)
}

#' Clean the Temperature data
#'
#' \code{clean_Temp} This function remove the observations which have Temperature values higher or lower
#'  than some predefined extreme values.
#'
#'
#' @param Data A dataframe of training or prediction data.
#' @param maxT A numeric that correspond to the temperature above which the corresponding
#' observations will be excluded
#' @param minT A numeric that correspond to the temperature below which the corresponding
#' observations will be excluded

#' @return A dataframe that correspond to the cleaned training or prediction data
#'
#' @export

clean_Temp <- function(Data, maxT = 130, minT= -80){
  Data <- dplyr::filter(Data, Temp <= maxT)
  Data <- dplyr::filter(Data, Temp >= minT)
  return(Data)
}


#' Clean the Temperature data
#'
#' \code{clean_Temp_2} This function remove the period of time where temperature
#' observations do not change.
#'
#'
#' @param data A dataframe of training or prediction data.
#' @param thresh A numeric that correspond to the number of similar successive
#' temperature values
#' @return A dataframe that correspond to the cleaned training or prediction data
#'
#' @export


clean_Temp_2 <- function(Data, thresh = 5){
  count <- rle(Data$Temp)$lengths
  idx <- c(1)
  l <- 1
  for(i in 1:length(count)){
    count_i <- count[i]
    if(count_i == 1){
      idx <- c(idx, idx[l] +1)
      l <- l + 1}
    else if(count_i > 1 & count_i <= thresh) {
      k <- l
      for(j in 1:count_i){
        idx <- c(idx, idx[l] +1)
        l <- l +1}
    }
    else if(count_i > thresh) {
      idx <- c(idx, idx[l] + count_i)
      l <- l + 1}
  }
  Data <- Data[idx,]
  return(Data)
}
