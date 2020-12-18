#-------------------------------------------------------------------------------
# RMV2.0 (version 1.1.0)
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
#-------------------------------------------------------------------------------


###############################################################################
#                         Plot Functions                                      #
###############################################################################

#' Plot of the actual pre or post installation data
#'
#' \code{act_plot} a function that generates a dygraph plot of the actual observations (eload and Temp)
#'
#'
#' @param Data A data frame of the actual observations
#' @param low_range_0 a numerical value of the lower bound of the eload axis
#' @param high_range_0 a numerical value of the upper bound of the eload axis
#' @param low_range_T_0 a numerical value of the lower bound of the Temp axis
#' @param high_range_T_0 a numerical value of the upper bound of the Temp axis
#'
#' @return A dygraph time series plot object
#'
#' @export

act_plot <- function(Data,title = NULL,
                     low_range_0 = NULL,
                     high_range_0 = NULL,
                     low_range_T_0 = NULL,
                     high_range_T_0 = NULL){
  dts <- as.POSIXct(strptime(Data$time, format = "%m/%d/%y %H:%M"))
  Data$dts <- dts
  if (length(which(duplicated(Data$dts)==T)!=0)){
    Data <- Data[-which(duplicated(Data$dts)==T),]
  }
  if (length(which(is.na(Data$dts)))!=0){
    Data <- Data[-which(is.na(Data$dts)),]
  }
  eload_xts<-xts::xts(Data$eload, order.by = Data$dts )
  names(eload_xts) <- c("eload")
  Temp_xts<-xts::xts(Data$Temp, order.by = Data$dts )
  names(Temp_xts) <- c("Temperature")
  data_xts <- cbind(eload_xts, Temp_xts)

  if (!is.null(low_range_0) && !is.null(high_range_0) &&
      !is.null(low_range_T_0) && !is.null(high_range_T_0)){
      low_range <- low_range_0
      low_range_T <- low_range_T_0
      high_range <- high_range_0
      high_range_T <- high_range_T_0
  }

  else{
    # define axis range if they are not specified
    low_range <- min(abs(Data$eload), na.rm = T)
    high_range <- as.numeric(quantile(Data$eload,probs = 0.975,na.rm = T)) * 1.2
    high_range <- low_range + 2 * (high_range-low_range)
    high_T <- max(Data$Temp, na.rm = T) *1.1
    high_range_T <- high_T * 1.15
    low_T <- as.numeric(quantile(Data$Temp,probs = 0.025,na.rm = T)) - 5
    low_range_T <- high_range_T - 2 * abs(high_range_T-low_T)
  }

  p <- dygraphs::dygraph(data_xts, main = title) %>% #
       dygraphs::dySeries("eload", label = "Actual", color = "#06AED5") %>%
       dygraphs::dyAxis("y", label = "eload",
                        valueRange = c(low_range, high_range)) %>%
       dygraphs::dyAxis("y2", label = "Temperature",
                        valueRange = c(low_range_T, high_range_T))%>%
       dygraphs::dySeries("Temperature", axis = 'y2',
                          label = "Temperature", color = "#FE4A49")  %>%
       dygraphs::dyLegend(width = 350)

  return(p)
}

#' Compute plot axis range for two different data files (e.g, pre and post)
#'
#' \code{axis_range} Compute plot axis range for two different data files (e.g, pre and post)
#'
#'
#' @param Data_1 and Data_2 dataframes of two different data files

#' @return a list object with the following components:
#' \describe{
#'   \item{low_range_0}{a numerical value of the lower bound of the eload axis}
#'   \item{high_range_0}{a numerical value of the upper bound of the eload axis}
#'   \item{low_range_T_0}{a numerical value of the lower bound of the Temp axis}
#'   \item{high_range_T_0}{a numerical value of the upper bound of the Temp axis}
#' }
#'
#'
#' @export

axis_range <- function(Data_1, Data_2){

  low_range_1 <- min(abs(Data_1$eload), na.rm = T)
  high_range_1 <- as.numeric(quantile(Data_1$eload,probs = 0.975,na.rm = T)) * 1.2
  high_range_1 <- low_range_1 + 2 * (high_range_1-low_range_1)
  high_T_1 <- max(Data_1$Temp, na.rm = T) *1.1
  high_range_T_1 <- high_T_1 * 1.15
  low_T_1 <- as.numeric(quantile(Data_1$Temp,probs = 0.025,na.rm = T)) - 5
  low_range_T_1 <- high_range_T_1 - 2 * abs(high_range_T_1-low_T_1)

  low_range_2 <- min(abs(Data_2$eload), na.rm = T)
  high_range_2 <- as.numeric(quantile(Data_2$eload,probs = 0.975,na.rm = T)) * 1.2
  high_range_2 <- low_range_2 + 2 * (high_range_2-low_range_2)
  high_T_2 <- max(Data_2$Temp, na.rm = T) *1.1
  high_range_T_2 <- high_T_2 * 1.15
  low_T_2 <- as.numeric(quantile(Data_2$Temp,probs = 0.025,na.rm = T)) - 5
  low_range_T_2 <- high_range_T_2 - 2 * abs(high_range_T_2-low_T_2)

  low_range <- min(low_range_1, low_range_2)
  low_range_T <- min(low_range_T_1, low_range_T_2)
  high_range <- max(high_range_1, high_range_2)
  high_range_T <- max(high_range_T_1, high_range_T_2)

  return(list(low_range = low_range,
              low_range_T = low_range_T,
              high_range = high_range,
              high_range_T = high_range_T)
         )

}

#' eload vs. Temperature scatter plot
#'
#' \code{eload_vs_Temp_plot} A function that creates a scatterplot of eload vs. Temperature
#'
#'
#' @param Data a dataframes with eload and Temp columns

#' @return A ggplot object
#'
#' @export

eload_vs_input_plot <- function(Data,input = "Temp"){
  Data <- clean_Temp(Data)
  p <- ggplot2::ggplot(data = Data,
                       ggplot2::aes_string(x = input, y = "eload")) +
       ggplot2::geom_point(colour="#2096BA",size=1) +
       ggplot2::theme_bw()+
       ggplot2::theme(panel.border = ggplot2::element_blank(),
                      axis.line = ggplot2::element_blank()) +
       ggplot2::theme(text = ggplot2::element_text(size=16),
                      axis.text.x = ggplot2::element_text(size=15),
                      axis.text.y = ggplot2::element_text(size=15),
                      legend.text = ggplot2::element_text(size=15))
  return(p)
}

#' actual vs. fitted scatter plot
#'
#' \code{act_vs_fit_plot} A function that creates a scatterplot of the actual vs. fitted data produced by the baseline model
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline model function

#' @return A ggplot object
#'
#' @export
act_vs_fit_plot <- function(baseline_obj){
  act <- dplyr::select(baseline_obj$train,eload)
  names(act) <- c("Actual")
  fit <- as.data.frame(baseline_obj$fitting)
  names(fit) <- c("Fit")
  Data <- cbind(act,fit)
  p <- ggplot2::ggplot(data = Data, ggplot2::aes(x = Actual, y = Fit)) +
       ggplot2::geom_point(colour="#2096BA",size=1) +
       ggplot2::geom_abline(size = 2) +
       ggplot2::theme_bw()+
       ggplot2::theme(panel.border = ggplot2::element_blank(),
                      axis.line = ggplot2::element_blank()) +
       ggplot2::theme(text = ggplot2::element_text(size=16),
                      axis.text.x = ggplot2::element_text(size=15),
                      axis.text.y = ggplot2::element_text(size=15),
                      legend.text = ggplot2::element_text(size=15))
  return(p)
}

#' errors vs. Temperature scatter plot
#'
#' \code{errors_vs_Temp_plot} A function that creates a scatterplot of the baseline model errors vs. Temperature  data produced by the baseline model
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline model function

#' @return A ggplot object
#'
#' @export

errors_vs_input_plot <- function(baseline_obj,input = "Temp"){
  act <- dplyr::select(baseline_obj$train,eload,input)
  names(act) <- c("Actual",input)
  fit <- as.data.frame(baseline_obj$fitting)
  names(fit) <- c("Fit")
  Data <- cbind(act,fit)
  Data$errors <- act$Actual - fit$Fit
  Data <- dplyr::select(Data,input,errors)
  p <- ggplot2::ggplot(data = Data,
                       ggplot2::aes_string(x = input, y = "errors")) +
       ggplot2::geom_point(colour="#2096BA",size=1) +
       ggplot2::theme_bw()+
       ggplot2::theme(panel.border = ggplot2::element_blank(),
                      axis.line = ggplot2::element_blank()) +
       ggplot2::theme(text = ggplot2::element_text(size=16),
                      axis.text.x = ggplot2::element_text(size=15),
                      axis.text.y = ggplot2::element_text(size=15),
                      legend.text = ggplot2::element_text(size=15))
  return(p)
}

#' Residual autocorrelation plot
#'
#' \code{acf_plot} A function that creates an autocorrelation plot of the baseline model residual
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline model function
#' @param lag_max The maximum lag at which to calculate the acf (autocorrelation function)
#' @return A ggplot object
#'
#' @export
#' @import forecast

acf_plot <- function(baseline_obj,lag_max = 12){
  act <- dplyr::select(baseline_obj$train,eload)
  names(act) <- c("Actual")
  fit <- as.data.frame(baseline_obj$fitting)
  names(fit) <- c("Fit")
  residual <- act$Actual - fit$Fit
  p <- forecast::ggAcf(x = residual, lag.max = lag_max) +
       ggplot2::theme_bw()+
       ggplot2::labs(title = NULL)+
       ggplot2::theme(panel.border = ggplot2::element_blank(),
                      axis.line = ggplot2::element_blank()) +
       ggplot2::theme(text = ggplot2::element_text(size=16),
                      axis.text.x = ggplot2::element_text(size=15),
                      axis.text.y = ggplot2::element_text(size=15),
                      legend.text = ggplot2::element_text(size=15))
  return(p)
}

#' Savings barplot
#'
#' \code{savings_results_plot} A function that creates a barplot of savings estimation and their uncertainties
#'
#'
#' @param savings_res_tab  a data frame produced by the savings_results function
#' @return A plot_ly object
#'
#' @export

savings_results_plot <-  function(savings_res_tab){
  savings_res_tab <- dplyr::arrange(savings_res_tab, FS)
  savings_res_tab$Name <- factor(savings_res_tab$Name,
                                 levels =unique(savings_res_tab$Name))
  savings_res_tab <- as.data.frame(savings_res_tab)
  p <- plotly::plot_ly(savings_res_tab, x = ~Name, y = ~FS,
                       type = 'bar',
                       marker = list(color= "#06AED5",
                                     line = list(color = "#086788",
                                     width = 1.5))
                       ) %>%
       plotly::layout(xaxis = list(showticklabels = F)) %>%
       plotly::layout(autosize = T) %>%
       plotly::config(displayModeBar = F) %>%
       plotly::layout(showlegend = F)
  return(p)
}
##  -----  Deprecated  -----
## exclude the uncertainty calculation
# savings_results_plot <-  function(savings_res_tab){
#   savings_res_tab <- dplyr::arrange(savings_res_tab, FS)
#   savings_res_tab$Name <- factor(savings_res_tab$Name,
#                                  levels =unique(savings_res_tab$Name))
#   savings_res_tab$savings_err <- savings_res_tab$FS*savings_res_tab$FSU/100
#   savings_res_tab <- as.data.frame(savings_res_tab)
#   p <- plotly::plot_ly(savings_res_tab, x = ~Name, y = ~FS,
#                        type = 'bar',
#                        marker = list(color= "#06AED5",
#                                      line = list(color = "#086788",
#                                      width = 1.5)),
#                        error_y = list(type = "data",
#                        array = savings_res_tab$savings_err, color = '#DD1C1A')
#                        ) %>%
#        plotly::layout(xaxis = list(showticklabels = F)) %>%
#        plotly::layout(autosize = T) %>%
#        plotly::config(displayModeBar = F) %>%
#        plotly::layout(showlegend = F)
#   return(p)
# }


#' Screening pie plot
#'
#' \code{screen_pie_plot} A function that creates a pie plot the screening results
#'
#'
#' @param screen_summary_list  a list object produced by screen_summary function
#' @return A plot_ly object
#'
#' @export

screen_pie_plot <- function(screen_summary_list){
  data <- data.frame(status=c("pass","fail"),
                     percentage=c(screen_summary_list$win_ratio,
                                  screen_summary_list$los_ratio))
  margin <- list(l = 5, r = 5, b = 0, t = 0)
  p <- plotly::plot_ly(data,
                       labels = ~status,
                       values = ~percentage,
                       type = 'pie',
                       textposition = 'inside',
                       textinfo = 'label+percent',
                       insidetextfont = list(size = 16, color = '#FFFFFF'),
                       hoverinfo = 'text',
                       #width = 300,
                       #height = 300,
                       marker = list(colors = c("#06AED5","#DD1C1A"),
                                     line = list(color = '#FFFFFF', width = 1))
                                     ) %>%
       plotly::layout(xaxis = list(showgrid = F,
                                   zeroline = F,
                                   showticklabels = F),
                      yaxis = list(showgrid = F,
                                   zeroline = F,
                                   showticklabels = F),
                      showlegend = F) %>%
       plotly::layout(autosize = T,  margin = margin)  %>%
       plotly::config(displayModeBar = F)
  return(p)
}


#' Eload heatmap
#'
#' \code{eload_heatmap} A function that creates a heatmap plot of eload where xaxis corresponds to the time of the week and yaxis corresponds to the weeks
#'
#'
#' @param Data  a data frame to plot
#' @return A plot_ly object
#'
#' @export

eload_heatmap <- function(Data,title = NULL,
                          colors = colorRamps::matlab.like(100),
                          zauto = T,
                          zmin = 0,
                          zmax = 0
                          ){
  Interval <- detect_interval(Data)
  Interval_2 <- 60 / Interval * 24
  Data <- time_features(Data)
  Data <- dplyr::select(Data, tow, week_date, eload)
  Data <- reshape2::melt(Data, id=c("week_date","tow"),na.rm=F)
  Data <- Data[!duplicated(Data[,c(1,2)]), ]
  Data <- reshape2::acast(Data, week_date ~ tow ~ variable)
  Data <- Data[,,1]
  y_Data <- row.names(Data)
  x_Data <- c(1:(7*Interval_2))
  Data <- as.matrix(Data)
  f1 <- list(family = "Arial, sans-serif",
             size = 12)
  ax <- list(zeroline = F, showline = T,
             mirror = "ticks", gridcolor = '#FFFFFF',
             gridwidth = 2, linecolor = "#242423",
             linewidth = 6, autotick = F,
             ticks = "outside", tick0 = 0,
             dtick = Interval_2,
             title = ' <br> Time Of the Week',
             titlefont = f1
             )
  ay <- list(zeroline = F, showline = T,
             mirror = "ticks", gridcolor = '#FFFFFF',
             gridwidth = 2, linecolor = "#242423",
             linewidth = 6,
             title = "Starting Date of the Week",
             titlefont = f1
             )
  margin <- list(l = 100, r = 50, b = 30, t = 30, pad = 1)
  p <- plotly::plot_ly(x = x_Data, y = y_Data, z = Data,
                       colors = colors, type = "heatmap",
                       #width = 1200, #height = 500,
                       xgap = 1, ygap = 1,
                       zauto = zauto, zmin = zmin, zmax = zmax
                       ) %>%
       plotly::layout(xaxis = ax , yaxis = ay)  %>%
       plotly::layout(autosize = T,  margin = margin)  %>%
       plotly::config(displayModeBar = F)
  return(p)
}


#' Savings heatmap
#'
#' \code{savings_heatmap} A function that creates a heatmap plot of savings where xaxis corresponds to the time of the week and yaxis corresponds to the weeks
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline function
#' @return A plot_ly object
#'
#' @export

savings_heatmap <- function(baseline_obj,title = NULL,
                          colors = colorRamps::matlab.like(100),
                          zauto = T,
                          zmin = 0,
                          zmax = 0
                          ){
  Data <- baseline_obj$pred
  # actual eload during prediction period = post-measure period
  act_post <- Data$eload
  # prediction of eload during prediction period = post period
  pred_post <- baseline_obj$prediction
  # savings:
  Data$savings <- (pred_post - act_post)
  Interval <- detect_interval(Data)
  Interval_2 <- 60 / Interval * 24
  Data <- time_features(Data)
  Data <- dplyr::select(Data, tow, week_date, savings)
  Data <- reshape2::melt(Data, id=c("week_date","tow"),na.rm=F)
  Data <- Data[!duplicated(Data[,c(1,2)]), ]
  Data <- reshape2::acast(Data, week_date ~ tow ~ variable)
  Data <- Data[,,1]
  y_Data <- row.names(Data)
  x_Data <- c(1:(7*Interval_2))
  Data <- as.matrix(Data)
  f1 <- list(family = "Arial, sans-serif",
             size = 12)
  ax <- list(zeroline = F, showline = T,
             mirror = "ticks", gridcolor = '#FFFFFF',
             gridwidth = 2, linecolor = "#242423",
             linewidth = 6, autotick = F,
             ticks = "outside", tick0 = 0,
             dtick = Interval_2,
             title = ' <br> Time Of the Week',
             titlefont = f1
             )
  ay <- list(zeroline = F, showline = T,
             mirror = "ticks", gridcolor = '#FFFFFF',
             gridwidth = 2, linecolor = "#242423",
             linewidth = 6,
             title = "Starting Date of the Week",
             titlefont = f1
             )
  margin <- list(l = 100, r = 50, b = 30, t = 30, pad = 1)
  p <- plotly::plot_ly(x = x_Data, y = y_Data, z = Data,
                       colors = colors, type = "heatmap",
                       #width = 1200, #height = 500,
                       xgap = 1, ygap = 1,
                       zauto = zauto, zmin = zmin, zmax = zmax
                       ) %>%
       plotly::layout(xaxis = ax , yaxis = ay)  %>%
       plotly::layout(autosize = T,  margin = margin)  %>%
       plotly::config(displayModeBar = F)
  return(p)
}


#' Plot of the pre-installation period data
#'
#' \code{pre_plot} Read a baseline object and plots the actual and the fitted data
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline model function

#' @param low_range_0 a numerical value of the lower bound of the eload axis
#' @param high_range_0 a numerical value of the upper bound of the eload axis
#' @param low_range_T_0 a numerical value of the lower bound of the Temp axis
#' @param high_range_T_0 a numerical value of the upper bound of the Temp axis
#'
#' @return A dygraph time series plot object
#'
#' @export

pre_plot <- function(baseline_obj,title = NULL,
                     low_range_0 = NULL,
                     high_range_0 = NULL,
                     low_range_T_0 = NULL,
                     high_range_T_0 = NULL){
  act <- dplyr::select(baseline_obj$train,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if (length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if (length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  eload_xts<-xts::xts(act$eload, order.by = act$dts )
  names(eload_xts) <- c("eload")
  Temp_xts<-xts::xts(act$Temp, order.by = act$dts )
  names(Temp_xts) <- c("Temperature")
  data_xts <- cbind(eload_xts, Temp_xts)

  fit <- as.data.frame(baseline_obj$fitting)
  names(fit) <- c("eload")
  fit$dts <- dts
  if (length(which(duplicated(fit$dts)==T)!=0)){
    fit <- fit[-which(duplicated(fit$dts)==T),]
  }
  if (length(which(is.na(fit$dts)))){
    fit <- fit[-which(is.na(fit$dts)),]
  }
  eload_fit_xts<-xts::xts(fit$eload, order.by = fit$dts )
  names(eload_fit_xts) <- c("eload_fit")
  data_xts <- cbind(data_xts, eload_fit_xts)

  if (!is.null(low_range_0) && !is.null(high_range_0) &&
      !is.null(low_range_T_0) && !is.null(high_range_T_0)){
      low_range <- low_range_0
      low_range_T <- low_range_T_0
      high_range <- high_range_0
      high_range_T <- high_range_T_0
  }

  else{
    # define axis range if they are not specified
    low_range <- min(abs(act$eload), na.rm = T)
    high_range <- as.numeric(quantile(act$eload,probs = 0.975,na.rm = T)) * 1.2
    high_range <- low_range + 2 * (high_range-low_range)
    high_T <- max(act$Temp, na.rm = T) *1.1
    high_range_T <- high_T * 1.15
    low_T <- as.numeric(quantile(act$Temp,probs = 0.025,na.rm = T)) - 5
    low_range_T <- high_range_T - 2 * abs(high_range_T-low_T)
  }

  p <- dygraphs::dygraph(data_xts, main = title) %>% #
       dygraphs::dySeries("eload", label = "Actual", color = "#FFB4A2") %>%
       dygraphs::dySeries("eload_fit", label = "Fitting", color = "#06AED5") %>%
       dygraphs::dyAxis("y", label = "eload",
                        valueRange = c(low_range, high_range)) %>%
       dygraphs::dyAxis("y2", label = "Temperature",
                        valueRange = c(low_range_T, high_range_T))%>%
       dygraphs::dySeries("Temperature", axis = 'y2', label = "Temperature",
                          color = "#FE4A49")  %>%
       dygraphs::dyLegend(width = 350)
  return(p)
}



#' Plot of the post-installation period data
#'
#' \code{pre_plot} Read a baseline object and plots the actual and the predicted data
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline model function

#' @param low_range_0 a numerical value of the lower bound of the eload axis
#' @param high_range_0 a numerical value of the upper bound of the eload axis
#' @param low_range_T_0 a numerical value of the lower bound of the Temp axis
#' @param high_range_T_0 a numerical value of the upper bound of the Temp axis
#'
#' @return A dygraph time series plot object
#'
#' @export

post_gillian <- function(baseline_obj, title=NULL, name="tryingagain",
                      mask = whichpartrev(baseline_obj$pred$eload, n=50),
                      plot_group = NULL,
                      low_range_0 = NULL,
                      high_range_0 = NULL,
                      low_range_T_0 = NULL,
                      high_range_T_0 = NULL){
  
  oact <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  act <- list()
  act$time <- oact$time[mask]
  act$eload <- oact$eload[mask]
  act$Temp <- oact$Temp[mask]
  
  
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if (length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if (length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  act$time <- act$time []
  eload_xts<-xts::xts(act$eload, order.by = act$dts )
  names(eload_xts) <- c("eload")
  Temp_xts<-xts::xts(act$Temp, order.by = act$dts )
  names(Temp_xts) <- c("Temperature")
  data_xts <- cbind(eload_xts, Temp_xts)
  
  pred <- as.data.frame(baseline_obj$prediction[mask])
  names(pred) <- c("eload")
  pred$dts <- dts
  if (length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if (length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }
  eload_pred_xts<-xts::xts(pred$eload, order.by = pred$dts )
  names(eload_pred_xts) <- c("eload_pred")
  data_xts <- cbind(data_xts, eload_pred_xts)
  
  if (!is.null(low_range_0) && !is.null(high_range_0) &&
      !is.null(low_range_T_0) && !is.null(high_range_T_0)){
    low_range <- low_range_0
    low_range_T <- low_range_T_0
    high_range <- high_range_0
    high_range_T <- high_range_T_0
  } else{
    # define axis range if they are not specified
    low_range <- min(abs(act$eload), na.rm = T)
    high_range <- as.numeric(quantile(act$eload,probs = 0.975,na.rm = T)) * 1.2
    high_range <- low_range + 2 * (high_range-low_range)
    high_T <- max(act$Temp, na.rm = T) *1.1
    high_range_T <- high_T * 1.15
    low_T <- as.numeric(quantile(act$Temp,probs = 0.025,na.rm = T)) - 5
    low_range_T <- high_range_T - 2 * abs(high_range_T-low_T)
  }
  
  
  p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
    dygraphs::dySeries("eload", label = "Actual", color = "#FFB4A2") %>%
    dygraphs::dySeries("eload_pred", label = "Prediction",
                       color = "#20BF55") %>%
    dygraphs::dyAxis("y", label = "eload",
                     valueRange = c(low_range, high_range)) %>%
    dygraphs::dyAxis("y2", label = "Temperature",
                     valueRange = c(low_range_T, high_range_T))%>%
    dygraphs::dySeries("Temperature", axis = 'y2',
                       label = "Temperature", color = "#FE4A49")  %>%
    dygraphs::dyLegend(width = 350)
  
  saveWidget(p, "temp.html", selfcontained = FALSE)
  width<- 1080
  height <- 610
  filename = paste("/Users/gillianchu/lbnl/plots/", name, ".png", sep="")
  webshot("temp.html", file = filename, 
          cliprect = c(10,30,width+50,height+50)
          ,vwidth = width, vheight = height )
  
  #return(p)
}

post_plot <- function(baseline_obj,title=NULL,
                      plot_group = NULL,
                      low_range_0 = NULL,
                      high_range_0 = NULL,
                      low_range_T_0 = NULL,
                      high_range_T_0 = NULL){
  act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if (length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if (length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  eload_xts<-xts::xts(act$eload, order.by = act$dts )
  names(eload_xts) <- c("eload")
  Temp_xts<-xts::xts(act$Temp, order.by = act$dts )
  names(Temp_xts) <- c("Temperature")
  data_xts <- cbind(eload_xts, Temp_xts)

  pred <- as.data.frame(baseline_obj$prediction)
  names(pred) <- c("eload")
  pred$dts <- dts
  if (length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if (length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }
  eload_pred_xts<-xts::xts(pred$eload, order.by = pred$dts )
  names(eload_pred_xts) <- c("eload_pred")
  data_xts <- cbind(data_xts, eload_pred_xts)

  if (!is.null(low_range_0) && !is.null(high_range_0) &&
      !is.null(low_range_T_0) && !is.null(high_range_T_0)){
      low_range <- low_range_0
      low_range_T <- low_range_T_0
      high_range <- high_range_0
      high_range_T <- high_range_T_0
  }

  else{
    # define axis range if they are not specified
    low_range <- min(abs(act$eload), na.rm = T)
    high_range <- as.numeric(quantile(act$eload,probs = 0.975,na.rm = T)) * 1.2
    high_range <- low_range + 2 * (high_range-low_range)
    high_T <- max(act$Temp, na.rm = T) *1.1
    high_range_T <- high_T * 1.15
    low_T <- as.numeric(quantile(act$Temp,probs = 0.025,na.rm = T)) - 5
    low_range_T <- high_range_T - 2 * abs(high_range_T-low_T)
  }


  p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
       dygraphs::dySeries("eload", label = "Actual", color = "#FFB4A2") %>%
       dygraphs::dySeries("eload_pred", label = "Prediction",
                          color = "#20BF55") %>%
       dygraphs::dyAxis("y", label = "eload",
                        valueRange = c(low_range, high_range)) %>%
       dygraphs::dyAxis("y2", label = "Temperature",
                        valueRange = c(low_range_T, high_range_T))%>%
       dygraphs::dySeries("Temperature", axis = 'y2',
                          label = "Temperature", color = "#FE4A49")  %>%
       dygraphs::dyLegend(width = 350)
  return(p)
}



#' Plot the residual in the prediction period
#'
#' \code{save_plot} Read a baseline object and plots the residuals between the actual and the predicted data
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline function
#' @param title a character string that corresponds to the main title of the plot

#' @return A dygraph time series plot
#'
#' @export

save_plot <- function(baseline_obj,title=NULL,plot_group=NULL){
  act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if (length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if (length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  pred <- as.data.frame(baseline_obj$prediction)
  names(pred) <- c("eload")
  pred$dts <- dts
  if (length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if (length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }
  savings <- as.data.frame(pred$eload - act$eload)
  names(savings) <- c("savings")
  over <- savings
  over$dts <- dts

  if (length(which(over$savings>=0))!=0){
    over$savings[which(over$savings>=0)] <- NA
    over <- over[-which(is.na(over$savings)),]
  }
  if ((dim(over)[1]) != 0){
    over_xts<-xts::xts(over$savings,order.by = over$dts )
    names(over_xts) <- c("over")
  }

  savings$dts <- dts
  if (length(which(savings$savings<0))!=0){
    savings$savings[which(savings$savings<0)] <- NA
    savings <- savings[-which(is.na(savings$savings)),]
  }
  if ((dim(savings)[1]) != 0){
    savings_xts<-xts::xts(savings$savings,order.by = savings$dts )
    names(savings_xts) <- c("savings")
  }

  if ((dim(savings)[1]) != 0 && (dim(over)[1]) != 0){
    data_xts <- cbind(savings_xts, over_xts)
  }
  if ((dim(savings)[1]) != 0 && (dim(over)[1]) == 0){
    data_xts <- savings_xts
  }
  if ((dim(savings)[1]) == 0 && (dim(over)[1]) != 0){
    data_xts <- over_xts
  }

  if ((dim(savings)[1]) != 0 && (dim(over)[1]) != 0){
    p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
         dygraphs::dySeries("savings", label = "Savings", fillGraph = T,
                            color = "green") %>%
         dygraphs::dySeries("over", label = "Over-Consumption", fillGraph = T,
                            color = "blue") %>%
         dygraphs::dyAxis("y", label = "savings") %>%
         dygraphs::dyLegend(width = 350)
  }
  if ((dim(savings)[1]) != 0 && (dim(over)[1]) == 0){
    p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
         dygraphs::dySeries("savings", label = "Savings", fillGraph = T,
                            color = "green") %>%
         dygraphs::dyAxis("y", label = "savings") %>%
         dygraphs::dyLegend(width = 350)
  }
  if ((dim(savings)[1]) == 0 && (dim(over)[1]) != 0){
    p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
         dygraphs::dySeries("over", label = "Over-Consumption", fillGraph = T,
                            color = "blue") %>%
         dygraphs::dyAxis("y", label = "savings") %>%
         dygraphs::dyLegend(width = 350)
  }

  return(p)
}

#' Plot the residual/savings and the identified change points in the post period
#'
#' \code{cpt_save_plot} Read a baseline object and plots the residuals between the actual and the predicted data
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline function
#' @param title a character string that corresponds to the main title of the plot

#' @return A dygraph time series plot
#'
#' @export

cpt_save_plot <- function(baseline_obj,cpt_det_obj,title=NULL,plot_group=NULL){
  act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if (length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if (length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  pred <- as.data.frame(baseline_obj$prediction)
  names(pred) <- c("eload")
  pred$dts <- dts
  if (length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if (length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }
  savings <- as.data.frame(pred$eload - act$eload)
  names(savings) <- c("savings")
  over <- savings
  over$dts <- dts

  if (length(which(over$savings>=0))!=0){
    over$savings[which(over$savings>=0)] <- NA
    over <- over[-which(is.na(over$savings)),]
  }
  if ((dim(over)[1]) != 0){
    over_xts<-xts::xts(over$savings,order.by = over$dts )
    names(over_xts) <- c("over")
  }

  savings$dts <- dts
  savings_means <- savings
  if (length(which(savings$savings<0))!=0){
    savings$savings[which(savings$savings<0)] <- NA
    savings <- savings[-which(is.na(savings$savings)),]
  }
  if ((dim(savings)[1]) != 0){
    savings_xts<-xts::xts(savings$savings,order.by = savings$dts )
    names(savings_xts) <- c("savings")
  }
  for (i in 1:length(cpt_det_obj$cpts)){
    if (i == 1){start_idx <- 1}
    else {start_idx <- cpt_det_obj$cpts[i-1]}
    end_idx <- cpt_det_obj$cpts[i]
    savings_means$savings[start_idx:end_idx] <- cpt_det_obj$interval_means[i]
  }
  savings_means_xts<-xts::xts(savings_means$savings,order.by = savings_means$dts )
  names(savings_means_xts) <- c("mean_values")
  if ((dim(savings)[1]) != 0 && (dim(over)[1]) != 0){
    data_xts <- cbind(savings_xts, over_xts)
  }
  if ((dim(savings)[1]) != 0 && (dim(over)[1]) == 0){
    data_xts <- savings_xts
  }
  if ((dim(savings)[1]) == 0 && (dim(over)[1]) != 0){
    data_xts <- over_xts
  }

  data_xts <- cbind(data_xts, savings_means_xts)
  idx_cpts <- cpt_det_obj$cpts[-length(cpt_det_obj$cpts)]

  if ((dim(savings)[1]) != 0 && (dim(over)[1]) != 0){
    p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
         dygraphs::dySeries("savings", label = "Savings", fillGraph = T,
                            color = "green") %>%
         dygraphs::dySeries("over", label = "Over-Consumption", fillGraph = T,
                            color = "blue") %>%
         dygraphs::dySeries("mean_values", label = "mean values", fillGraph = F,
                            color = "red", stepPlot = T, strokeWidth = 7) %>%
         dygraphs::dyAxis("y", label = "savings") %>%
         dygraphs::dyEvent(dts[idx_cpts]) %>%
         dygraphs::dyLegend(width = 350)
    return(p)
  }
  if ((dim(savings)[1]) != 0 && (dim(over)[1]) == 0){
    p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
         dygraphs::dySeries("savings", label = "Savings", fillGraph = T,
                            color = "green") %>%
         dygraphs::dySeries("mean_values", label = "mean values", fillGraph = F,
                            color = "red", stepPlot = T, strokeWidth = 7) %>%
         dygraphs::dyAxis("y", label = "savings") %>%
         dygraphs::dyEvent(dts[idx_cpts]) %>%
         dygraphs::dyLegend(width = 350)
    return(p)
  }
  if ((dim(savings)[1]) == 0 && (dim(over)[1]) != 0){
    p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
         dygraphs::dySeries("over", label = "Over-Consumption", fillGraph = T,
                            color = "blue") %>%
         dygraphs::dySeries("mean_values", label = "mean values", fillGraph = F,
                            color = "red", stepPlot = T, strokeWidth = 7) %>%
         dygraphs::dyAxis("y", label = "savings") %>%
         dygraphs::dyEvent(dts[idx_cpts]) %>%
         dygraphs::dyLegend(width = 350)
    return(p)
  }

}


##  -----  Deprecated  -----
# residuals_plot <- function(baseline_obj,title=NULL,plot_group=NULL){
#   act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
#   dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
#   act$dts <- dts
#   if (length(which(duplicated(act$dts)==T)!=0)){
#     act <- act[-which(duplicated(act$dts)==T),]
#   }
#   if (length(which(is.na(act$dts)))!=0){
#     act <- act[-which(is.na(act$dts)),]
#   }
#   pred <- as.data.frame(baseline_obj$prediction)
#   names(pred) <- c("eload")
#   pred$dts <- dts
#   if (length(which(duplicated(pred$dts)==T)!=0)){
#     pred <- pred[-which(duplicated(pred$dts)==T),]
#   }
#   if (length(which(is.na(pred$dts)))){
#     pred <- pred[-which(is.na(pred$dts)),]
#   }
#   savings <- as.data.frame(pred$eload - act$eload)
#   names(savings) <- c("savings")
#   over <- savings
#   over$dts <- dts
#
#   if (length(which(over$savings>=0))!=0){
#     over$savings[which(over$savings>=0)] <- NA
#     over <- over[-which(is.na(over$savings)),]
#   }
#   over_xts<-xts::xts(over$savings,order.by = over$dts )
#   names(over_xts) <- c("over")
#
#   savings$dts <- dts
#   if (length(which(savings$savings<0))!=0){
#     savings$savings[which(savings$savings<0)] <- NA
#     savings <- savings[-which(is.na(savings$savings)),]
#   }
#   savings_xts<-xts::xts(savings$savings,order.by = savings$dts )
#   names(savings_xts) <- c("savings")
#
#   data_xts <- cbind(savings_xts, over_xts)
#
#   p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
#     dygraphs::dySeries("savings", label = "Positive Residuals", fillGraph = T,
#                        color = "blue") %>%
#     dygraphs::dySeries("over", label = "Negative Residuals", fillGraph = T,
#                        color = "red") %>%
#     dygraphs::dyAxis("y", label = "Residuals (kWh)") %>%
#     dygraphs::dyLegend(width = 350)
#   return(p)
# }


#' BarPlot the daily savings
#'
#' \code{daily_save_barplot} Read a baseline object and plots the daily residuals between the actual and the predicted data
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline function
#' @param title a character string that corresponds to the main title of the plot

#' @return A dygraph time series plot object
#'
#' @export

daily_save_barplot<- function(baseline_obj,title=NULL,plot_group=NULL){
  act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if (length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if (length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  pred <- as.data.frame(baseline_obj$prediction)
  names(pred) <- c("eload")
  pred$dts <- dts
  if (length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if (length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }

  act$date <- as.Date(act$dts)
  act_daily <- act %>%
               dplyr::group_by(date) %>%
               dplyr::summarise(eload=sum(eload))
  pred$date <- as.Date(pred$dts)
  pred_daily <- pred %>%
                dplyr::group_by(date) %>%
                dplyr::summarise(eload=sum(eload))
  savings <- as.data.frame(pred_daily$eload - act_daily$eload)
  names(savings) <- c("savings")
  savings$date <- act_daily$date
  over <- savings
  over$date <- savings$date

  if (length(which(over$savings>=0))!=0){
    over$savings[which(over$savings>=0)] <- NA
    #over <- over[-which(is.na(over$savings)),]
  }
  # add empty date at the begining and at the end for plotting purpose
  over_new <- over$savings
  over_new <- c(NA,over_new,NA)
  date_new <- over$date
  date_new <- c(date_new[1] - as.difftime(1, unit="days"), date_new, date_new[length(date_new)] + as.difftime(1, unit="days"))

  over_xts<-xts::xts(over_new,order.by = date_new)
  names(over_xts) <- c("Over-Consumption")

  savings$date <- act_daily$date
  if (length(which(savings$savings<0))!=0){
    savings$savings[which(savings$savings<0)] <- NA
    #savings <- savings[-which(is.na(savings$savings)),]
  }
  # add empty date at the begining and at the end for plotting purpose
  savings_new <- savings$savings
  savings_new <- c(NA,savings_new,NA)
  date_new <- savings$date
  date_new <- c(date_new[1] - as.difftime(1, unit="days"), date_new, date_new[length(date_new)] + as.difftime(1, unit="days"))

  savings_xts<-xts::xts(savings_new,order.by = date_new )
  names(savings_xts) <- c("savings")


  data_xts <- cbind(savings_xts, over_xts)

  p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
       dyMultiColumn()%>%
       dygraphs::dyAxis("x", drawGrid = F) %>%
       dygraphs::dyAxis("y", label = "Daily Savings") %>%
       dygraphs::dyOptions(colors = c("#4DAF4A","blue"),
                 gridLineColor = "lightblue")
  return(p)
}





#' BarPlot the weekly savings
#'
#' \code{weekly_save_barplot} Read a baseline object and plots the weekly residuals between the actual and the predicted data
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline function
#' @param title a character string that corresponds to the main title of the plot

#' @return A dygraph time series plot
#'
#' @export

weekly_save_barplot <- function(baseline_obj,title=NULL,plot_group=NULL){
  act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if (length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if (length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  pred <- as.data.frame(baseline_obj$prediction)
  names(pred) <- c("eload")
  pred$dts <- dts
  if (length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if (length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }

  act$date <- as.Date(act$dts)
  act_daily <- act %>%
               dplyr::group_by(date) %>%
               dplyr::summarise(eload=sum(eload))

  pred$date <- as.Date(pred$dts)
  pred_daily <- pred %>%
                dplyr::group_by(date) %>%
                dplyr::summarise(eload=sum(eload))

  act_daily$week <- as.Date(cut(act_daily$date,breaks = "week",start.on.monday = T))
  act_weekly <- act_daily %>% dplyr::group_by(week) %>% dplyr::summarise(eload=sum(eload))

  pred_daily$week <- as.Date(cut(pred_daily$date,breaks = "week",start.on.monday = T))
  pred_weekly <- pred_daily %>%
                 dplyr::group_by(week) %>%
                 dplyr::summarise(eload=sum(eload))

  savings <- as.data.frame(pred_weekly$eload - act_weekly$eload)
  names(savings) <- c("savings")
  over <- savings
  over$week <- act_weekly$week

  if (length(which(over$savings>=0))!=0){
    over$savings[which(over$savings>=0)] <- NA
    #over <- over[-which(is.na(over$savings)),]
  }
  # add empty week at the begining and at the end for plotting purpose
  over_new <- over$savings
  over_new <- c(NA,over_new,NA)
  week_new <- over$week
  week_new <- c(week_new[1] - as.difftime(1, unit="weeks"), week_new, week_new[length(week_new)] + as.difftime(1, unit="weeks"))

  over_xts<-xts::xts(over_new,order.by = week_new)
  names(over_xts) <- c("Over-Consumption")

  savings$week <- act_weekly$week
  if (length(which(savings$savings<0))!=0){
    savings$savings[which(savings$savings<0)] <- NA
    #savings <- savings[-which(is.na(savings$savings)),]
  }
  # add empty week at the begining and at the end for plotting purpose
  savings_new <- savings$savings
  savings_new <- c(NA,savings_new,NA)
  week_new <- savings$week
  week_new <- c(week_new[1] - as.difftime(1, unit="weeks"), week_new, week_new[length(week_new)] + as.difftime(1, unit="weeks"))

  savings_xts<-xts::xts(savings_new,order.by = week_new )
  names(savings_xts) <- c("savings")


  data_xts <- cbind(savings_xts, over_xts)

  p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
       dyMultiColumn()%>%
       dygraphs::dyAxis("x", drawGrid = F) %>%
       dygraphs::dyAxis("y", label = "Weekly Savings") %>%
       dygraphs::dyOptions(colors = c("#4DAF4A","blue"),
                 gridLineColor = "lightblue")
  return(p)

}


#' BarPlot the monthly savings
#'
#' \code{monthly_save_barplot} Read a baseline object and plots the monthly residuals between the actual and the predicted data
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline function
#' @param title a character string that corresponds to the main title of the plot

#' @return A dygraph time series plot
#'
#' @export

monthly_save_barplot <- function(baseline_obj,title=NULL,plot_group=NULL){
  act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if (length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if (length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  pred <- as.data.frame(baseline_obj$prediction)
  names(pred) <- c("eload")
  pred$dts <- dts
  if (length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if (length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }

  act$date <- as.Date(act$dts)
  act_daily <- act %>%
               dplyr::group_by(date) %>%
               dplyr::summarise(eload=sum(eload))

  pred$date <- as.Date(pred$dts)
  pred_daily <- pred %>%
                dplyr::group_by(date) %>%
                dplyr::summarise(eload=sum(eload))
  act_daily$month <- as.Date(cut(act_daily$date,breaks = "month"))
  act_monthly <- act_daily %>%
                 dplyr::group_by(month) %>%
                 dplyr::summarise(eload=sum(eload))

  pred_daily$month <- as.Date(cut(pred_daily$date,breaks = "month"))
  pred_monthly <- pred_daily %>%
                  dplyr::group_by(month) %>%
                  dplyr::summarise(eload=sum(eload))

  savings <- as.data.frame(pred_monthly$eload - act_monthly$eload)
  names(savings) <- c("savings")

  over <- savings
  over$month <- act_monthly$month

  if (length(which(over$savings>=0))!=0){
    over$savings[which(over$savings>=0)] <- NA
    #over <- over[-which(is.na(over$savings)),]
  }

  # add empty month at the begining and at the end for plotting purpose
  over_new <- over$savings
  over_new <- c(NA,over_new,NA)
  month_new <- over$month
  month_new <- c(month_new[1] - as.difftime(30, unit="days"), month_new, month_new[length(month_new)] + as.difftime(30, unit="days"))

  over_xts<-xts::xts(over_new,order.by = month_new)
  names(over_xts) <- c("Over-Consumption")

  savings$month <- act_monthly$month
  if (length(which(savings$savings<0))!=0){
    savings$savings[which(savings$savings<0)] <- NA
    #savings <- savings[-which(is.na(savings$savings)),]
  }
  # add empty month at the begining and at the end for plotting purpose
  savings_new <- savings$savings
  savings_new <- c(NA,savings_new,NA)
  month_new <- savings$month
  month_new <- c(month_new[1] - as.difftime(30, unit="days"), month_new, month_new[length(month_new)] + as.difftime(30, unit="days"))

  savings_xts<-xts::xts(savings_new,order.by = month_new )
  names(savings_xts) <- c("savings")

  data_xts <- cbind(savings_xts, over_xts)

  p <- dygraphs::dygraph(data_xts, main = title, group = plot_group) %>%
       dyMultiColumn()%>%
       dygraphs::dyAxis("x", drawGrid = F) %>%
       dygraphs::dyAxis("y", label = "Weekly Savings") %>%
       dygraphs::dyOptions(colors = c("#4DAF4A","blue"),
                 gridLineColor = "lightblue")
  return(p)
}

#' @export
savings_plot_type <- function(baseline_obj,granularity){
  switch(granularity,
    "Original Granularity" = p <- save_plot(baseline_obj),
    "Daily" = p<- daily_save_barplot(baseline_obj),
    "Weekly" = p <- weekly_save_barplot(baseline_obj),
    "Monthly" = p <- monthly_save_barplot(baseline_obj)
  )
  return(p)
}

#' Plot the CUSUM
#'
#' \code{cusum_plot} Read a baseline object and plots the cumulative sum of the savings
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline function
#' @param title a character string that corresponds to the main title of the plot

#' @return A dygraph time series plot
#'
#' @export

cusum_plot <- function(baseline_obj,title=NULL,plot_group=NULL){
  act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if (length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if (length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  pred <- as.data.frame(baseline_obj$prediction)
  names(pred) <- c("eload")
  pred$dts <- dts
  if (length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if (length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }
  savings <- as.data.frame(pred$eload - act$eload)
  names(savings) <- c("savings")

  cusum_savings <- cumsum(savings)
  cusum_savings$dts <- dts
  cusum_savings_xts<-xts::xts(cusum_savings$savings,order.by = cusum_savings$dts )
  names(cusum_savings_xts) <- c("cusum_savings")

  p <- dygraphs::dygraph(cusum_savings_xts, main = title, group = plot_group) %>%
       dygraphs::dySeries("cusum_savings",
                          label = "Cumulative Sum of Differences",
                          fillGraph = T, color = "#377EB8") %>%
       dygraphs::dyAxis("y", label = "Cumulative Sum of Differences") %>%
       dygraphs::dyLegend(width = 350)
  return(p)
}




#' Plot the daily CUSUM
#'
#' \code{daily_cusum_plot} Read a baseline object and plots the cumulative sum of the savings
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline function
#' @param title a character string that corresponds to the main title of the plot

#' @return A dygraph time series plot
#'
#' @export

daily_cusum_barplot <- function(baseline_obj,title=NULL,plot_group=NULL){
  act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if(length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if(length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  pred <- as.data.frame(baseline_obj$prediction)
  names(pred) <- c("eload")
  pred$dts <- dts
  if(length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if(length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }

  act$date <- as.Date(act$dts)
  act_daily <- act %>%
               dplyr::group_by(date) %>%
               dplyr::summarise(eload=sum(eload))

  pred$date <- as.Date(pred$dts)
  pred_daily <- pred %>%
                dplyr::group_by(date) %>%
                dplyr::summarise(eload=sum(eload))

  savings <- as.data.frame(pred_daily$eload - act_daily$eload)
  names(savings) <- c("savings")

  cusum_savings <- cumsum(savings)
  cusum_savings$date <- act_daily$date
  cusum_savings_xts<-xts::xts(cusum_savings$savings,order.by = cusum_savings$date )
  names(cusum_savings_xts) <- c("cusum_savings")

  p <- dygraphs::dygraph(cusum_savings_xts, main = title, group = plot_group) %>%
       dyBarChart()%>%
       dygraphs::dyAxis("x", drawGrid = F) %>%
       dygraphs::dyAxis("y", label = "Daily Cumulative Savings") %>%
       dygraphs::dyOptions(colors = "#377EB8",
                           gridLineColor = "lightblue")
  return(p)
}



#' BarPlot the weekly CUSUM
#'
#' \code{monthly_cusum_barplot} Read a baseline object and plots the cumulative sum of the savings
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline function
#' @param title a character string that corresponds to the main title of the plot

#' @return A dygraph time series plot
#'
#' @export

weekly_cusum_barplot <- function(baseline_obj,title=NULL,plot_group=NULL){
  act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if(length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if(length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  pred <- as.data.frame(baseline_obj$prediction)
  names(pred) <- c("eload")
  pred$dts <- dts
  if(length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if(length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }

  act$date <- as.Date(act$dts)
  act_daily <- act %>%
               dplyr::group_by(date) %>%
               dplyr::summarise(eload=sum(eload))

  pred$date <- as.Date(pred$dts)
  pred_daily <- pred %>%
                dplyr::group_by(date) %>%
                dplyr::summarise(eload=sum(eload))

  act_daily$week <- as.Date(cut(act_daily$date,breaks = "week",start.on.monday = T))
  act_weekly <- act_daily %>%
                dplyr::group_by(week) %>%
                dplyr::summarise(eload=sum(eload))

  pred_daily$week <- as.Date(cut(pred_daily$date,breaks = "week",start.on.monday = T))
  pred_weekly <- pred_daily %>%
                 dplyr::group_by(week) %>%
                 dplyr::summarise(eload=sum(eload))

  savings <- as.data.frame(pred_weekly$eload - act_weekly$eload)
  names(savings) <- c("savings")

  cusum_savings <- cumsum(savings)
  cusum_savings$week <- act_weekly$week
  cusum_savings <- dplyr::select(cusum_savings,week,savings)

  # add empty week at the begining and at the end for plotting purpose
  cusum_new <- cusum_savings$savings
  cusum_new <- c(NA,cusum_new,NA)
  week_new <- cusum_savings$week
  week_new <- c(week_new[1] - as.difftime(1, unit="weeks"), week_new, week_new[length(week_new)] + as.difftime(1, unit="weeks"))

  cusum_savings_xts<-xts::xts(cusum_new,order.by = week_new)
  names(cusum_savings_xts) <- c("cusum_savings")

  p <- dygraphs::dygraph(cusum_savings_xts, main = title, group = plot_group) %>%
       dyBarChart()%>%
       dygraphs::dyAxis("x", drawGrid = F) %>%
       dygraphs::dyAxis("y", label = "Weekly Cumulative Savings") %>%
       dygraphs::dyOptions(colors = "#377EB8",
                           gridLineColor = "lightblue")
  return(p)

}



#' BarPlot the monthly CUSUM
#'
#' \code{monthly_cusum_barplot} Read a baseline object and plots the cumulative sum of the savings
#'
#'
#' @param baseline_obj A baseline object, which is produced by the baseline function
#' @param title a character string that corresponds to the main title of the plot

#' @return A dygraph time series plot
#'
#' @export

monthly_cusum_barplot <- function(baseline_obj,title=NULL,plot_group=NULL){
  act <- dplyr::select(baseline_obj$pred,time,eload,Temp)
  dts <- as.POSIXct(strptime(act$time, format = "%m/%d/%y %H:%M"))
  act$dts <- dts
  if(length(which(duplicated(act$dts)==T)!=0)){
    act <- act[-which(duplicated(act$dts)==T),]
  }
  if(length(which(is.na(act$dts)))!=0){
    act <- act[-which(is.na(act$dts)),]
  }
  pred <- as.data.frame(baseline_obj$prediction)
  names(pred) <- c("eload")
  pred$dts <- dts
  if(length(which(duplicated(pred$dts)==T)!=0)){
    pred <- pred[-which(duplicated(pred$dts)==T),]
  }
  if(length(which(is.na(pred$dts)))){
    pred <- pred[-which(is.na(pred$dts)),]
  }

  act$date <- as.Date(act$dts)
  act_daily <- act %>%
               dplyr::group_by(date) %>%
               dplyr::summarise(eload=sum(eload))

  pred$date <- as.Date(pred$dts)
  pred_daily <- pred %>%
                dplyr::group_by(date) %>%
                dplyr::summarise(eload=sum(eload))

  act_daily$month <- as.Date(cut(act_daily$date,breaks = "month"))
  act_monthly <- act_daily %>%
                 dplyr::group_by(month) %>%
                 dplyr::summarise(eload=sum(eload))

  pred_daily$month <- as.Date(cut(pred_daily$date,breaks = "month"))
  pred_monthly <- pred_daily %>%
                  dplyr::group_by(month) %>%
                  dplyr::summarise(eload=sum(eload))

  savings <- as.data.frame(pred_monthly$eload - act_monthly$eload)
  names(savings) <- c("savings")

  cusum_savings <- cumsum(savings)
  cusum_savings$month <- act_monthly$month

  # add empty month at the begining and at the end for plotting purpose
  cusum_new <- cusum_savings$savings
  cusum_new <- c(NA,cusum_new,NA)
  month_new <- cusum_savings$month
  month_new <- c(month_new[1] - as.difftime(30, unit="days"), month_new, month_new[length(month_new)] + as.difftime(30, unit="days"))

  cusum_savings_xts<-xts::xts(cusum_new,order.by = month_new)
  names(cusum_savings_xts) <- c("cusum_savings")
  p <- dygraphs::dygraph(cusum_savings_xts, main = title, group = plot_group) %>%
       dyBarChart()%>%
       dygraphs::dyAxis("x", drawGrid = F) %>%
       dygraphs::dyAxis("y", label = "Monthly Cumulative Savings") %>%
       dygraphs::dyOptions(colors = "#377EB8",
                           gridLineColor = "lightblue")
  return(p)
}

#' @export
cusum_plot_type <- function(baseline_obj,granularity){
  switch(granularity,
    "Original Granularity" = p <- cusum_plot(baseline_obj),
    "Daily" = p<- daily_cusum_barplot(baseline_obj),
    "Weekly" = p <- weekly_cusum_barplot(baseline_obj),
    "Monthly" = p <- monthly_cusum_barplot(baseline_obj)
  )
  return(p)
}



#' @export
dyMultiColumn <- function(dygraph) {
  dygraphs::dyPlotter(dygraph = dygraph,
                      name = "MultiColumn",
                      path = system.file("js_files/barchart_2.js",
                                         package = "RMV2.0"))
}


#' @export
dyBarChart <- function(dygraph) {
  dygraphs::dyPlotter(dygraph = dygraph,
                      name = "BarChart",
                      path = system.file("js_files/barchart.js",
                                         package = "RMV2.0"))
}
