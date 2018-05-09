#-------------------------------------------------------------------------------
# RMV2.0 (version 1.1.0)
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# These codes are an adaptation of the original codes that can be found on:
# https://bitbucket.org/berkeleylab/eetd-loadshape
#-------------------------------------------------------------------------------

#' Time Of the Week and Temperature baseline model function.
#'
#' \code{towt_baseline} This function builds a baseline model using gradient boosting machine algorithm.
#'
#'
#' @param train_path The path of the file from which the training data are to be read from.
#' @param pred_path The path of the file from which the prediction data are to be read from.
#' @param train_Data A dataframe, of the training period, where the columns correspond to the time steps (time), the energy load (eload) and to the Temperature (Temp).
#' @param pred_Data A dataframe, of the prediction period, where the columns correspond to the time steps (time), the energy load (eload) and to the Temperature (Temp).
#' @param timescaleDays Numeric correspond to the timescale for weighting function.
#' @param verbosity Numeric that determine what progress and error reports to print .
#' @param intervalMinutes Numeric of length of a Time Of Week interval as input variables.
#' @param fahrenheit Boolean that determine if the temperatures are in  Fahrenheit.
#'
#' @return a towt_baseline object, which alist with the following components:
#' \describe{
#'   \item{towt_model}{an object that has been created by the function makeBaseline,
#'    and which correspond to the towt model.}
#'   \item{train}{a data frame that correspond to the training data after the
#'   cleaning and filtering function were applied.}
#'   \item{fitting}{the fitted values.}
#'   \item{goodness_of_fit}{a data frame that contains the goodness of fitting metrics.}
#'   \item{pred}{a data frame that correspond to the prediction data after the
#'   cleaning and filtering function were applied.}
#'   \item{prediction}{the predicted values.}
#' }
#' @export



towt_baseline <- function(train_path = NULL,
                         pred_path = NULL,
                         train_Data = NULL,
                         pred_Data = NULL,
                         timescaleDays = 14,
                         verbosity = 1,
                         intervalMinutes = 15,
                         fahrenheit = T){
  # train read and preprocess
  if (!is.null(train_path)){
   train <- read.csv(file = train_path, header=T,
                     row.names = NULL, stringsAsFactors = F)
  }
  else{train <- train_Data}
  train <- towt_time_var(train)
  train <- clean_Temp(train)
  train <- clean_eload(train)
  train$time <- as.POSIXlt(train$time)

  # pred read and preprocessing
  if (!is.null(pred_path)) {
    pred <- read.csv(file = pred_path, header=T,
                     row.names = NULL, stringsAsFactors = F)
   }
   else(pred <- pred_Data)
   pred <- towt_time_var(pred)
   pred <- clean_Temp(pred)
   pred$time <- as.POSIXlt(pred$time)

  #else{pred <- train}
  doTemperatureModel = T

  towt_model <- makeBaseline(train$time,
                             train$eload,
                             train$Temp,
                             pred$time,
                             pred$Temp,
                             intervalMinutes=intervalMinutes,
                             timescaleDays=timescaleDays,
                             fahrenheit=fahrenheit,
                             verbose=verbosity)



  # Fitting results:
  y_fit <- towt_model$trainBaseline
  train_output <- train$eload
  fit_residuals <- train_output - y_fit
  goodness_of_fit <- as.data.frame(matrix(nr=1,nc=4))
  names(goodness_of_fit) <- c("fit_R2","fit_RMSE","fit_CVRMSE","fit_NMBE")
  goodness_of_fit$fit_R2 <- 100*(1-mean((fit_residuals)^2)/var(train_output))
  goodness_of_fit$fit_RMSE <- sqrt(mean((fit_residuals)^2))
  goodness_of_fit$fit_CVRMSE <- 100*sqrt(mean((fit_residuals)^2))/mean(train_output)
  goodness_of_fit$fit_NMBE <- 100*mean((fit_residuals))/mean(train_output)

  res <- NULL
  res$towt_model <- towt_model
  train$time <- format(train$time,"%m/%d/%y %H:%M")
  res$train <- train
  res$fitting <- y_fit
  res$goodness_of_fit <- goodness_of_fit

  # Prediction
  if (!is.null(pred_path) | !is.null(pred_Data)) {
   # We only want the baseline prediction at the times that we ask for it
   # interpolate baseline load to the actual times
   # use "constant" rather than "linear" interpolation. Note that for
   # the interval that runs from t1 to t2, we want the value to be
   # constant at y(t2) not y(t1). This is because the meter reports
   # the average power over the period that _ends_ at the reported time.
   # Create a new time vector and load vector that shift things to the
   # start of the interval rather than the end.
   # We'll make:
   # y(t1) = t1
   # y(t1+delta) = y(t2)
   # y(t2+delta) = y(t3)
   # and so on.
   tBaseNum <- as.numeric(towt_model$timeVec)
   baseline <-  towt_model$Baseline
   y_pred <- approx(tBaseNum,
                    baseline,
                    as.numeric(pred$time),
                    method="constant")$y
   pred$time <- format(pred$time,"%m/%d/%y %H:%M")
   res$pred <- pred
   res$prediction <- y_pred
  }
  return(res)
}

#-------------------------------------------------------------------------------
#
#     Data preprocessing
#
#-------------------------------------------------------------------------------

#' Convert the time format
#'
#' \code{towt_time_var} Function that exclude observation where the time stamps are missing and convert the time column into the POSIXct format.
#'
#'
#' @param Data A dataframe of the training or prediction data.
#'
#' @return A dataframe where the time column is in the POSIXct format.
#'
#' @export

towt_time_var <- function(Data){
  dts <- as.POSIXct(strptime(Data$time, format = "%m/%d/%y %H:%M"))
  Data$time <- dts
  Data <- Data[complete.cases(Data),]
  return(Data)
}

#-------------------------------------------------------------------------------
#
#     Functions used by the towt_baseline function to build the towt model
#
#-------------------------------------------------------------------------------


piecewiseVariables <- function(Tvec,Tknot) {
	nT = length(Tvec)
	nbins = length(Tknot) + 1 # lower than lowest; in between; and higher than highest
	Tknot = c(-1000000,Tknot,1000000)  # Add a knot to make the loop below work out right

	Tmat = matrix(0,nrow=nT,ncol=nbins)
	for (ibin in 1:nbins) {
		ok = (Tvec > Tknot[ibin]) & (Tvec <= Tknot[ibin+1])
		ok[is.na(ok)] = F
		if (ibin ==1) {
			Tmat[ok,ibin] = Tvec[ok]
			Tmat[!ok,ibin] = Tknot[ibin+1]
		} else {
			Tmat[ok,ibin] = Tvec[ok] - Tknot[ibin]
			Tmat[Tvec > Tknot[ibin+1],ibin] = Tknot[ibin+1]-Tknot[ibin]
		}
	}
	return(Tmat)
}


##

findOccUnocc<- function(intervalOfWeek,loadVec,TempF,intervalMinutes=15,verbose=1) {
	if (verbose > 4) { print("starting findOccUnocc()") }
	# Figure out which times of week a building is in one of two modes
	#  (called 'occupied' or 'unoccupied')

	uTOW = unique(intervalOfWeek)
	nTOW = length(uTOW)

	# Define 'occupied' and 'unoccupied' based on a regression
	# of load on outdoor temperature: times of week that the regression usually
	# underpredicts the load will be called 'occupied', the rest are 'unoccupied'
	# This is not foolproof but usually works well.
	#
	TempF50 = TempF-50
	TempF50[TempF > 50] = 0
	TempF65 = TempF-65
	TempF65[TempF < 65] = 0

	if (verbose > 4) {
		print("fitting temperature regression")
	}
	amod = lm(loadVec ~ TempF50+TempF65,na.action=na.exclude)

	okocc = rep(0,nTOW)
	for (itow in 1:nTOW) {
		okTOW = intervalOfWeek==uTOW[itow]
		# if the regression underpredicts the load more than 65% of the time
		# then assume it's an occupied period
		if ( sum(residuals(amod)[okTOW]>0,na.rm=T) > 0.65*sum(okTOW) ) {
			okocc[itow]=1
		}
	}
	if (verbose > 4) { print("leaving findOccUnocc()") }
	return(cbind(uTOW,okocc))
}

###

fitLBNLregress <- function(timeVec,loadVec,tempVec,
	predTime,predTemp,tempKnots,weightvec=NULL,
	intervalMinutes=15, fahrenheit = F,
	doTemperatureModel=doTemperatureModel,verbose=1) {

	if (verbose > 3) {print("starting fitLBNLregress()")}
	if (!is.null(weightvec)) {
		# if weights are specified then base occupied/unoccupied decision
		# just on the relatively higher weights
		okUseForOcc = weightvec > 0.2*max(weightvec,na.rm=T)
		okUseForOcc[is.na(okUseForOcc)]=F
	} else {
		okUseForOcc = rep(T,length(loadVec))
	}

	minuteOfWeek = 24*60*timeVec$wday+60*timeVec$hour + timeVec$min
	intervalOfWeek = 1+floor(minuteOfWeek/intervalMinutes)
	nLoadTime = as.numeric(timeVec)

	minuteOfWeekPred = 24*60*predTime$wday+60*predTime$hour + predTime$min
	intervalOfWeekPred = 1+floor(minuteOfWeekPred/intervalMinutes)
	nPredTime = as.numeric(predTime)

	# If there's no temperature data, just fit the time-of-week regression.
	# In this case there is no difference between occupied and unoccupied periods,
	# just do one prediction.

	if (is.null(tempVec) | !doTemperatureModel) {
		# If we don't have temperature data then just fit the time-of-week model

		# make data frame for explanatory variables in training period
		# We will use the variable name ftow first for the training period and
		#	then for the prediction period for notational convenience when using
		#	the predict() function.

		if (verbose > 3) {print("fitting TOW model for training period")}

		ftow = factor(intervalOfWeek)
		dframe = data.frame(ftow)
		amod = lm(loadVec ~ .+0,data=dframe, na.action=na.exclude,
				weights = weightvec)
		trainingLoadPred = predict(amod)

		if (verbose > 3) {print("predicting baseline for prediction period")}
		ftow = factor(intervalOfWeekPred)
		dframePred = data.frame(ftow)
		oktowpred = factor(ftow) %in% amod$xlevels$ftow
		predVec = rep(NA,length(predTime))
		predVec[oktowpred] = predict(amod,dframePred)

	} else {
		# If we have temperature data then fit the time-of-week-and-temperature model

		if (fahrenheit) {
			# temperature vector is already in fahrenheit
			tempVecF = tempVec
			tempVec = (tempVec-32)*5/9
			tempVecPredF = predTemp
			tempVecPred = (predTemp-32)*5/9
		} else {
			tempVecF = (tempVec*9/5)+32
			tempVecPredF = (predTemp*9/5)+32
			tempVecPred = predTemp
		}
		# findOccUnocc requires Fahrenheit temperatures; everywhere else we can use either
		#  Celsius or Fahrenheit, as long as temperature knots are set appropriately
		#
		# base occupied/unoccupied decision only on cases where we have load data:
		okload = !is.na(loadVec)
		occInfo = findOccUnocc(intervalOfWeek[okload],loadVec[okload],tempVecF[okload])
		occIntervals = occInfo[occInfo[,2]==1,1]  # which time intervals are 'occupied'?
		#

		occVec = rep(0,length(loadVec))
		if (length(occIntervals) > 2) {
			for (i in 1:length(occIntervals)) {
				occVec[intervalOfWeek==occIntervals[i]] = 1
			}
		}

		if (verbose > 3) {print("done determining occupied hours")}

		# If there aren't enough temperature data above the highest temperature knot,
		# then remove the knot. Repeat until there are sufficient data above the highest
		# remaining knot, or until there's only one knot left.
		ntempknots = length(tempKnots)
		checkknots = T
		while (checkknots) {
			if (sum(tempVec[okload] > tempKnots[ntempknots],na.rm=T) < 20) {
				# not enough data above upper knot; throw away that upper knot
				tempKnots = tempKnots[-ntempknots]
				ntempknots = ntempknots - 1
				if (ntempknots == 1) {
					# We have to keep at least one knot, even if we have no data above it.
					# A real fix requires rewriting piecewiseVariables so it can handle
					# a case with no knots (just a single linear temperature dependence);
					# not doing this for now.
					checkknots = F
				}
			} else {
				# We have enough data above the upper knot, so need to keep checking
				checkknots = F

			}
		} #endwhile
		# Same principle as above, for aomount of data below the lowest knot.
		checkknots = T
		while (checkknots) {
			if (sum(tempVec[okload] < tempKnots[1], na.rm=T) < 20) {
				# not enough data below lower knot; throw away that lower knot
				tempKnots = tempKnots[-1]
				ntempknots = ntempknots-1
				if (ntempknots == 1) {
					# We have to keep one knot, even though we have no data below it.
					checkknots = F
				}
			} else {
				checkknots = F # we have sufficient data below the lowest knot
			}
		} #endwhile
		tempMat = piecewiseVariables(tempVec,tempKnots)
		tempMatPred = piecewiseVariables(tempVecPred,tempKnots)
		tMname=rep(NA,ncol(tempMat))
		for(i in 1:ncol(tempMat)) {
			tMname[i]=paste("tempMat",i,sep="")
		}
		names(tempMat) = tMname
		names(tempMatPred) = tMname

		if (verbose > 3) { print("done setting up temperature matrix") }

		if (is.null(weightvec)) {
			weightvec = rep(1,length(loadVec))
		}

		# make data frame for explanatory variables in training period
		# We will use the variable name ftow twice, first for the training period and
		#	then for the prediction period, for notational convenience when using
		#	the predict() function.
		ftow = factor(intervalOfWeek)
		dframe = data.frame(ftow,tempMat)
		trainingLoadPred = rep(NA,nrow(dframe))

		# make data frame for explanatory variables in prediction period
		ftow = factor(intervalOfWeekPred)
		dframePred = data.frame(ftow,tempMatPred)
		predVec = rep(NA,length(predTime))

		okocc = occVec==1
		okocc[is.na(okocc)] = T


		if(sum(okocc > 0)) {
			if (verbose > 3) { print("fitting regression for occupied periods") }
			# fit model to training data
			amod = lm(loadVec ~ .+0,data=dframe, na.action=na.exclude,
				weights = weightvec,subset=okocc)
			tP = predict(amod,dframe[okocc,])
			trainingLoadPred[okocc] = tP

			# Now make predictions for prediction period
			# filter out times of week that are not in occupied training period.
			oktowpred = dframePred$ftow %in% amod$xlevels$ftow
			predVec[oktowpred] = predict(amod,dframePred[oktowpred,])
			if (verbose > 3) { print("done with prediction for occupied periods") }
		}

		if (sum(!okocc) > 0) {
			if (verbose > 3) { print("fitting regression for unoccupied periods") }
			bmod = lm(loadVec ~ .+0,data=dframe,na.action=na.exclude,
				weights = weightvec,subset=!okocc)
			tP = predict(bmod,dframe[!okocc,])
			trainingLoadPred[!okocc] = tP

			# filter out times of week that are not in unoccupied training period.
			oktowpred = dframePred$ftow %in% bmod$xlevels$ftow
			predVec[oktowpred] = predict(bmod,dframePred[oktowpred,])
			if (verbose > 3) { print("done with prediction for unoccupied periods") }
		}

	}

	predVec[predVec < 0] = 0

	# Out$training has baseline predictions for training period
	# Out$predictions has baseline predictions for prediction period
	Out = NULL
	Out$training = data.frame(timeVec,nLoadTime,trainingLoadPred)
	Out$predictions = data.frame(predTime,nPredTime,predVec)

	if (verbose > 3) {print("leaving fitLBNLregress()")}
	return(Out)
}

###

makeBaseline <- function(dataTime, dataLoad, dataTemp, predTime, predTemp,
	intervalMinutes=15, timescaleDays = 14,fahrenheit = F,
	doTemperatureModel=T,verbose=1) {

	if (verbose > 2) { print("starting makeBaseline()") }
	npoints = length(dataLoad)

	t0 = min(dataTime,na.rm=T)
	t1 = max(dataTime,na.rm=T)

	deltaT = as.numeric(difftime(t1,t0,units="days"))
	nsegments = max(1,ceiling(deltaT/timescaleDays))
	segmentwidth = (npoints-1)/nsegments
	pointlist = floor(sort(npoints-segmentwidth*(0:nsegments))+0.001)

	nModelRuns = max(1,length(pointlist))

	TrainMatrix = matrix(NA,nrow=nModelRuns,ncol=length(dataTime))
	PredMatrix = matrix(NA,nrow=nModelRuns,ncol=length(predTime))

	TrainWeightMatrix = matrix(NA, nrow=nModelRuns,ncol=length(dataTime))
	WeightMatrix = matrix(NA,nrow=nModelRuns,ncol=length(predTime))

	if (verbose > 2) {print(paste("running regression at",nModelRuns,"steps"))}
	for (irun in 1:nModelRuns) {
		if (verbose > 4) { print(paste("starting model run number",irun)) }
		tcenter = dataTime[pointlist[irun]]
		tDiff = as.numeric(difftime(tcenter,dataTime,units="days"))
		tDiffPred = as.numeric(difftime(tcenter,predTime,units="days"))

		# Statistical weight for training period
		weightvec = timescaleDays^2/(timescaleDays^2 + tDiff^2)

		# Statistical weight for prediction period
		weightvecPred = timescaleDays^2/(timescaleDays^2 + tDiffPred^2)

		tempKnots = (c(40, 55, 65, 80, 90)-32)*5/9

		regOut = fitLBNLregress(dataTime,dataLoad,dataTemp,
			predTime,predTemp,
			tempKnots = tempKnots, weightvec=weightvec,
			intervalMinutes=intervalMinutes,fahrenheit=fahrenheit,
			doTemperatureModel=doTemperatureModel,verbose=verbose)

		trainOut = regOut$training
		TrainMatrix[irun,] = trainOut$trainingLoadPred
		TrainWeightMatrix[irun,] = weightvec

		predOut = regOut$predictions
		PredMatrix[irun,] = predOut$predVec
		WeightMatrix[irun,] = weightvecPred
	}
	finalBaseline = apply(PredMatrix*WeightMatrix,2,sum)/apply(WeightMatrix,2,sum)
	finalTrainBaseline =
		apply(TrainMatrix*TrainWeightMatrix,2,sum)/apply(TrainWeightMatrix,2,sum)

	Out = NULL
	Out$timeVec = predTime
	Out$Baseline = finalBaseline
	Out$PredMatrix = PredMatrix
	Out$WeightMatrix = WeightMatrix
	Out$trainTime = dataTime
	Out$trainBaseline = finalTrainBaseline
	if (verbose > 2) { print("leaving makeBaseline()") }
	return(Out)
}
