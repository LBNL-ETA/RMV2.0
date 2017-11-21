# RMV2.0 - LBNL M&V2.0 Tool

***

**RMV2.0** is an open-source [R](https://cran.r-project.org/) package for performing
measurement and verification 2.0 (M&V 2.0)for Commercial Buildings. Aimed to run
locally, the package provides algorithms and a graphical user interface (GUI)
based on [Shiny](https://shiny.rstudio.com/) package. The intent of this package
is to help M&V practitioners to perform with a high level of automation the following:
* Quickly and easily summarize and visualize smart meter data.
* Create energy consumption baseline models using advanced algorithms.
* Screen smart meter data for commercial customers to identify buildings that
are well suited to the automated M&V approach; use this information to target
future whole-building projects.
*  Quantify savings at both a building and portfolio level.
*  Identify potential non-routine events (NRE) in the post-installation period
using statistical methodology.


## Installation

To install **RMV2.0** package the user is required to install:
* [R](https://cran.r-project.org/) version 3.4.2 or later
* A web browser such as Chrome, Firefox or Safari
* [RStudio](https://www.rstudio.com/)
* [devtools](https://cran.r-project.org/web/packages/devtools/index.html) R package

Once R and RStudio are installed, open Rstudio and install the devtools
package by running in the console the following:
```r
install.packages("devtools")
```
Install the **RMV2.0** package using the following command:
```r
devtools::install_github("LBNL-ETA/RMV2.0")
```

## Input Data Format
The **RMV2.0** package requires a specific format for the the csv file that
corresponds to the input data. As an example of this format see bellow:

```r
"time","eload","Temp"
"1/1/14 23:00",24.17,52.6
"1/1/14 23:15",24.95,52.1
```
which correspond to the following table:

| time        | eload | Temp |
| ------------|:-----:| ----:|
| 1/1/14 23:00 | 24.17 | 52.6 |
| 1/1/14 23:15 | 24.95 | 52.1 |

Where "time" is the column of the timestamp, "eload" is the electric load and
"Temp" is the outside air temperature. These three columns are required to
produce a baseline model. If more variables are available, it's possible to add
them to the model by adding them as new columns in the input data files and
provide their names to the algorithm (this will be discussed later).

 Note that within the *RMV2.0/data* folder two example files are provided.

#### Timestamps format

For convenience, an R function is included with the package to convert the actual
timestamps format into "\%m/\%d/\%y \%H:\%M" format. To do so one should use the
timestamp formatting used by the base R function *strptime* to define the format.
For example converting a table that include a time column with the following
timestamp format "2013-08-01 00:00:00" into a table with "1/8/13 00:00" format:

```r
data <- time_format(data,"%Y-%m-%d %H:%M:%S")
```

#### Holidays/Vacations Periods
The RMV2.0 package includes a mechanism for creating an additional variable that
differentiate Holidays and vacation days from the rest of the data. To do so a
csv file need to be provided. The format of this csv file is defined as follow:

```r
date
"2007/7/4"
"2007/9/3"
"2007/10/8"
```
An example corresponding to the US federal holidays is provided in *RMV2.0/inst/extdata* folder.



## RMV2.0 Using graphical user interface (GUI)

To launch the **RMV2.0** GUI in the default the user can use the following
command in Rstudio:
```r
RMV2.0:::runRMV_UI()
```
It is also possible to use the Rstudio
[*Addins*](https://rstudio.github.io/rstudioaddins/) menu.

**A more detailed description of the **RMV2.0** GUI will be published soon.**

## RMV2.0 Using commands

### To generate baseline models

#### GBM model

The *RMV2.0* package has the *gbm_baseline* function that reads the input data
clean it from the missing values and outliers (for more details see the help of
functions *clean_eload* and *clean_Temp*), build a GBM baseline model and return
an gbm_baseline object.
```r
gbm_obj <- gbm_baseline(train_path = "train.csv",
                        pred_path = "pred.csv",
                        days_off_path = "USA_Fed_Holidays.csv",
                        variables = c("Temp","tow")
                        )
```
*train_path* and *pred_path* are the paths of the input data files of
respectively training period and prediction period.  *days_off_path* is the path
of the file that contain the holidays and vacation days. The argument *variables*
correspond to the variables that will be considered by the function as input
variables. In the above example the variables are the temperature (*Temp*) and
the Time Of the Week (*tow*). Note that since the *days_off_path* is provided the
algorithm will add automatically a third variable, which is named *"days_off"*
that correspond to the US federal holidays. If additional variables are
considered the user needs to add the names of these variables to the R vector of
the *variables* argument. For example if data of the solar radiation are
available the user will need to add this data to the training and prediction data
files as new column and name it, for example *solar_rad* then the *variables*
argument needs to be modified into *c("Temp","tow","solar_rad")*. In this
example several parameters of the *gbm_baseline* function are omitted, these
parameters correspond to the set up of the cross-validation involved in the
tuning process of the GBM model. For more details about this parameters the
users are referred to the help of the function (can be accessed by using
*?gbm_baseline* in R consol).


Once the GBM model is built, the user can access to:
* the goodness of fit statistics using the following command:
```r
gbm_obj$goodness_of_fit
```
* to the created xgboost model for the optimal hyper-paramters:
```r
gbm_obj$gbm_model
```
* to the fitted values (training period / pre period):
```r
gbm_obj$fitting
```
* to the predicted values (prediction period / post period):
```r
gbm_obj$prediction
```
* to the training/pre data after the cleaning and filtering functions were
applied:
```r
gbm_obj$train
```
* to the prediction/post data after the cleaning and filtering functions were
applied:
```r
gbm_obj$pred
```
* to the table of all the results of the cross validation:
```r
gbm_obj$gbm_cv_results
```
* and to the optimal combination of the hyper-parameters:
```r
gbm_obj$tuned_parameters
```

#### TOWT model

Similarly to the GBM model, the TOWT model is build using the *towt_baseline*
function that reads the input data clean it from the missing values and outliers
and return an towt_baseline object. Note that the TOWT model only involves the
time of the week and temperature input variables. For more detail about the TOWT
model the user can refer to the
[eetd-loadshape](https://bitbucket.org/berkeleylab/eetd-loadshape) bitbucket
repository. The *towt_baseline* function can be used as follow

```r
towt_obj <- towt_baseline(train_path = "train.csv",
                          pred_path = "pred.csv",
                          days_off_path = "USA_Fed_Holidays.csv")
```
Using the *towt_obj* the user can access to similar data as with *gbm_obj*:
* the goodness of fit statistics using the following command
```r
towt_obj$goodness_of_fit
```
*the TOWT trained model ($towt_model)
```r
towt_obj$towt_model
```
* to the fitted values (training period / pre period):
```r
towt_obj$fitting
```
* to the predicted values (prediction period / post period):
```r
towt_obj$prediction
```
* to the training/pre data after the cleaning and filtering functions were
applied:
```r
towt_obj$train
```
* to the prediction/post data after the cleaning and filtering functions were
applied:
```r
towt_obj$pred
```

### To estimate savings

The savings are estimated by subtracting the actual energy consumption from the
baseline model predicted energy consumption of the post period. Thus, savings and
the corresponding fractional savings can be estimated using the function
*savings* as follow:

```r
savings_obj <- savings(baseline_obj)
```
where *baseline_obj* can be either *gbm_obj* or *towt_obj*.

From the *savings_obj* one can extract:
* the estimated savings
```r
savings_obj$savings
```
* and the corresponding fractional savings
```r
savings_obj$frac_savings
```

### To identify potential non-routine events

The Potential Non-Routine Events are identified using statistical multiple change
points algorithm. Change points are considered to be the points in the time
series where a change in the statistical properties (i.e. change in mean and/or
variance) is observed. This procedure is performed using the *cpt_det* function
as follow

```r
nre_obj <- cpt_det(baseline_obj, interval)
```
where *baseline_obj* can be either *gbm_obj* or *towt_obj*, and *interval* is the
minimum size of the potential non-routine event that can be identified.

The change points indices can be obtained using:

```r
nre_obj$cpts
```
