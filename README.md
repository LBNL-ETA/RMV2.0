# RMV2.0 - LBNL M&V2.0 Tool

RMV2.0 is an open-source [R](https://cran.r-project.org/) package for performing
measurement and verification 2.0 (M&V 2.0)for Commercial Buildings. Aimed to run
locally, the package provides algorithms and a graphical user interface (GUI)
based on [Shiny](https://shiny.rstudio.com/) package. The intent of this package
is to help M&V practitioners to perform with a high level of automation the following:
* Quickly and easily summarize and visualize smart meter data.
* Create energy consumption baseline models using advanced algorithms.
* Screen smart meter data for commercial customers to identify buildings that
are well suited to the automated M&V approach; use this information to target
future whole-building projects.
*  Quantify savings and corresponding at both a building and portfolio level.
*  Identify potential non-routine events (NRE) in the post-installation period
using statistical methodology.


## Installation

To install RMV2.0 package first to install [R](https://cran.r-project.org/)
and [RStudio](https://www.rstudio.com/). Once you R and RStudio are installed,
open Rstudio and install the devtools package by running in the console the
following:
```r
install.packages("devtools")
```
Install the RMV2.0 package using the following command:
```r
devtools::install_github("LBNL-ETA/RMV2.0")
```

## Input Data
The RMV2.0 package requires a specific format for the the csv file that
corresponds to the input data. As an example of this format see bellow:

```r
"time","eload","Temp"
"1/1/14 1:00",24.17,52.6
"1/1/14 1:15",24.95,52.1
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
