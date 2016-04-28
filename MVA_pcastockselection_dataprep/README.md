
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MVA_pcastockselection_dataprep** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of Quantlet: MVA_pcastockselection_dataprep
 
Published in: MVA

Description: 'The quantlet reads total return data and constituents for the STOXX EUROPE 600
index from two csv files. A specified time interval is selected and pulled from
the data, afterwards saved as Prices.dat. This file is used later on in quantlet
MVA_pcastockselection_algorithm as input to select stocks.'
  
Keywords: stocks, portfolio, time series, stock selection, index, pca
     
See also: 'MVA_pcastockselection_algorithm, MVA_pcastockselection_results'

Author: Christoph, Schult

Datafile: TimeSeriesData.csv, Constituents.csv
     
Output: A csv file with prices of stocks for a determined time interval.

```

```r
# remove variables
rm(list = ls())

# === input parameters ===

# define root path
sPathRoot = "C:/"  # for MAC "/Users/"
# define file with price data
sReadDataFile = "TimeSeriesData.csv"
# define file with constituents for the portfoilio
sReadConstituents = "Constituents.csv"
# define name of file to save respective prices
sSavePrices = "Prices.dat"
# define column in data file where the dates are stored
iColDates = 1
# define the date format in the dataset
sDateFormat = "%d.%m.%Y"
# define period to look at
sStartDate = "01.04.2006"
sEndDate = "01.04.2016"

# === definition of functions ===

# function to remove missing values from vector
complete = function(x) {
  all(complete.cases(x))
}

# === data processing ===

# set working directory
setwd(sPathRoot)

# load prices
dataprices = read.csv2(sReadDataFile, header = TRUE, dec = ",")
colnames(dataprices) = sub("X", "", colnames(dataprices))
# define dates vector for constituents
iadatesprices = as.Date(dataprices[, iColDates], sDateFormat)

# load constituents lists
dataconst = read.csv2(sReadConstituents, header = FALSE, dec = ",")
iadatesconst = as.Date(dataconst[, iColDates], sDateFormat)

# find relevant interval and constituents
lintervalprices = iadatesprices <= as.Date(sEndDate, sDateFormat) & iadatesprices >= as.Date(sStartDate, sDateFormat)
lintervalconst = iadatesconst <= as.Date(sEndDate, sDateFormat) & iadatesconst >= as.Date(sStartDate, sDateFormat)

# get relevant assets
relassets = unique(c(as.matrix(dataconst[lintervalconst, -iColDates])))
relassets = relassets[is.na(relassets) == FALSE]

# find position of relevant const in data
lrelconst = colnames(dataprices) %in% relassets
dataprices = dataprices[lintervalprices, lrelconst]

# delete assets with missing values
lComplete = apply(dataprices, 2, complete)
dataprices = dataprices[, lComplete]
iadatesprices = iadatesprices[lintervalprices]

# === save data ===

output = list(Dates = iadatesprices, RI = dataprices)
write.table(output, sSavePrices, col.names = TRUE)
```
