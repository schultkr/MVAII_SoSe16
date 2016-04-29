
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MVApcapfalgo** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml


Name of Quantlet: MVApcapfalgo
 
Published in: MVA

Description: 'This quantlet applies the selection algorithm based on principal component
analysis to identify stocks, which have contributed heavily to the movement in the
original portfolio. In each step the algorithm applies pca to the correlation matrix of
stocks not removed yet. Eigenvectors are selected associated with eigenvalues smaller
than one. Stocks with the highest absolute weight in these eigenvectors are removed.
This step is repeated until either the number of remaining stocks reaches a predefined
threshold or the remaining eigenvalues are sufficiently close to each other. 
Furthermore the algorithm is applied for different scale parameter values.'

Keywords: pca, returns, stock selection, portfolio management, eigenvalues, eigenvectors
     
See also: MVApcapfresults, MVApcapfdata

Author: Christoph Schult

Datafile: Prices.dat
     
Output: 'Two plots illustrating how the result of the algorithm depends on the
selected scale parameter. Furthermore based on the selected assets an equal 
weighted portfolio is contracted and saved in a .dat file'

```

![Picture1](AlgoResults.png)

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
sEndDate   = "01.04.2016"

# === definition of functions ===

# function to remove missing values from vector
complete = function(x) {
  all(complete.cases(x))
}

# === data processing ===

# set working directory
setwd(sPathRoot)

# load prices
dataprices           = read.csv2(sReadDataFile, header = TRUE, dec = ",")
colnames(dataprices) = sub("X", "", colnames(dataprices))
iadatesprices        = as.Date(dataprices[, iColDates], sDateFormat)

# load constituents lists
dataconst    = read.csv2(sReadConstituents, header = FALSE, dec = ",")
iadatesconst = as.Date(dataconst[, iColDates], sDateFormat)

# find relevant interval and constituents
iStartDate      = as.Date(sEndDate, sDateFormat)
iEndDate        = as.Date(sStartDate, sDateFormat)
lintervalprices = iadatesprices <= iEndDate & iadatesprices >= iStartDate
lintervalconst  = iadatesconst <= iEndDate & iadatesconst >= iStartDate

# get relevant assets
relassets = unique(c(as.matrix(dataconst[lintervalconst, -iColDates])))
relassets = relassets[is.na(relassets) == FALSE]

# find position of relevant const in data
lrelconst  = colnames(dataprices) %in% relassets
dataprices = dataprices[lintervalprices, lrelconst]

# delete assets with missing values
lComplete     = apply(dataprices, 2, complete)
dataprices    = dataprices[, lComplete]
iadatesprices = iadatesprices[lintervalprices]

# === save data ===

output = list(Dates = iadatesprices, RI = dataprices)
write.table(output, sSavePrices, col.names = TRUE)

```
