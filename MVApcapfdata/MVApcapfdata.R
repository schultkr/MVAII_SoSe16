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
lintervalprices = iadatesprices <= as.Date(sEndDate, sDateFormat) & iadatesprices >= as.Date(sStartDate, sDateFormat)
lintervalconst  = iadatesconst <= as.Date(sEndDate, sDateFormat) & iadatesconst >= as.Date(sStartDate, sDateFormat)

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