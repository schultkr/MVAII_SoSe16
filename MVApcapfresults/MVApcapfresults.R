# === input parameters ===
rm(list = ls())
# define paths and filenames
sPathRoot = "C:/"  # for MAC '/Users/'
sReadDataFile = c("Portfolios2014to2016.dat", "Portfolios2011to2016.dat", "Portfolios2006to2016.dat")
# define auxiliary parameters for the data
sDateFormat = "%d.%m.%Y"
# define returns
sTypeReturns = "grossreturn"
iStepSize    = 1

# set working directory
setwd(sPathRoot)

# function to compute returns for a vector
returnsfun = function(x) {
  n = length(x)
  switch(sTypeReturns, grossreturn = (x[(iStepSize + 1):n] - x[1:(n - iStepSize)])/x[1:(n - iStepSize)], logreturn = diff(log(x), 
                                                                                                                          iStepSize))
}


# === general graphical parameters ===
plotportfolios = function(sFileName) {
  
  # open new device
  #dev.new()
  # defines general plot characteristics
  par(las = 1, font = 1, font.axis = 1, font.main = 2, mfrow = c(2, 1), mai = c(0.5, 1, 0.5, 0.1))
  
  # Determine which frequency for x axis should be displayed
  sDateFormat = "%Y-%m-%d"
  sFreq       = "%Y"
  
  # === read portfolios ===
  input        = read.table(sFileName)
  # define date vector
  iadates      = input$Dates
  # define original portfolio
  pfallassets  = input$PFallassets
  # returns original portfolios
  retallassets = returnsfun(pfallassets)
  # define portfolio constructed by pca
  pfoptassets  = input$PFoptassets
  # returns optimal portfolios
  retoptassets = returnsfun(pfoptassets)
  
  # define main titles
  sEndDate     = as.character(iadates[length(iadates)])
  sStartDate   = as.character(iadates[1])
  sMain  = paste("Portfolio Values and Returns \n", sStartDate, "to", sEndDate, sep = " ")
  
  # define lim for x and y
  xlimtime   = c(1, length(iadates))
  ylimvalues = c(min(c(pfallassets, pfoptassets)) - 0.2, max(c(pfallassets, pfoptassets)))
  ylimret    = c(min(c(retallassets, retoptassets)), max(c(retallassets, retoptassets)))
  # === create plot for portfolio values ===
  
  # creating labels for x axis
  xlabelshelp = format(as.Date(iadates, sDateFormat), sFreq)
  xlabels     = unique(xlabelshelp)
  createticks = function(x) {
    y = min(which(x == xlabelshelp))
  }
  xtickpos    = as.numeric(sapply(xlabels, createticks))
  
  # create plot
  plot(pfallassets, type = "l", lwd = 3, lty = 1, col = "darkblue",
       frame = FALSE, axes = FALSE, xlab = "", ylab = "Portfolio values", 
       main = sMain, ylim = ylimvalues, xlim = xlimtime)
  # add values for portfolio with selected consts according to pca
  lines(pfoptassets, lwd = 3, lty = 2, col = "darkred")
  # add y axis
  axis(side = 2, lwd = 0.5)
  
  # === plot returns === create plot for returns
  plot(retallassets, type = "l", lwd = 3, lty = 1, col = "darkblue",
       frame = FALSE, axes = FALSE, xlab = "Date", ylab = "Returns", 
       main = "", ylim = ylimret, xlim = xlimtime)
  lines(retoptassets, lwd = 3, lty = 2, col = "darkred")
  
  # Format the y axis
  axis(side = 2, lwd = 0.5)
  # Format the x axis
  axis(side = 1, at = xtickpos, label = xlabels, lwd = 0.5)
}

# create plots
lapply(sReadDataFile, plotportfolios)