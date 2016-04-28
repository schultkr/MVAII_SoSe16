
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MVA_pcastockselection_results** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of Quantlet: MVA_pcastockselection_results
 
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
![Picture1](portfolioreturns2006to2016.png)
```r

Name of Quantlet: MVA_pcastockselection_results
 
Published in: MVA

Description: ’This quantlet produces plots to illustrate the results of the principal component 
selection process. Stocks are iteratively selected according to their contribution
to principal components. For each portfolio the performance is plotted and the time
series of returns.'

  
Keywords: pca, time series, plot, returns, stock selection, portfolio management 
     
See also: MVA_pcastockselection_algorithm, MVA_pcastockselection_datapreperation

Author: Christoph Schult

Datafile: Portfolios2014to2016.dat, Portfolios2011to2016.dat, Portfolios2006to2016.dat
     
Output: 'Three plots for PCA portfolio returns and values for different time periods. All 
portfolios constructed according to pca (red) are compared to the  portfolio with
all members of the index (blue). The asset universe are constituents in the STOXX
EUROPE 600 index.'
```
