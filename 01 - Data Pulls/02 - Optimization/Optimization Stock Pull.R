library(tidyverse)
library(BatchGetSymbols)  # Used to Pull in Stock Data


# INPUTS  -------------------------------------------------------------------------------------------------------
  startDate     <- Sys.Date() - 365 * 2
  endDate       <- Sys.Date()
  stockList     <- read_csv("C:/Users/dbry1/OneDrive - University of Oklahoma/1. School/1. University of Oklahoma/4. Senior/FALL 2020/3 - S.I. Fund (FIN-4613-001)/4 - Source Data/ConsDiscTickers.csv")
  stockList     <- c(stockList$Ticker)

# DATA PULL -----------------------------------------------------------------------------------------------------
  
  # API Pull
    df <- BatchGetSymbols(tickers = c(stockList), 
                          first.date = startDate,
                          last.date = endDate, 
                          freq.data = 'monthly',
                          cache.folder = file.path(tempdir(), 'BGS_Cache'))
    
  # Adjust Columns
  
    df <- data.frame(df$df.tickers) %>%
      
    # Select date and returns
      select(ticker,
             date        = ref.date,
             adj.returns = ret.adjusted.prices) %>%
      
    # Pivot to Get Stock Names on Cols
      pivot_wider(names_from  = ticker,
                  values_from = adj.returns)

    
# WRITE FILE ----------------------------------------------------------------------------------------------------
    
  fileName <- paste0("01 - Data Pulls/02 - Optimization/Return_Data_CD_through_",
                     startDate,
                     ".csv")
    
  write.csv(df, file = fileName)
  
  
  