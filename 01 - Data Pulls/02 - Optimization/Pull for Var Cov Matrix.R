library(tidyverse)
library(BatchGetSymbols)  # Used to Pull in Stock Data

# INPUTS  -------------------------------------------------------------------------------------------------------
  
  # Dates
    selectInterval <- 'weekly' 
    startDate      <- Sys.Date() - 365 * 2
    endDate        <- Sys.Date()
    
  # Stocks
    stockList     <- read_csv("C:/Users/dbry1/OneDrive - University of Oklahoma/1. School/1. University of Oklahoma/4. Senior/FALL 2020/3 - S.I. Fund (FIN-4613-001)/4 - Source Data/ConsDiscTickers.csv")
    
  # Benchmark
    benchmark     <- 'XLY'
    
# DATA PULL -----------------------------------------------------------------------------------------------------
    
    stockList <- c(stockList$Ticker)
    
  # Pull Stock Data ---------------------------------------------------------------------------------------------
                  
    df <- BatchGetSymbols(tickers      = c(benchmark, stockList), 
                          first.date   = startDate,
                          last.date    = endDate, 
                          freq.data    = selectInterval,
                          cache.folder = file.path(tempdir(), 'BGS_Cache'))
    
                  
  # Mutate Data (Drop Cols and Rename) --------------------------------------------------------------------------
    
    df <- as.data.frame(df$df.tickers) %>%
      
      # Change Names of Data and Drop Others
        select(date        = ref.date,
               stockName   = ticker,
               stockReturn = ret.adjusted.prices) %>%
        drop_na()    
    
      # Get Benchmark Returns
        df.benchmark <- df %>%
          filter(stockName == benchmark)
    
      # Omit Benchmark returns from df
        df <- df %>%
          filter(stockName != benchmark) %>%
          
          # Pivot Stock Names to Cols
            pivot_wider(names_from  = stockName,
                        values_from = stockReturn) %>%
          drop_na()
        
      # Set up Excess Returns from Benchmark df(.excess)
        df.excess <-  data.matrix(df %>%
                        select(-date))
                        rownames(df.excess) <- df$date
          
      # Subtract Avg Benchmark Return from Sector Returns
        df.excess <-  df.excess[ , 1:NCOL(df.excess)] - df.benchmark[ , 3:3]
        
                  
  # Var-Cov Matric Calcs -----------------------------------------------------------------------------------------
      
    # Create Variance-Covariance Matrix
      VarCovMatrix  <- (t(df.excess) %*% df.excess) / length(stockList) # Creates Variance Covariance Matrix
        
        
  # Write Return File and Var Cov Matrix -------------------------------------------------------------------------
      
    write.csv(df, file = "01 - Data Pulls/02 - Optimization/Returns - Cons. Disc..csv")
    
    write.csv(VarCovMatrix, file = "01 - Data Pulls/02 - Optimization/Var-Cov Matrix - Cons. Disc..csv")
