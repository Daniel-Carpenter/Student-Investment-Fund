library(quantmod)
library(tidyverse)


# Select Metrics of Interest -----------------------------------------------
  metricList  <- yahooQF(c("Earnings/Share",
                           "Previous Close",
                           "P/E Ratio"))
  

# Get Tickers List ---------------------------------------------------------
  
  # Pull in Tickers List
    tickerList <- read.csv("01 - Data Pulls/ConsDiscTickers.csv") %>%
      select(Ticker)
    
  # Convert tickerList to `List`
    tickerList <- tickerList$Ticker


# Pull in Data from Yahoo Finance ------------------------------------------
    
  # Data Pull
    df <- getQuote(Symbols = tickerList,
                   src     = "yahoo",
                   what    = metricList) 
      
  # Add Stock Names
    df$ticker   = rownames(df)
    
  # Reorder Cols
    df <- df %>% 
      select(ticker,
             "Earnings/Share",
             "P. Close",
             old_PE_Ratio = "P/E Ratio") %>%
      mutate(calculated_PE_Ratio = `P. Close` / `Earnings/Share`)
    
    
# Write Excel File
    write_excel_csv(df, path = "01 - Data Pulls/Cons_Disc_Upload_File.csv")
      
