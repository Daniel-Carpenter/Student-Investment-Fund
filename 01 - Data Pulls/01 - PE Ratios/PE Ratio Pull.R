library(quantmod)
library(tidyverse)


# Select Metrics of Interest -------------------------------------------------

  metricList  <- yahooQF(c("Earnings/Share",
                           "EPS Forward",
                           "Previous Close",
                           "P/E Ratio"))
  

# Get Tickers List -----------------------------------------------------------
  
  # Pull in Tickers List
    tickerList <- read.csv("01 - Data Pulls/ConsDiscTickers.csv") %>%
      select(Ticker)
    
  # Convert tickerList to `List`
    tickerList <- tickerList$Ticker


# Pull in Data from Yahoo Finance --------------------------------------------
    
  # Data Pull
    df <- getQuote(Symbols = tickerList,
                   src     = "yahoo",
                   what    = metricList) 
      
  # Add Stock Names
    df$ticker   = rownames(df)
    
  # Calculate PE Ratios to overcome Null Values ----------------------------
    
    df <- df %>%
      
      # Yahoo Finance Given PE Ratio (with NA's for Negative Values)
        mutate(old_PE_Ratio    = `P/E Ratio`) %>%
      
      # Calculated PE Ratio with Regular EPS
        mutate(calc_PE         = `P. Close` / `Earnings/Share`) %>%
      
      # Calculated PE Ratio with FORWARD EPS
        mutate(calc_PE_Forward = `P. Close` / `EPS Forward`) %>%
      
    # Select only Relevent Cols
      select(tradeTime = `Trade Time`,
             old_PE_Ratio,
             calc_PE,
             calc_PE_Forward)
    
    
# Write Excel File ---------------------------------------------------------
    write_excel_csv(df, path = "01 - Data Pulls/Cons_Disc_Upload_File.csv")
      
