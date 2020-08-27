library(quantmod)
library(tidyverse)

metricList  <- yahooQF(c("P/E Ratio"))

# Pull in Tickers List
  symbolList     <- read.csv("01 - Data Pulls/ConsDiscTickers.csv") %>%
    select(Ticker)
names(symbolList)

df <- getQuote(Symbols = symbolList,
               src     = "yahoo",
               what    = metricList)
