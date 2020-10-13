library(BatchGetSymbols)
library(quantmod)
library(tidyverse)
library(skimr)


# Inputs --------------------------------------------------------------------
  startDate    = "2020-01-01"
  endDate      = Sys.Date()

# Select Metrics of Interest -------------------------------------------------

  metricList  <- yahooQF(c("Earnings/Share",
                           "EPS Forward",
                           "Previous Close",
                           "P/E Ratio",
                           "Book Value",
                           "Market Capitalization"))
  

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
      
      # Calculate Book to Market
        mutate(bookToMarket = `Book Value` / `Market Capitalization`)
      
    
    df.output <- df %>%
      
    # Select only Relevent Cols
      select(`Trade Time`  = `Trade Time`,
             `Ticker`      = ticker,
             `Pre-Calculated P/E Ratio`      = old_PE_Ratio,
             `Manually Caculated P/E Ratio`  = calc_PE,
             `Forward P/E Ratio`             = calc_PE_Forward,
             `Book to Market`                = bookToMarket)
    
# Write Excel File ---------------------------------------------------------
    write_excel_csv(df.output, path = "01 - Data Pulls/01 - PE Ratios/02 - Foward PE Ratios - Cons. Disc..csv")
    
    
# Set Theme ----------------------------------------------------------------
    DBC_Theme <- theme(
                      # All Font  Size
                        text                = element_text(size  = 11),
                      
                      # Panel
                        panel.background    = element_rect(fill    = "white", 
                                                           colour  = "white"),
                        panel.border= element_rect(fill    = NA,
                                                   color   = "grey90"),
                        panel.grid.major.x  = element_line(linetype = "solid",
                                                           color   = "grey95",
                                                           size    = 0.2),
                        panel.grid.major.y  = element_line(linetype = "solid",
                                                           color   = "grey90", 
                                                           size    = 0.2),
                        
                      # Main Title
                        plot.title          = element_text(colour  = "grey15",
                                                           size    = 16,
                                                           hjust   = 0),
                        plot.subtitle       = element_text(colour  = "grey15",
                                                           size    = 13,
                                                           hjust   = 0),
                        
                      # Axis Titles
                        axis.text           = element_text(colour  = "grey15",
                                                           size    = 11),
                        axis.title          = element_text(colour  = "grey15",
                                                           size    = 12),
                        
                      # Facet Titles
                        strip.text          = element_text(colour  = "grey15",
                                                           size    = 12),
                        strip.background    = element_rect(fill    = "grey95",
                                                           color   = "grey90",
                                                           size    = .40),
                      
                      # Legend
                        legend.background   = element_rect(fill    = "grey99",
                                                           colour  = "grey85",
                                                           size    = .40),
                        legend.text         = element_text(colour  = "grey15"),
                        legend.position     = "top",
                        legend.title        = element_blank(),
                      
                      # Margin
                        plot.margin         = margin(t = 15, 
                                                     b = 30, 
                                                     r = 30, 
                                                     l = 30, 
                                                     unit = "pt")
                      )
    
    # Set Theme Active
      theme_set(DBC_Theme)
    
# Pull in Historical Data for Graph ----------------------------------------
    
  # Pull in Data
    df.tickers <- BatchGetSymbols(tickers      = c(tickerList), 
                          first.date   = startDate,
                          last.date    = endDate, 
                          freq.data    = 'monthly',
                          cache.folder = file.path(tempdir(), 'BGS_Cache'))
    
    df.tickers <- df.tickers$df.tickers
    
  # Create Graph
    ggplot(data = df.tickers,
           aes(x = ref.date,
               y = price.adjusted,
               color = ticker)) +
      geom_line(alpha = 1/2) +
      theme_get() 
    
    
    
# Notes --------------------------------------------------------------------
  # 1. Could look at book to market ratio = common SE / market cap
    # MI homes - see how they are doing
    # Aarons
    
    
    
    
    
      
