library(tidyverse)
library(BatchGetSymbols)  # Used to Pull in Stock Data
library(readxl)

# INPUTS  -------------------------------------------------------------------------------------------------------
  
  # Dates
    selectInterval <- 'monthly' 
    startDate      <- Sys.Date() - 365 * 2
    endDate        <- Sys.Date()
    
  # Stocks
    stockList     <- read_csv("C:/Users/dbry1/OneDrive - University of Oklahoma/1. School/1. University of Oklahoma/4. Senior/FALL 2020/3 - S.I. Fund (FIN-4613-001)/4 - Source Data/ConsDiscTickers.csv")
    
  # Benchmark
    benchmark     <- 'XLY'
    SI.Benchmark  <- '^RUJ'
    
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
      
    write.csv(df, file = "01 - Data Pulls/02 - Optimization/Returns_Monthly - Cons. Disc..csv")
    write.csv(VarCovMatrix, file = "01 - Data Pulls/02 - Optimization/Var-Cov Matrix_Monthly - Cons. Disc..csv")
    
    
  #  GRAPH DATA FROM OPTIMIZED PORTFOLIOS ========================================================================
    
    # Pull in Weights from Optimization
      opt.Monthly.Weights <- read_excel("01 - Data Pulls/02 - Optimization/Optimization - Cons. Disc..xlsx", 
                                sheet = "Analysis", range = "l11:m19")
    
    # Get Stock Names for Pull
      opt.Monthly.Names <-c(opt.Monthly.Weights$`Stock Name`)
      
    # Pull in Data from Yahoo
      df.opt.Monthly <- BatchGetSymbols(tickers      = c(benchmark, SI.Benchmark, opt.Monthly.Names), 
                            first.date   = startDate,
                            last.date    = endDate, 
                            freq.data    = selectInterval,
                            cache.folder = file.path(tempdir(), 'BGS_Cache'))
      
    # Select Necessary Columns for Graph
      df.opt.Monthly <- as.data.frame(df$df.tickers) %>%
      
      # Change Names of Data and Drop Others
        select(date        = ref.date,
               stockName   = ticker,
               stockReturn = ret.adjusted.prices) %>%
        drop_na() %>%
        
      # add weights
        left_join(opt.Monthly.Weights,
                  by = c("stockName" = "Stock Name")) %>%
    
      # multiply weights
        mutate(weightedReturn = if_else(is.na(`Optimal Weight`),
                                        stockReturn,
                                        stockReturn * `Optimal Weight`)) %>%
        select(-stockReturn,
               -`Optimal Weight`) %>%
      
      # Add helper
        mutate(stockGroup = if_else(stockName %in% opt.Monthly.Names, "portfolio",
                                  if_else(stockName == benchmark, " price_benchmark",
                                          "sector_benchmark"))) %>%
      # group by sector and benchmarks
        group_by(date,
                 stockGroup) %>%
        summarise(weightedReturn = sum(weightedReturn))
    
    
    # Set Theme --------------------------------------------------------------------------------
      
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
    
  # Graph Returns ----------------------------------------------------------
    ggplot(df.opt.Monthly,
           aes(x = date,
               y = weightedReturn,
               color = stockGroup,
               linetype = stockGroup))  +
      geom_line(alpha = 1/2) +
      theme_get()
