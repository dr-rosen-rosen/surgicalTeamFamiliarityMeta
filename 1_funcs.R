####################################################################################################
####################################################################################################
################################ Analysis scripts
################################ for OR team Familiarity meta
####################################################################################################
####################################################################################################

library(meta)
library(readxl)

# Do meta on r effects
do_metacor <- function(f, sheet) {
  
  df_meta <- read_xlsx(f, sheet = sheet)
  #df_meta <- df_meta[!(df_meta$Author %in% c('Maruthappu2016')),] # Maruthappu2016 uses GEE, and I don't think this can be combined with linear regression
  
  m.cor <- metacor(cor, 
                   n, 
                   data = df_meta,
                   studlab = df_meta$Author,
                   sm = "ZCOR",
                   method.tau = "SJ")
  return(m.cor)
}

# do meta on rr effects
do_mgen <- function(f, sheet) {
  
  df_meta <-read_xlsx(f, sheet = sheet)
  df_meta$rr <- log(df_meta$rr)
  df_meta$lower <- log(df_meta$lower)
  df_meta$upper <- log(df_meta$upper)
  df_meta$seTE <- (df_meta$upper - df_meta$lower)/3.92
  
  m.gen <- metagen(rr, 
                   seTE, 
                   studlab = Author,
                   method.tau = "SJ",
                   sm = "RR",
                   data = df_meta)
  return(m.gen)
}

