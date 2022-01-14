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

# z = −0.862 + √[0.743 − 2.404×log(P)]
p <- .05


ci_from_ratio_and_p <- function(est, p) {
  # Using methods from here: https://doi.org/10.1136/bmj.d2090
  z <- -0.862 + sqrt(0.743 - 2.404*log(p)) #qnorm(p)
  print(paste('z:',z))
  est <- log(est) # put estimate on the log scale
  print(paste('est:',est))
  se <- abs(est / z)
  print(paste('se:',se))
  lower <- exp(est - (1.96*se)) # taking anti-log
  upper <- exp(est + (1.96*se))
  # lower <- est - (1.96*se) # taking anti-log
  # upper <- est + (1.96*se)
  print(lower)
  print(upper)
}