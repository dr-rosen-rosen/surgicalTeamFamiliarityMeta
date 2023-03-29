####################################################################################################
####################################################################################################
################################ Main analysis file
################################ for OR familiarity meta-analysis
####################################################################################################
####################################################################################################library(metacor)
library(here)
library(config)
library(tidyverse)
library(tidyselect)
library(effectsize)
library(qrcode)

debuggingState(on = FALSE)
Sys.setenv(R_CONFIG_ACTIVE = "mike")
config <- config::get()
source(here("1_funcs.R"), echo = TRUE)


qrcode::qr_code(
  'https://osf.io/2t4yv/',
  ecl = "H") %>%
  plot()


# m.cor <- do_metacor(
#   f = file.path(here(),config$meta_data_dir, config$meta_data_file),
#   sheet = 'cor'
# )
# summary.meta(m.cor)
# forest.meta(m.cor)
# funnel.meta(m.cor)
# 
# m.gen <- do_mgen(
#   f = file.path(here(),config$meta_data_dir, config$meta_data_file),
#   sheet = 'rr'
# )
# 
# summary.meta(m.gen)
# forest.meta(m.gen)
# funnel.meta(m.gen)