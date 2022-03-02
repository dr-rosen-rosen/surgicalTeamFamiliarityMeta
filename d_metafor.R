# https://www.metafor-project.org/doku.php/tips:assembling_data_smd

library(metafor)

df <- readxl::read_excel(
  file.path(here(),config$meta_data_dir, config$meta_data_file),
  sheet = 'd_metafor'
)

dat1 <- escalc(measure="SMD", m1i=m1i, sd1i=sd1i, n1i=n1i,
               m2i=m2i, sd2i=sd2i, n2i=n2i, data=df)
res1 <- rma(yi, vi, slab = author, data=dat1)
summary(res1)
forest(res1,
       header = TRUE)
