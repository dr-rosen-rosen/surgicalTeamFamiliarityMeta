# https://www.metafor-project.org/doku.php/tips:assembling_data_smd

library(metafor)

df <- readxl::read_excel(
  file.path(config$jaxon_data_dir, config$meta_data_file),
  sheet = 'd_metafor'
)

df <- df %>% filter(
  effect_num != 13, # Kenyon duplicated data
  effect_num !=  14, # Kenyon duplicated data
  # effect_num != 15, # Mantoo Surgeon console time but only over a 5 year period.
  author != 'ElBardissi2008'
  )

dat1 <- escalc(measure="SMD", m1i=m1i, sd1i=sd1i, n1i=n1i,
               m2i=m2i, sd2i=sd2i, n2i=n2i, data=df)
res1 <- rma(yi, vi, slab = author, data=dat1)
summary(res1)
forest(res1,
       header = TRUE)
funnel(res1)
