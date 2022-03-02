library(metafor)

df <- readxl::read_excel(
  file.path(here(),config$meta_data_dir, config$meta_data_file),
  sheet = 'beta_metafor'
)

df <- df %>%
  filter(effect_num != 21 & effect_num != 26 & effect_num != 38) # This is a crazy outlier for sampling variance.

### Standardize unstandarzied coefficients and CIs
df$beta <- replmiss(df$beta, with(df, (df$unstandardized_coefs*(df$SD_x/df$SD_y))))
df$ci.lb <- replmiss(df$ci.lb, with(df, (df$ci.lb*(df$SD_x/df$SD_y))))
df$ci.ub <- replmiss(df$ci.ub, with(df, (df$ci.ub*(df$SD_x/df$SD_y))))

### Just using beta's as the effect size
df$yi <- df$beta

# using reported standard errors for sampling variances where possible
# The confidence interval bounds can be converted into the standard errors with:
df$se <- replmiss(df$se, with(df, (ci.ub - ci.lb)/(2*1.96)))
# using p values to get standard errors (betas use a t value, not z like ORs)
df$ti <- sign(df$yi) * qt(df$pval/2, df = df$resid_df)
df$se <- replmiss(df$se, df$yi / df$ti)

# calculate sampling variances
df$vi <- replmiss(df$vi, df$se^2)

# Finally, since we won't need these variables any further, we can remove the test statistics and standard errors with
df$ti <- NULL


res2 <- rma.mv(yi = yi, V = vi, data = df,
               slab = author,
               mods = ~ familiarity_level + outcome,
               random = ~ 1 | author / effect_num,
               test = 't',
               method = 'REML')
summary(res2)
predict(res2, 
        transf = exp, 
        digits = 2)
forest(res2,
       main = '(Preliminary)\nEffects of team familiarity on utilization outcomes',
       header = TRUE,
       ilab = df$familiarity_level,
       ilab.xpos = -.5,
       order = df$familiarity_level,
       addfit = TRUE,
       addpred = TRUE)
# radial(res2)
funnel(res2)
# plot(gosh(res2))
# funnel(trimfill(res2))
