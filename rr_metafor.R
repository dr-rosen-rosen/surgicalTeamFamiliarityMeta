library(metafor)

df <- readxl::read_excel(
  file.path(here(),config$meta_data_dir, config$meta_data_file),
  sheet = 'or_metafor'
)

# following https://www.metafor-project.org/doku.php/tips:assembling_data_or

# this uses 2 x 2 data if there is any (there isn't)
df <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=df)
# For the studies reporting the odds ratios directly, 
# we can easily transform these values to log odds ratios with
df$yi <- replmiss(df$yi, log(df$or))
# The p-values can be converted into the corresponding z-values with:
df$zi <- sign(df$yi) * qnorm(df$pval/2, lower.tail=FALSE)
# Together with the log odds ratios, the test statistics can be converted into the corresponding standard errors with
df$sei <- df$yi / df$zi
# The confidence interval bounds can be converted into the standard errors with:
df$sei <- replmiss(df$sei, with(df, (log(ci.ub) - log(ci.lb))/(2*1.96)))
# Finally, any missing values for the vi variable (the sampling variances) can now be replaced with:
df$vi <- replmiss(df$vi, df$sei^2)
# Finally, since we won't need these variables any further, we can remove the test statistics and standard errors with
df$zi <- df$sei <- NULL

# res <- rma(yi,vi,data = df)
# predict(res, transf = exp, digits = 2)
# summary(res)
# forest(res,
#        atransf=exp)
# funnel(res)
# funnel(trimfill(res))
res2 <- rma.mv(yi = yi, V = vi, data = df,
              slab = author,
              # mods = ~ outcome_category,
              random = ~ 1 | author / effect_num,
              test = 't',
              method = 'REML')
summary(res2)
predict(res2, transf = exp, digits = 2)
forest(res2)
forest(res2,
        atransf=exp,
       header = TRUE,
       # ilab = df$outcome_category,
       # ilab.xpos = -2.5,
       ylim=c(-1, 38),
       xlim=c(-5.2, 2),
       cex=0.75,
       order = df$outcome_category,
       rows=c(3:7,12:16,21:23,28:33),
       main = '(Preliminary)\nEffect of team familiarity on surgical outcomes',
       mlab=mlabfun("RE Model for All Studies", res2),
       psize=1, 
       # header="Author(s) and Year"
       )
### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.75, font=2)
# text(-2.4,21,'Outcome type')
### switch to bold italic font
par(font=4)

### add text for the subgroups
text(-5.2, c(8,17,24,34), pos=4, c("Adverse events",
                               "Complications",
                               "Mortality",
                              "Utilization"))
### fit random-effects model in the three subgroups
res.a <- rma(yi, vi, subset=(outcome_category=="adverse_event"), data=df)
res.c <- rma(yi, vi, subset=(outcome_category=="complications"),     data=df)
res.m <- rma(yi, vi, subset=(outcome_category=="mortality"),  data=df)
res.u <- rma(yi, vi, subset=(outcome_category=="utilization"),  data=df)

### add summary polygons for the three subgroups
addpoly(res.a, row=2, cex = .75, mlab=mlabfun("RE Model for Subgroup", res.a))
addpoly(res.c, row= 11, cex = .75, mlab=mlabfun("RE Model for Subgroup", res.c))
addpoly(res.m, row= 20, cex = .75, mlab=mlabfun("RE Model for Subgroup", res.m))
addpoly(res.u, row= 27, cex = .75, mlab=mlabfun("RE Model for Subgroup", res.u))

### fit meta-regression model to test for subgroup differences
res <- rma.mv(yi, vi, 
           mods = ~ outcome_category,
           random = ~ 1 | author / effect_num,
           data=df)

### add text for the test of subgroup differences
text(-5.2, -1, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
                                              Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                              ", p = ", .(formatC(res$QMp, digits=2, format="f")))))

funnel(res2,
       atransf=exp)
# funnel(trimfill(res2))


