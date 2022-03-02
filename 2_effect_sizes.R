library(effectsize)
library(epitools)

# Gfreer2015a
est <- effectsize::oddsratio_to_riskratio(0.403225806, .0293, log = FALSE)
p <- .049
# Gfreer2015a
est <- effectsize::oddsratio_to_riskratio(0.552486188, .0309, log = FALSE)
p <- .119
# Seth2013
est <- effectsize::oddsratio_to_riskratio(1.111111111, 0.183118741, log = FALSE)
p <- .6


ci_from_ratio_and_p(
  est = est,
  p = p
)

# Xiao2015
rr_matrix <- matrix(c(668,1025,74,156),nrow = 2, ncol = 2)
riskratio.wald(x = rr_matrix, rev = "rows")

# Elbardissi2008

d <- d_from_means_sds(
  m1 = 8.6,
  m2 = 22,
  sd1 = 1.6,
  sd2 =  3.1,
  n1 = 17,
  n2 = 14
)
OR <- effectsize::d_to_oddsratio(d, log = FALSE)

# Laheij2002
###### Mortality
# quartile 4
or <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .6, r = .12),
  p0 = .12)
or_l <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .4, r = .12),
  p0 = .12)
or_u <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = 1, r = .12),
  p0 = .12)
cat(or,or_l,or_u)
# quartile 3
or <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .61, r = .12),
  p0 = .12)
or_l <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .4, r = .12),
  p0 = .12)
or_u <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .9, r = .12),
  p0 = .12)
cat(or,or_l,or_u)
# quartile 2
or <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = 1.03, r = .12),
  p0 = .12)
or_l <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .7, r = .12),
  p0 = .12)
or_u <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = 1.4, r = .12),
  p0 = .12)
cat(or,or_l,or_u)
###### Seconary intervention
# quartile 4
or <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .32, r = .21),
  p0 = .21)
or_l <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .2, r = .21),
  p0 = .21)
or_u <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .5, r = .21),
  p0 = .21)
cat(or,or_l,or_u)
# quartile 3
or <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .53, r = .21),
  p0 = .21)
or_l <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .4, r = .21),
  p0 = .21)
or_u <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .7, r = .21),
  p0 = .21)
cat(or,or_l,or_u)

# quartile 2
or <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .8, r = .21),
  p0 = .21)
or_l <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = .6, r = .21),
  p0 = .21)
or_u <- effectsize::riskratio_to_oddsratio(
  RR = rr_from_hr(hr = 1, r = .21),
  p0 = .21)
cat(or,or_l,or_u)