library(effectsize)

# Gfreer2015a
est <- effectsize::oddsratio_to_riskratio(0.403225806, .0293, log = FALSE)
p <- .049
# Gfreer2015a
est <- effectsize::oddsratio_to_riskratio(0.552486188, .0309, log = FALSE)
p <- .119
ci_from_ratio_and_p(
  est = est,
  p = p
)