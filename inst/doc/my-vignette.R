## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#install.packages("bnma")
#or devtools::install_github("MikeJSeo/bnma")
library(bnma)

## -----------------------------------------------------------------------------
parkinsons

## -----------------------------------------------------------------------------
network <- with(parkinsons, network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, SE = SE, response = "normal", Treat.order = Treat.order))
network$Treat.order 
network$Study.order

## -----------------------------------------------------------------------------
network$r

## -----------------------------------------------------------------------------
network <- with(smoking, network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", mean.d = 0.1, hy.prior = list("dhnorm", 0, 5)))

## -----------------------------------------------------------------------------
result <- network.run(network, n.run = 30000)

## ---- fig.width = 6, fig.height = 6-------------------------------------------
network.forest.plot(result)
# draw.network.graph(network)
# network.autocorr.diag(result)
# network.autocorr.plot(result)
# network.cumrank.tx.plot(result)
# network.deviance.plot(result)
# network.gelman.plot(result)

## -----------------------------------------------------------------------------
network <- with(cardiovascular, network.data(Outcomes, Study, Treat, N, response = "multinomial"))
result <- network.run(network)
summary(result)

## -----------------------------------------------------------------------------
network <- with(statins, network.data(Outcomes, Study, Treat, N=N, response = "binomial", Treat.order = c("Placebo", "Statin"), covariate = covariate, covariate.type = "discrete", covariate.model = "common"))
result <- network.run(network)
summary(result)

## ---- fig.width = 6, fig.height = 6-------------------------------------------
network.covariate.plot(result, base.treatment = "Placebo", comparison.treatment = "Statin")

## -----------------------------------------------------------------------------
network <- with(certolizumab, network.data(Outcomes = Outcomes, Treat = Treat, Study = Study, N = N, response = "binomial", Treat.order = Treat.order, baseline = "common", baseline.risk = "exchangeable"))
result <- network.run(network)
summary(result)

## -----------------------------------------------------------------------------
network <- with(parkinsons_contrast, {
  contrast.network.data(Outcomes, Treat, SE, na, V, type = "random", mean.d = 0.01, prec.d = 0.1, hy.prior = list("dhnorm", 0.01, 0.01))
})
result <- contrast.network.run(network)
summary(result)

## -----------------------------------------------------------------------------
network <- with(smoking, {
  ume.network.data(Outcomes, Study, Treat, N = N, response = "binomial", type = "random")
})
result <- ume.network.run(network)
summary(result)

## -----------------------------------------------------------------------------
network <- with(thrombolytic, nodesplit.network.data(Outcomes, Study, Treat, N, response = "binomial", pair = c(3,9), type = "fixed"))
result <- nodesplit.network.run(network)
summary(result)
