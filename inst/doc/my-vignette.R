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
#blocker (binomial)
#cardiovascular (multinomial)
#certolizumab (binomial with a covariate)
#parkinsons (normal)
#parkinsons_contrast (normal- for contrast model)
#smoking (binomial)
#statins (binomial with a covariate)
#thrombolytic (binomial)

## -----------------------------------------------------------------------------
network <- with(smoking, network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", mean.d = 0.1, hy.prior = list("dhnorm", 0, 5)))

## -----------------------------------------------------------------------------
result <- network.run(network, n.run = 30000)

## ---- fig.height=10, fig.width=10, out.height=600, out.width=600--------------
network.forest.plot(result, label.margin = 15)
# relative.effects.table(result)
# draw.network.graph(network)
# network.gelman.plot(result)
# network.autocorr.diag(result)
# network.autocorr.plot(result)
# network.rank.tx.plot(result)
# network.cumrank.tx.plot(result)
# sucra(result)
# network.deviance.plot(result)
# network.leverage.plot(result)

## -----------------------------------------------------------------------------
network <- with(cardiovascular, network.data(Outcomes, Study, Treat, N, response = "multinomial"))
result <- network.run(network)
summary(result)

## -----------------------------------------------------------------------------
network <- with(statins, network.data(Outcomes, Study, Treat, N=N, response = "binomial", Treat.order = c("Placebo", "Statin"), covariate = covariate, covariate.type = "discrete", covariate.model = "common"))
result <- network.run(network)
summary(result)

## ---- fig.height=10, fig.width=10, out.height=600, out.width=600--------------
network.covariate.plot(result, base.treatment = "Placebo", comparison.treatment = "Statin")

## -----------------------------------------------------------------------------
network <- with(certolizumab, network.data(Outcomes = Outcomes, Treat = Treat, Study = Study, N = N, response = "binomial", Treat.order = Treat.order, baseline = "common", baseline.risk = "exchangeable"))
result <- network.run(network)
summary(result)

## -----------------------------------------------------------------------------
# fit an inconsistency model (UME)
network2 <- with(smoking, {
  ume.network.data(Outcomes, Study, Treat, N = N, response = "binomial", type = "random")
})
result2 <- ume.network.run(network2)
summary(result2)

# fit a consistency model and compare posterior mean deviance in the consistency model and inconsistency model
network1 <- with(smoking, {
  network.data(Outcomes, Study, Treat, N = N, response = "binomial", type = "random")
})
result1 <- network.run(network1)

## ---- fig.height=10, fig.width=10, out.height=600, out.width=600--------------
network.inconsistency.plot(result1, result2)

## -----------------------------------------------------------------------------
network <- with(thrombolytic, nodesplit.network.data(Outcomes, Study, Treat, N, response = "binomial", pair = c(3,9), type = "fixed"))
result <- nodesplit.network.run(network)
summary(result)

## -----------------------------------------------------------------------------
#Using metaprop function from meta package, we meta-analyze placebo event proportion.
#library(meta)
#placebo_index <- which(certolizumab$Treat == "Placebo")
#meta.pla <- metaprop(event = certolizumab$Outcomes[placebo_index], n = certolizumab$N[placebo_index], method = "GLMM", sm = "PLOGIT")
#mean.A = meta.pla$TE.random; prec.A = 1/meta.pla$tau^2

network <- with(certolizumab, network.data(Outcomes = Outcomes, Treat = Treat, Study = Study, N = N, response = "binomial", mean.A = -2.27, prec.A = 2.53))
result <- network.run(network)
#summary(result, extra.pars = c("RD", "RR", "NNT"))
summary(result, extra.pars = c("RR"))

## -----------------------------------------------------------------------------
set.seed(1234) # seed for generating reproducible initial values
network <- with(blocker, network.data(Outcomes = Outcomes, Treat = Treat, Study = Study, N = N, response = "binomial"))

# JAGS RNG list of initial values
jags_inits <- list(
  list(".RNG.name"="base::Wichmann-Hill", ".RNG.seed" = 94387),
  list(".RNG.name"="base::Wichmann-Hill", ".RNG.seed" = 24507),
  list(".RNG.name"="base::Wichmann-Hill", ".RNG.seed" = 39483)
)
result <- network.run(network, n.chains=3, RNG.inits=jags_inits)

# bnma initial values now contain initial values for the parameters and the JAGS RNG initial values
str(result$inits)

# reproducible results
summary(result)

