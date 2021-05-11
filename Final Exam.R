r <- 0.016
T <- 4.75
sigma <- 0.0229040592604545*sqrt(250)
capital_guarantee <- 80%
payoff_pricing<-function(t){
  d1 <- (r/sigma+sigma/2)*sqrt(t)
  d2 <- d1-sigma*
}