r <- 0.016
T <- 4.75
sigma_1 <- 0.0229040592604545*sqrt(250)
sigma_2 <- 0.0126173511106009*sqrt(250)
t_1=4
t_2=2
capital_guarantee <- 0.8
management_fee <- 0.03

payoff_pricing<-function(r,sigma,t){
  d1 <- (r/sigma+sigma/2)*sqrt(t)
  d2 <- d1-sigma*sqrt(t)
  exp(-r*(T-t))*(pnorm(d1)-exp(-r*t)*pnorm(d2))
}

participation_rate_vary_by_r<-c()
r_series<-seq(from=0,to=0.03,length.out=10)

for (i in r_series){
  r <- i
  participation_rate_vary_by_r[length(participation_rate_vary_by_r)+1]<-((1-management_fee)-capital_guarantee*exp(-r*T))/(payoff_pricing(r,sigma_1,t_1)+payoff_pricing(r,sigma_2,t_2)/2)
}
plot(r_series, participation_rate_vary_by_r, main="participation rate", ylab = "participation rate",xlab="interest rate", type = "l", col = "red")

