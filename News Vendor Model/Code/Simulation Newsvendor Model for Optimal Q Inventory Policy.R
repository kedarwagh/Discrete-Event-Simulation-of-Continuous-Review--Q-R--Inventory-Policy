#install.packages("ks")
#install.packages("rgl")
#install.packages("xtable")
library(rgl)
library(xtable)
library(ks)

rm(list=ls())

nvd = read.csv("C:/MSTM/Course/Study Material/Supply Chain Analytics/Class 6_Inventory Management/NewsvendorData.csv", header = T)

price = 10
cost = 5
N = 1000

ProfitProfile = c()
for(Q in 100:1000){
  u = runif(1000) #Generate Random probability values
  k = kde(nvd$Demand) #Estimate density of observed demands

  #Convert u and k into random demands
  D = qkde(u, k) #u - pvalue and k - density estimates
  
  Profit = c()
  for(i in 1:N){
    Pr = min(D[i],Q)*(price-cost) - max(0,(Q-D[i]))*cost
    Profit = c(Profit,Pr)
  }
  ExP = mean(Profit) 
  ProfitProfile = c(ProfitProfile,ExP)
}

ind = max(ProfitProfile)
plot(seq(100,1000),ProfitProfile, type = "l")

Q_opt = which(ProfitProfile == max(ProfitProfile))
Q_opt_final = Q_opt+100
Q_opt_final
