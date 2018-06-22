#install.packages("ks")
#install.packages("rgl")
#install.packages("xtable")
#install.packages("rgl")
library(rgl)
library(xtable)
library(ks)
rm(list=ls())

d1 = read.csv("C:/MSTM/Course/Study Material/Supply Chain Analytics/Class 6_Inventory Management/NewsvendorData.csv", header = T)
d2 = read.csv("C:/MSTM/Course/Study Material/Supply Chain Analytics/Class 6_Inventory Management/DeliveryLeadTimeData.csv", header = T)

#Create a function to compute the expected cost
CostServiceLevel = function(Q,R,D,L){
  Cost = 0
  Miss = 0
  Inventory = Q
  Order = FALSE
  Lead = 0
  OrderNumber = 0
  CummInventory = 0
  h = 0.01
  K = 1000
  
  InventoryProfile <- c()
  
  for(i in 1:length(D)){
    if((Order == TRUE) & (Lead > 0)){
      Lead = Lead - 1
    }
    
    if((Order == TRUE) & (Lead == 0)){
      Inventory = Inventory + Q
      Order = FALSE
    }
    
    if(Inventory >= D[i]){
      Inventory = Inventory - D[i]
    }
    else if((Inventory >= 0) & (Inventory < D[i])){
      Inventory = Inventory - Inventory
      Miss = Miss + (D[i] - Inventory)
    }
    else{
      Inventory = 0
    }
    
    if((Inventory <= R) & (Order == FALSE)){
      Order = TRUE
      Lead = sample(L, size = 1, replace = TRUE)
      OrderNumber = OrderNumber + 1
    }
    
    CummInventory = CummInventory + Inventory
    InventoryProfile <- c(InventoryProfile, Inventory)
    #i = i + 1
    InventoryProfile
  }
  Cost = CummInventory*h + K*OrderNumber
  ServiceLevel = (sum(D) - Miss)/sum(D)
  
  return(list(COST=Cost, SERVICELEVEL = ServiceLevel, INVENTORY_PROFILE = InventoryProfile))
}

Q = seq(2500, 15500, 500) #Create a vector of Ordered Quantity
R = seq(500, 8500, 200) #Create a vector of Reorder point

CostLevel = matrix(nrow=length(Q), ncol=length(R))
ServiceLevel = matrix(nrow=length(Q), ncol=length(R))

#Bootstrapping
for(i in 1:length(Q)){
  for(j in 1:length(R)){
    Output = CostServiceLevel(Q[i], R[j], d1$Demand, d2$LeadTime)
    CostLevel[i,j] = Output$COST
    ServiceLevel[i,j] = Output$SERVICELEVEL
}
}

ind1 = which(ServiceLevel == 1, arr.ind = T)
ind2 = which(CostLevel == min(CostLevel[ind1]), arr.ind = T)

#Optimal Order (Q,R) policy at Service Level 0.95
Q[ind2[1,1]]
R[ind2[1,2]]

#Visualise
persp3d(Q, R, CostLevel ,color = 4, alpha = 0.7, back ="lines");
persp3d(Q, R,  ServiceLevel, color = 2, alpha = 0.7, back ="lines");
image(Q, R, CostLevel )
contour(Q, R, CostLevel, add=TRUE)
image(Q, R, ServiceLevel )
contour(Q, R, ServiceLevel, add=TRUE)

#Running with the Optimal (Q, R) Policy
Output1 = CostServiceLevel(Q[ind2[1,1]], R[ind2[1,2]], d1$Demand, d2$LeadTime)
plot(seq(1, length(d1$Demand)), Output1$INVENTORY_PROFILE, type = "l", col = 4, ylim = c(0, 11000))
legend("topright", legend = c("Inventory", "Demand"), lty = c(1, 1), col = c(4, 2));
lines(seq(1, length(d1$Demand)), d1$Demand, col = 2);
