#Inventory Management

#Demand in a day : mean 1,200, standard deviation
#Replenishment period : 7 days (1 week)
#(Q1) How many is the safety stock to ensure service level 97.5%
#(Q2) How many is the safety stock to ensure service level 99.9%

#(Q1)
qnorm(0.975,0,sqrt(7)*45)
#(Q2)
qnorm(0.99,0,sqrt(7)*45)