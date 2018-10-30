rm(list=ls())

getwd()
setwd('//MGPCTFSW01/UserFolders$/t103354/Desktop/BDAA case competition')

#first goal: create one big complete dataset to do EDA on. So don't have to deal with pesky consumer ID etc.

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

regionalManagers=read.csv("regionalManagers.csv", header = TRUE)
Orders = read.csv("orders.csv", header = TRUE)
customers = read.csv("customers.csv", header = TRUE)
products = read.csv("product.csv", header = TRUE)


#Used manual processes to combine all of these into 1 dataset. So want to create combined file now and save to access later

customerOrder <- merge(customers, Orders, by=c("CustomerID"))

customerOrderProduct <- merge(customerOrder, products, "ProductID")

customerOrderProductRegionalManagers <- merge(customerOrderProduct, regionalManagers, "Region")

finalData = customerOrderProductRegionalManagers

#need to factor qualitative data
attach(finalData)

finalData$Region = factor(Region)
finalData$Province = factor(Province)
finalData$Customer.Segment = factor(Customer.Segment)
finalData$Order.Priority = factor(Order.Priority)
finalData$Ship.Mode = factor(Ship.Mode)
finalData$Product.Category = factor(Product.Category)
finalData$Product.Sub.Category = factor(Product.Sub.Category)
finalData$Product.Container = factor(Product.Container)
finalData$Regional.Manager = factor(Regional.Manager)

#there were a few more qualitative varibles but I chose not to include them because I won't use them in my analysis anyhow

#add profit per unit category
finalData$Unit.Profit = Unit.Price-Unit.Cost

#seems like each order is only for 1 item
#want to add total profit as well since we should predict on that
finalData$Profit = Unit.Profit*Order.Quantity

plot(Order.Quantity,Profit)


#do one way EDA for all intersted variables and note results

#seems that region by itself does not matter
#same for province
#same for customer segment,order priority
#even looking at unit.Price, the fit doesn't look linear to me.

#essentially NOTHING relates linearly to Profit, which isn't good. Time to abandon this method

a = lm(Profit~Unit.Price+Discount,data=finalData)
summary(a)
plot(a)

#LOOKING at the residual plots the major problem seems to be that most of our orders are for small amounts of items
#but a few of the orders are for large item orders, which really messes up all of our graphs. 





