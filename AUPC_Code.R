#Load Librarys
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
#library(zoo)

## Set WorkingDirectory
setwd('~/Downloads/AUPC/')

## Read Data Sets
cost <- fread("ActualCostData.csv")
sales <- fread("ActualSalesData.csv")
budget <- fread("BudgetSalesData.csv")
customer <- fread("Customer.csv")
product <- fread("Product.csv")

## Convert Date from Text to Date
cost$Month <- as.Date(paste(substr(cost$Month,5,6),"-01-",substr(cost$Month,0,4),sep=""), "%m-%d-%Y")
sales$Month <- as.Date(paste(substr(sales$Month,5,6),"-01-",substr(sales$Month,0,4),sep=""), "%m-%d-%Y")
budget$Month <- as.Date(paste(substr(budget$Month,5,6),"-01-",substr(budget$Month,0,4),sep=""), "%m-%d-%Y")

# Merge Data
salesProd <- merge(sales, product, by = "ProductCode", all.x = T)
salesProdCust <- merge(salesProd, customer, by = "CustomerCode", all.x = T)
merge1 <- c("CustomerCode", "ProductCode", "Month")
salesProdCustCost <- merge(salesProdCust, cost, by = merge1, all = T)
allMerged <- merge(salesProdCustCost, budget, by = merge1, all =T)

## Merge to look at Budgeted VS Actaul 
allBudgeted <- merge(review, product, by = "ProductCode", all.x = T)

## Factor out Categorical Data


# Chart out Distribution of Product Code
salesProd %>% 
  count(SalesQuantity, sort = TRUE) %>%
  filter(n > 100) %>%
  ggplot(aes (x = reorder(SalesQuantity,n), y = n)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  ggtitle("Distribution of Product Code")

# Types of Products colored by Region
allMerged %>%
  na.omit() %>%
  ggplot(aes(factor(DivisionDesc), fill = SalespersonDesc)) + geom_bar()

# Time series
salesProdCustCost %>%
  ggplot(aes(x = Month, y = SalesQuantity, color = DivisionDesc)) + geom_line()

# Sales People by Products sold
allMerged %>%
  na.omit() %>%
  arrange(DivisionDesc, SalespersonDesc) %>%
  ggplot(aes(factor(SalespersonDesc), NetValue, fill = DivisionDesc)) + geom_bar(stat="identity")

# Sales People by Products sold
allMerged %>%
  na.omit() %>%
  arrange(ZoneDesc, SalespersonDesc) %>%
  ggplot(aes(factor(SalespersonDesc), NetValue, fill = ZoneDesc)) + geom_bar(stat="identity")

# Plot Sales over time
sales %>%
  arrange(Month) %>%
  ggplot(aes(factor(Month), SalesQuantity)) + geom_point()

#What are those 3 outliers from the sales data?? 
sales %>%
  filter(SalesQuantity > 15000)