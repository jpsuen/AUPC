#Load Librarys
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(forecast)

## Set WorkingDirectory
setwd('~/Downloads/AUPC/')

## Read Data Sets
cost <- fread("data/ActualCostData.csv")
sales <- fread("data/ActualSalesData.csv")
budget <- fread("data/BudgetSalesData.csv")
customer <- fread("data/Customer.csv")
product <- fread("data/Product.csv")

## Convert Date from Text to Date
cost$Month <- as.Date(paste(substr(cost$Month,5,6),"-01-",substr(cost$Month,0,4),sep=""), "%m-%d-%Y")
sales$Month <- as.Date(paste(substr(sales$Month,5,6),"-01-",substr(sales$Month,0,4),sep=""), "%m-%d-%Y")
budget$Month <- as.Date(paste(substr(budget$Month,5,6),"-01-",substr(budget$Month,0,4),sep=""), "%m-%d-%Y")

# Merge Data
salesProd <- merge(sales, product, by = "ProductCode", all.x = T)
salesProdCust <- merge(salesProd, customer, by = "CustomerCode", all.x = T)
merge1 <- c("CustomerCode", "ProductCode", "Month")
salesProdCustCost <- merge(salesProdCust, cost, by = merge1, all = T)
m <- salesProdCustCost
allMerged <- merge(salesProdCustCost, budget, by = merge1, all =T)

## Merge to look at Budgeted VS Actaul 
allBudgeted <- merge(review, product, by = "ProductCode", all.x = T)

## Messing around with Holt-Winters
##
## First Isolate a single customer
## For each Purchase?
## What are the most frequently sold products??
#
sort(table(m$ProductCode),decreasing=TRUE)[1:3]
## 00049 00042 00029 
## 5901  2650  1545 
## 00049 is most frequent by a lot!!
#
## Who are my most frequent Customers??
sort(table(m$CustomerCode),decreasing=TRUE)[1:3]
##  3201 3207 2803 
##  220  187  174 
## Customer 3201 is pretty good.
#
# Set things up for Customer 3201 to purchase product 00049
n <- filter(m, ProductCode == "00042")
n <- filter(n, CustomerCode == "3207")
n <- select(n, Month, SalesQuantity)
n <- arrange(n, Month)
m1 <- n$Month[1]
m2 <- n$Month[nrow(n)]
m1year <- as.integer(substr(m1,0,4))
m1month <- as.integer(substr(m1,6,7))
# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
# Calculate Number of Months
length <- mondf(m1, m2)
#Create Data Freame with all dates
month <- data.frame(Month=seq(m1, by="1 month", length = length))
# Merge existing and new
n <- merge(data.frame(month),n,all.x=TRUE, by="Month")
# Replace NA with 0
n[is.na(n)] <- 0
# Create Time-Series
ts <- ts(n$SalesQuantity, deltat = 1/12, start=c(m1year,m1month))
#
## Which model should be used in Tableau???
model <- hw(ts, h=5, level=c(80,90))
model <- ets(ts, model="ANN")



## Chart variables in lattice plot color by Division Code
## Set Column colors
cols <- character(nrow(m))
cols[m$DivisionCode == "A"] <- "blue"
cols[m$DivisionCode == "B"] <- "green"
cols[m$DivisionCode == "C"] <- "red"
## Develop Plot
plot <- pairs(~SalesQuantity+GrossValue+NetValue+ProductCost+DistributionCost+SalesCommission, data = m, col=cols)

## Chart variables in lattice plot color by Division Code
cols <- character(nrow(m))
cols[m$RegionCode == "0"] <- "red"
cols[m$RegionCode == "1"] <- "green3"
cols[m$RegionCode == "2"] <- "blue"
cols[m$RegionCode == "3"] <- "cyan"
cols[m$RegionCode == "4"] <- "gray"
cols[m$RegionCode == "5"] <- "yellow"
cols[m$RegionCode == "9"] <- "magenta"
## Develop Plot
plot <- pairs(~SalesQuantity+GrossValue+NetValue+ProductCost+DistributionCost+SalesCommission, data = m, col=cols)

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