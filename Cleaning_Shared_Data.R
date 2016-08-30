#
# This is data that was merged by Brandon and Q/A'd against the TA's dataset.
# This script is to document cleaning proceedures
#
# Fixing the issue of multi store names
#Load Librarys
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(forecast)

## Set WorkingDirectory
setwd('C:/Users/joshuasuen/Downloads/AUPC-master/AUPC-master')

## Read Data Set
m <- fread("data/Merged_Brandon.csv")

#Function To Create Company Division Code
subRight <- function(x){
  n1 <- substr(x, nchar(x)-0, nchar(x))
  n2 <- substr(x, nchar(x)-1, nchar(x))
  if (grepl("[^0-9]",n2) == FALSE) {
    n2
  } else if (grepl("[^0-9]",n1) == FALSE) {
    n1
  } else {
    0
  }
}
## Function to Update Customer Name
popRight <- function(x){
  n1 <- substr(x, nchar(x)-0, nchar(x))
  n2 <- substr(x, nchar(x)-1, nchar(x))
  if (grepl("[^0-9]",n2) == FALSE) {
    substr(x, 0, nchar(x)-2)
  } else if (grepl("[^0-9]",n1) == FALSE) {
    substr(x, 0, nchar(x)-1)
  } else {
    x
  }
}
#Apply
temp <- lapply(m$Customer, subRight)
#Combine List w/ Dataframe
m$CustomerDiv <- do.call("rbind", temp)
#Remove Number from Customer Names
temp <- lapply(m$Customer, popRight)
#Combine List w/ Dataframe
m$Customer <- do.call("rbind", temp)
#clean up Temp
rm(temp)


##Replace 16 Cities
##Tuscon, Rhode Island, Philadelphia, etc
m$Zone <- gsub("Philadephia","Philadelphia",m$Zone)
m$Zone <- gsub("Connecticut", "Hartford",m$Zone)
m$Zone <- gsub("Massachusetts", "Boston",m$Zone)
m$Zone <- gsub("Rhode Island", "Providence",m$Zone)
m$Zone <- gsub("Tuscon", "Tucson",m$Zone)
m$Zone <- gsub("Austin", "Austin, TX",m$Zone)
m$Zone <- gsub("Charleston", "Charleston, SC",m$Zone)
m$Zone <- gsub("Cleveland", "Cleveland, OH",m$Zone)
m$Zone <- gsub("Dallas", "Dallas, TX",m$Zone)
m$Zone <- gsub("Jacksonville", "Jacksonville, FL",m$Zone)
m$Zone <- gsub("Kansas City", "Kansas City, MO",m$Zone)
m$Zone <- gsub("Lincoln", "Lincoln, NE",m$Zone)
m$Zone <- gsub("Madison", "Madison, WI",m$Zone)
m$Zone <- gsub("Portland", "Portland, OR",m$Zone)
m$Zone <- gsub("Rochester", "Rochester, NY",m$Zone)
m$Zone <- gsub("Springfield", "Springfield, IL",m$Zone)

## Write to File
write.csv(m, file="data/newCust.csv")


