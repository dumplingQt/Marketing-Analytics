## It is a Very small practice on BGBB Model
## The reason why I choose online games is that it is a typical example of non-contractual relationship

# clear the workspace
rm(list=ls())

libs=c("readxl", "BTYD", "BTYDplus", "dplyr", "lubridate")
if(sum(!(libs %in% .packages(all.available=TRUE)))>0){
  install.packages(libs[!(libs %in% .packages(all.available=TRUE))])}
rm(libs)

library(readxl)
library(BTYD)
library(BTYDplus)
library(dplyr)
library(lubridate)

## load the data
transactions <- as.data.frame(
  read_excel("data/IndExercise/game_transactions.xlsx", sheet=1))
colnames(transactions) <- c(
  "package", "cust", "date", "sales", "reg_date")

nrow(transactions)
length(unique(transactions$cust))

transactions$reg_date = as.Date(transactions$reg_date)
transactions$date = as.Date(transactions$date)

## sort by customer id and date (of payment)
transactions = arrange(transactions, cust, date)

## keep only the customers who register before 2016-01-01,
## so that everyone has equal number of purchase opportunities 
## in the calibration data period
transactions = filter(transactions, reg_date<"2016-01-01")
transactions$month = month(transactions$date)
# calibration period
transactions.cal = filter(transactions, date<"2017-01-01")
# forecasting period
transactions.for = filter(transactions, date>="2017-01-01")

# aggregate all the purchases within one month together
cal.monthly = transactions.cal %>% 
  group_by(cust, month) %>%
  summarise(total.sales=sum(sales), num.purchases=n())
cal.ss = cal.monthly %>%
  group_by(cust) %>%
  summarise(x=n(), t.x=max(month), avr.sales=mean(total.sales), birth.month=min(month))

# keep those customers who purchase in the first month
# as if they are "acquired in the first month"
cal.ss = cal.ss[cal.ss$birth.month==1, ]

# using month 2-12 as the calibration data
cal.ss$x = cal.ss$x - 1
cal.ss$t.x = cal.ss$t.x - 1

#prepare the average price for question2
cal.avr.sales = cal.ss %>%
  group_by(t.x, x) %>%
  summarise(avr.sales=mean(avr.sales))

cal.ss = as.matrix(cal.ss)

freq <- cal.ss[,"x"]
rec <- cal.ss[,"t.x"]
trans.opp <- 11 # transaction opportunities
# create the recency-frequency matrix
cal.rf.matrix <- dc.MakeRFmatrixCal(freq, rec, trans.opp)
cal.rf.matrix[1:5,]

## estimate the model based on the recency-frequency matrix
params <- bgbb.EstimateParameters(cal.rf.matrix)
# goodness of fit with the data
bgbb.PlotFrequencyInCalibration(params, cal.rf.matrix)

## calculate the number of expected purchases for these customers in the next 6 months
##  i.e., Jan-Jun in 2017
x = cal.rf.matrix[,1]
t.x = cal.rf.matrix[,2]
n.cal = cal.rf.matrix[,3]
n.star = 6
expected.purchases = bgbb.ConditionalExpectedTransactions(params, n.cal, n.star, x, t.x)
total.expected.purchases = cal.rf.matrix[,4] %*% expected.purchases

x = cal.rf.matrix[,1]
t.x = cal.rf.matrix[,2]
n.cal = cal.rf.matrix[,3]
n.star = 4
expected.purchases = bgbb.ConditionalExpectedTransactions(params, n.cal, n.star, x, t.x)
total.expected.purchases = cal.rf.matrix[,4] %*% expected.purchases

bgbb.PlotFreqVsConditionalExpectedFrequency(params, 11, cal.rf.matrix, expected.purchases)

## estimate the expected contribution of the customers as well
## during Jan-Jun in 2017, based on their purchase information in Feb-Dec in 2016
matrix = cbind(cal.rf.matrix,expected.purchases)
final.matrix = cbind(matrix[ matrix[,4] > 0,],cal.avr.sales[,3])

total.expected.purchases = final.matrix[,4] *final.matrix[,5] *final.matrix[,6]
sum(total.expected.purchases)
