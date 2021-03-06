#######################################
#
# Decision Making on Promotion Strategy for Maru Batting Center
# based on Customer Lifetime Value
# A first small smart attempt!
#
#######################################

rm(list=ls())

#### set up all the parameters
# give names for different customer segments
seg.names = c(
  "Little Leaguers", "Summer Sluggers", 
  "Elite Ballplayers (Print Ad)", "Elite Ballplayers (Party)", 
  "Entertainment Seekers"
# Input contact cost and response rate on ads of each segment
contact.cost = c(1000, 1500, 300, 12500, 50)
response.rate = c(0.10, 0.15, 0.005, 0.25, 0.025)
# calculate the acquisition cost using the formula contact.cost / response.rate
acquisition.cost = contact.cost/response.rate
# input labor cost for marketing
workers.needed = c(2, 1, 1, 1, 2)
worker.labor.cost = rep(1500, 5)
instructors.needed = c(1, 0, 1, 1, 0)
instructor.hourly.labor.cost = c(3000, 0, 4500, 4500, 0)
# calculate the total cost per hour
total.cost.per.hour = workers.needed * worker.labor.cost + instructors.needed * instructor.hourly.labor.cost
# input the revenue received per hour
hourly.price.charged = c(6500, 3000, 7500, 7500, 4000)
# calculate the hourly margins
hourly.margin = hourly.price.charged - total.cost.per.hour
hourly.margin.pct = hourly.margin / hourly.price.charged
# input the annual hours in service for each segment
annual.hours = c(10, 4, 20, 20, 1.5)
# calculate total annual margin for each segment
annual.margin = annual.hours * hourly.margin
# input retention rate
retention.rate = c(0.75, 0.50, 0.60, 0.60, 0.35)
# number of years to calculate (assume they can be alive for total 25 years)(long enough haha)
num.year = 25

#### Start to consider different marketing proposals!
### Calculate breakeven point for each segment (ignore the discount rate)
discount.rate = 0

## Set up an awesome function to output the CLV of a customer in one segment.
calc.CLV = function(seg.index) {
  
  # survival rate for each year
  survival.rate = rep(0, num.year)
  # annual profit for each year
  annual.profit = rep(0, num.year)
  # NPV of annual profit
  npv.annual.profit = rep(0, num.year)
  # cumulative profit
  cum.profit = rep(0, num.year)
  # NPV of cumulative profit
  npv.cum.profit = rep(0, num.year)
  
  for (t in 1:num.year) {
    
      # At period 1, a customer is just acquired
      # so his survival rate is 100%, has no profit, and need full acquisition cost
    if (t==1) {
      survival.rate[t] = 1
      annual.profit[t] = annual.profit[t] - acquisition.cost[seg.index]
    }
    else {
      # At other periods, survival rate = survival rate in previous period * retention rate
      survival.rate[t] = survival.rate[t-1] * retention.rate[seg.index]
    }

    # calculate the annual profit for this period
    annual.profit[t] = annual.profit[t] + survival.rate[t] * annual.margin[seg.index]
    # calculate the NPV of the annual profit for this year
    # purchase happens at the beginning of each year so there is no discounting for the 1st period
    npv.annual.profit[t] = annual.profit[t] / (1+discount.rate)^(t-1)
    
    if (t==1) {
      # in the 1st period, cumulative profit is simply the annual profit
      cum.profit[t] = annual.profit[t]
      npv.cum.profit[t] = cum.profit[t]
    }
    else {
      # in other periods, cumulative profit is the one in previous period plus the annual profit for this period
      cum.profit[t] = cum.profit[t-1] + annual.profit[t]
      npv.cum.profit[t] = cum.profit[t] / (1+discount.rate)^(t-1)
    }
    
  }
  
  # wrap all results!
  ret = list(
    survival.rate=survival.rate, 
    annual.profit=annual.profit,
    npv.annual.profit=npv.annual.profit,
    cum.profit=cum.profit,
    npv.cum.profit=npv.cum.profit)
  return(ret)
  
}

# using lihai de lapply to apply this function to all segments
results = lapply(1:5, calc.CLV)

# obtain the break-even year for all the segments
# we assume that when npv.annual.profit<100, a customer is an attrition (he dies).
# managed to write a clever and gorgeous loop 
for (i in 1:5) {
  positive.npv.cum.profit = which(results[[i]]$npv.cum.profit>0)
  print(positive.npv.cum.profit[1])
}
npv.annual.profit.large.enough = which(results[[1]]$npv.annual.profit>=100)
last.index = length(npv.annual.profit.large.enough)
results[[1]]$npv.cum.profit[npv.annual.profit.large.enough[last.index]]

### consider their is a discount rate of 10% and calculate CLV
discount.rate = 0.10

results = lapply(1:5, calc.CLV)

# calculate CLV for all the segments
All.clv = rep(0, 5)
for(i in 1:5) {
  npv.annual.profit.large.enough = which(results[[i]]$npv.annual.profit>=100)
  last.index = length(npv.annual.profit.large.enough)
  All.clv[i] = results[[i]]$npv.cum.profit[npv.annual.profit.large.enough[last.index]]
}
All.clv

### identify which segment is most attractive (choose CLV)

best.segement = which.max(All.clv)
best.segement

### Consider an alternative place, Chiyoda ward, which is the alternative for original Minato ward
# managed to solve it by adding parameters to previous variables
contact.cost = c(contact.cost, 600)
response.rate = c(response.rate, 0.08)
# calculate the acquisition cost
acquisition.cost = contact.cost/response.rate
acquisition.cost

workers.needed = c(workers.needed, 2)
worker.labor.cost = c(worker.labor.cost, 1500)
instructors.needed = c(instructors.needed, 1)
instructor.hourly.labor.cost = c(instructor.hourly.labor.cost, 3000)
# calculate the total cost per hour
total.cost.per.hour = workers.needed * worker.labor.cost + instructors.needed * instructor.hourly.labor.cost

hourly.price.charged = c(hourly.price.charged, 6500)
hourly.margin = hourly.price.charged - total.cost.per.hour
hourly.margin.pct = hourly.margin / hourly.price.charged
annual.hours = c(annual.hours, 10)
annual.margin = hourly.margin * annual.hours

retention.rate = c(retention.rate, 0.65)

# calculate the results for Chiyoda
chiyoda.results <- calc.CLV(6)
# determine if Chiyoda should be targeted
npv.annual.profit.large.enough.Chiyoda = which(chiyoda.results$npv.annual.profit>=100)
last.index.Chiyoda = length(npv.annual.profit.large.enough.Chiyoda)
chiyoda.results$npv.cum.profit[npv.annual.profit.large.enough.Chiyoda[last.index.Chiyoda]]

### consider a 2nd proposal
## targeting Elite Ballplayers segment by offering a ¥500 discount
## on all future purchases to Elite Ballplayers who purchase at least 20 batting cage hours in Year 1.
# this means that they offer 500 discount since year 2
# to still make use of the function
# first assume the discount is offered since year 1,
# after calling the function to do the calculation,
# add the missing 500*20=10000 profit to annual.profit[1], npv.annual.profit[1],
# and to all the elements in cum.profit and npv.cum.profit

# change the parameters.
# 1. the hourly price is reduced to 7000
# 2. the retention rate is increased to 0.75

hourly.price.charged[4] = 7000
hourly.margin = hourly.price.charged-total.cost.per.hour
hourly.margin.pct = hourly.margin / hourly.price.charged
annual.margin = hourly.margin * annual.hours

retention.rate[4] = 0.75

elite.ballplayers.discount = calc.CLV(4)
# Add the 200*50 to the annual profit in the first period
elite.ballplayers.discount$annual.profit[1] = elite.ballplayers.discount$annual.profit[1] + 500*20
# also the NPV of the annual profit in the first period - Why?
elite.ballplayers.discount$npv.annual.profit[1] = elite.ballplayers.discount$npv.annual.profit[1] + 500*20
# Add the 200*50 to the cumulative profit in all the periods
elite.ballplayers.discount$cum.profit = elite.ballplayers.discount$cum.profit + 500*20
# also to the NPV of the cumulative profit in all the periods
elite.ballplayers.discount$npv.cum.profit = elite.ballplayers.discount$npv.cum.profit + {(500*20) / (1+discount.rate)^(c(1:num.year)-1)}

npv.annual.profit.large.enough.EBpromo = which(elite.ballplayers.discount$npv.annual.profit>=100)
last.index.EBpromo = length(npv.annual.profit.large.enough.EBpromo)
elite.ballplayers.discount$npv.cum.profit[npv.annual.profit.large.enough.EBpromo[last.index.EBpromo]]

### consider a 3rd proposal:
### offering converted Elite Ball-players a free bat
# change the parameters
# 1. the response rate is increased 0.29
# 2. for each "ACQUIARED" customer, a free bat is to be given, which increases the acquisition cost
response.rate[4] = 0.29
acquisition.cost = contact.cost / response.rate
# add the bat cost to the acquisition cost (only for this segment)
acquisition.cost[4] = acquisition.cost[4] + 10000
# set the parameters used in aforementioned proposal back to the original value
hourly.price.charged[4] = 7500
hourly.margin = hourly.price.charged-total.cost.per.hour
hourly.margin.pct = hourly.margin / hourly.price.charged
annual.margin = hourly.margin * annual.hours
retention.rate[4] = 0.60

elite.ballplayers.bat = calc.CLV(4)
round(elite.ballplayers.bat$npv.annual.profit,3)

# make decision!
npv.annual.profit.large.enough.EBbat = which(elite.ballplayers.bat$npv.annual.profit>=100)
last.index.EBbat = length(npv.annual.profit.large.enough.EBbat)
elite.ballplayers.bat$npv.cum.profit[npv.annual.profit.large.enough.EBbat[last.index.EBbat]]
