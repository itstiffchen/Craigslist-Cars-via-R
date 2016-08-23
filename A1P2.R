# Assignment 1 Part 2
# Tiffany Chen ID: 998840686

vehicles = load("/Users/tiffanychen/Desktop/STA 141/vehicles.rda")

# ANOMALY 1: large prices
## 1 HIGH PRICE
p = vposts$price
tail(sort(vposts$price), 50)
max(p,na.rm=TRUE)
ids = which(vposts$price == 600030000) # there are 2
ids
vposts[ids, ] # this posting for 600030000 is posted twice. Cost is supposed to be between 6000 & 30,000  sold byOwner
vposts[ids, "price"] = 18000 # let's change to the mean

ids = which(vposts$price == 30002500)
vposts[ids,] #2002 Cadillac Seville 8 cyl gas sold byOwner
vposts[ids, "price"] = 2750
# pretty sure this is a range of prices too

ids = which(vposts$price == 9999999)
vposts[ids, ] # just delete this one. "$20 obo. comes with complimentary Oboe. the description makes no sense. new used car, good bad condition. location is everywhere." contradictory information
vposts[-13937, ] # delete this one

ids = which(vposts$price == 569500)
vposts[ids, ] # post is byDealer, they have their own site, so credible source. the price doesnt seem right though. I went on their website www.lot1autosales.com and found the exact car on sale for $6,995 
# http://www.lot1autosales.com/2007_Chevrolet_Monte%20Carlo_Melrose%20Park_IL_258261321.veh
vposts[ids,"price"] = 56950

ids = which(vposts$price == 400000)
vposts[ids, ]
# http://newyork.craigslist.org/wch/ctd/5215236700.html
# reasonable according to google 
# 2006 Ford GT Canada
# http://www.autotrader.ca/cars/ford/gt/

ids = which(vposts$price == 359000)
vposts[ids, ]
vposts[ids, "price"] = 35900 # dropped a digit
# Chevrolet 2010 
# http://www.cargurus.com/Cars/l-Used-2010-Chevrolet-Silverado-1500-LTZ-ev41
# i think theres a typo, extra 0

## 2 HIGH ODOMETER
tail(sort(vposts$odometer), 50)

od = which(vposts$odometer == 1234567890)
vposts[od, ] 
vposts[-18161, ] 
# delete this one. the odometer is clearly wrong, so many typos in the post. 

od = which(vposts$odometer == 99999999)
vposts[od, ] 
vposts[-4530, ]
# this owner also clearly typed a random odometer for his 1988 jeep comanche

od = which(vposts$odometer == 16000000)
vposts[od, ] 
vposts[-19227, ]
vposts[-19537, ] 
# posted twice, the maximum odometer is about 6 digits 999999
# if this car is like new, then the odometer reading is wrong
od = which(vposts$odometer == 9500000)
vposts[od, ] 

od = which(vposts$odometer == 3000000)
vposts[od, ] 

od = which(vposts$odometer == 2800000)
vposts[od, ] 
## i dont get why its high. makes no sense. 
# i can at most change it to 999999 or delete a digit. 

vposts$price == 1 & grepl('\\$', vposts$body) # piazza 
grepl('$', c('This has $ in it', 'This does not'))


## 3 
head(sort(vposts$year), 50)
# there is a year 2022? 
yr = which(vposts$year == 2022)
vposts[yr, ]
## based on the picture i think its 2012
yr = which(vposts$year == 1900) # delete. it was posted 7 times and has the same title CAR WON'T PASS SMOG??WE'LL BUY TODAY!! - $750
vposts[yr, ]

yr = which(vposts$year == 1921) # ok 
vposts[yr, ]

yr = which(vposts$year == 1922) # ok
vposts[yr, ]

yr = which(vposts$year == 1923) # ok 
vposts[yr, ]

head(sort(vposts$year), 50)
# there is a year 4? 
yr = which(vposts$year == 4)
vposts[yr, ]
## deleting this has too many typos. the link on craigslist, this post has been removed 
# https://newyork.craigslist.org/mnh/cto/5233079816.html

# 4 location in the ocean
library(maps)
map('state')
smoothScatter(vposts$long, vposts$lat)
map('state', add = T)
points(vposts$long, vposts$lat, add = T)
locator(1)
identify(vposts$long, vposts$lat)
vposts[34388,]

## INSIGHT 1: transmission and price
plot(vposts$transmission, vposts$price, ylim = c(0, 100000), main = "Transmission vs. Price", xlab = "Transmission", ylab = "Price")

# INSIGHT 2: type of car and price
plot(vposts$type, vposts$price, ylim = c(0, 100000), main = "Type vs. Price", xlab = "Type", ylab = "Price", las = 2)

head(vposts[other, ])
other = !is.na(vposts$transmission) & vposts$transmission == "other"

price_other = vposts[other, "price"]
tail(sort(price_other))

# INSIGHT 3: type, cylinder, size
x=table(vposts$type, vposts$cylinder, vposts$size)
library(lattice)
dotplot(x, besides=TRUE, breaks=seq(0,3000,10), main="Distribution of vehicle type, cylinder, and size", auto.key=TRUE)

# alternative fuel cars vs. regular price 
plot(vposts$fuel, vposts$price, ylim = c(0, 100000), main = "Fuel Type vs. Price", xlab = "Fuel Type", ylab = "Price", las = 2)

# alternative fuel cars vs. regular price vs. age
vposts$age = 2015 - vposts$year # many negative values, so got to clean it
betterage = vposts$age[vposts$age>=-1 & vposts$age<=115]
table(vposts$age, vposts$fuel, vposts$price)

# this doesn't look good? 
# i tried changing the axis??
library(lattice)
histogram( ~ vposts$price | vposts$transmission + vposts$type, vposts, main="Distribution of Price by Transmission and type", type = "percent", xlab="Price of Car") 

a = vposts[ !is.na(vposts$price) & vposts$price == 1, ]
b = subset(vposts, price == 1)

