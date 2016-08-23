##  Assignment 1 Part 1
## Tiffany Chen

# code neatly, very clean!!!!!

# load the data
print(load(url("http://eeyore.ucdavis.edu/stat141/Data/vehicles.rda")))

# q1
# there are 34677 rows and 26 columns
dim(vposts)
# number of observations
nrow(vposts)

# q2
names(vposts)
sapply(vposts,class) 

# q3  
mean(vposts$price, na.rm = TRUE)
sd(vposts$price, na.rm = T)
median(vposts$price, na.rm = TRUE)
quantile(vposts$price, probs = seq(0,1,0.1), na.rm = TRUE)

prices = subset(vposts$price, vposts$price <= 30000,) # from Piazza

# hist original data w/ outlier
hist(vposts$price)
rug(vposts$price)
summary(vposts$price)

# hist with no outlier
hist(prices, main = "Distribution of Vehicle Prices w/o Outliers", xlab = "Price", ylab = "Count")
m = mean(prices, na.rm = TRUE)
med = median(prices, na.rm = TRUE)
q = quantile(prices, probs = seq(0,1,0.1), na.rm = TRUE)
abline(v = c(m, med), col = c("red", "blue"), lty = c(1,2), lwd = c(1,3)) # mean and median
abline(v = q) # deciles

# from Piazza
price_boxplot = boxplot(vposts$price)
sort(price_boxplot$out)
## shows the outliers, 
# i should subset the data
# excluding prices 29000 and higher, similar to online results

#q4
## this deleted the NAs
x = table(vposts$type)
prop.table(x)
# same as
table(vposts$type)/length(vposts$type)

#q5
levels(vposts$fuel) # give levels of the unique values

#relationship btwn type and fuel
plot(vposts$type,vposts$fuel,main='Vehicle Type vs. Fuel Type',xlab="Vehicle Type",ylab="Fuel Type")
with(vposts,plot(type,fuel))

# all three
x=table(vposts$type,vposts$fuel,vposts$transmission)
mosaicplot(x, las=2)
axis(las=2)

# does this depend on transmission type?
# yes
# piazza
par(mfrow=c(2,2))
auto = subset(vposts, transmission == "automatic")
plot(auto$type, auto$fuel, main = "Vehicle Type vs. Fuel Type -- Automatics", xlab = "Vehicle Type", ylab = "Fuel Type")

man = subset(vposts,transmission=="manual")
plot(man$type, man$fuel, main = "Vehicle Type vs. Fuel Type -- Manual", xlab= "Vehicle Type", ylab = "Fuel Type")

other = subset(vposts,transmission == "other")
plot(other$type, other$fuel, main="Vehicle Type vs. Fuel Type -- Other", xlab = "Vehicle Type", ylab = "Fuel Type")

#q6
# 7 different cities
table(vposts$city)

#q7
c=table(vposts$byOwner, vposts$city)
barplot(c, main="Proportion sale by Owner vs. sale by Dealer across Cities", xlab="City",ylab="Number of Cars",col=c("blue","red"),legend.text=c("Sold by Dealer","Sold by Owner"),beside=TRUE) 
## dot chart from piazza
dotchart(c, main="Proportion sale by Owner vs. sale by Dealer across Cities", xlab="City",col=c("blue","red"),labels=c("Sold by Owner", "Sold by Dealer"),cex=.7)

#q8
p = vposts$price
max(p,na.rm=TRUE)
ids=which(vposts$price==600030000)
vposts[ids,"price"]=21000
p = vposts$price
max(p,na.rm=TRUE)
ids=which(vposts$price==30002500)
vposts[ids,"price"]=30000
p = vposts$price
max(p,na.rm=TRUE)
ids=which(vposts$price==9999999)
vposts[ids,"price"]=30000
p = vposts$price
max(p,na.rm=TRUE)

# q9 
# piazza Nick
owner = subset(vposts, byOwner == "TRUE") # subset byOwner first
counts = table(owner$city, owner$maker)
top3_makes = function(city_counts) {
  # Assume city_counts has counts for just one city.
  # Get indexes of "top 3" makes.
  # You could also use head(..., 3) instead of [1:3].
  top3 = order(city_counts, decreasing = TRUE)[1:3]
  
  # Convert indexes to names.
  # Use rownames() instead if "make" is the rows.
  colnames(counts)[top3]
}
apply(counts,1,top3_makes)

dealer=subset(vposts,byOwner=="FALSE") # then subset by dealer
counts=table(dealer$city,dealer$maker)

c=table(vposts$city,vposts$maker,vposts$byOwner)
c=table(vposts$byOwner,vposts$city,vposts$maker)

par(mfrow=c(2,2))
bos = subset(vposts,city=="boston")
x=table(bos$maker,bos$byOwner)
mosaicplot(x,color=TRUE,las=2,main="Boston")

chi = subset(vposts,city=="chicago")
x=table(chi$maker,chi$byOwner)
mosaicplot(x,color=TRUE,las=2,main="Chicago")

den = subset(vposts,city=="denver")
x=table(den$maker,den$byOwner)
mosaicplot(x,color=TRUE,las=2,main="Denver")

lv = subset(vposts,city=="lasvegas")
x=table(lv$maker,lv$byOwner)
mosaicplot(x,color=TRUE,las=2,main="Las Vegas")

nyc = subset(vposts,city=="nyc")
x=table(nyc$maker,nyc$byOwner)
mosaicplot(x,color=TRUE,las=2,main="NYC")

sac = subset(vposts,city=="sac")
x=table(sac$maker,sac$byOwner)
mosaicplot(x,color=TRUE,las=2,main="Sac")

sfbay = subset(vposts,city=="sfbay")
x=table(sfbay$maker,sfbay$byOwner)
mosaicplot(x,color=TRUE,las=2,main="SF")

#q10 ??
x=table(vposts$year,vposts$city,vposts$byOwner)
mosaicplot(x)
vposts$age = 2015 - vposts$year # many negative values, so got to clean it
head(sort(vposts$age), 250)

ids=which(vposts$age==-1)
vposts[1929,] # 2016 car
vposts[21975,] #2022 car, actually is a 2016 honda. 
vposts[9361,] #1962
vposts[2078,] #2016
vposts[2785,] #2016 car

tail(sort(vposts$age),250)
ids=which(vposts$age==2011) # check out the aged 2011 car
vposts[8417,] # just delete this
ids=which(vposts$age==115)
vposts[27557,] # 1900  tires
vposts[27901,] # 1900 dodge

# subset by age -1 to 115
betterage = vposts$age[vposts$age>=-1 & vposts$age<=115]

library(lattice)
histogram( ~ betterage | byOwner + city, vposts, main="Distribution of Cars by City and Sale by Owner/Dealer",xlab="Age of Car (2015 as current year)",breaks=seq(-5,120,5))

#q11
install.packages("maps")
library(maps)
map('usa')
map() # looks like its just the US
points(vposts$long, vposts$lat, col = "red", pch = ".")

#q12 
x=table(vposts$type, vposts$fuel, vposts$drive, vposts$transmission)
library(lattice)
dotplot(x, besides=TRUE, breaks=seq(0,3000,10), main="Distribution of fuel type, drive, transmission, and vehicle type", auto.key=TRUE)

## below is not good cuz the ranges on the axes are different
par(mfrow=c(2,2))
plot(vposts$fuel, main="Distribution of Fuel Type",xlab="Fuel Type",ylab="Frequency",ylim=c(0,50000))
plot(vposts$drive, main="Distribution of Drive Type",xlab="Drive Type",ylab="Frequency",ylim=c(0,50000))
plot(vposts$transmission, main="Distribution of Transmission Type",xlab="Transmission Type",ylab="Frequency",ylim=c(0,50000))
plot(vposts$type,main="Distribution of Vehicle Type",xlab="Vehicle Type",ylab="Frequency",ylim=c(0,50000),cex.names=.5)
     
# q13
# cars that are newer, have not been driven yet/less miles traveled
# cars that are newer with better technology can travel farther 
# compared to older cars
plot(vposts$year, vposts$odometer) # clumped up 

# year vs odometer
plot(vposts$year, vposts$odometer, xlab = "Year", ylab = "Odometer", main= "Year vs. Odometer", xlim = c(1950,2015), ylim = c(0,450000))

# price vs odometer
plot(vposts$price, vposts$odometer, xlab="Price",ylab="Odometer", main="Price vs. Odometer", xlim = c(1000,50000), ylim = c(0,450000))
# cars that have more mileage, are valued less

#q14
# define old car as 25 years old. so 1990 and earlier
old = vposts[vposts$year <= 1950,]
unique(old$maker)
hist(old$price, main = "Price Distribution of 1950 cars and Older", xlab= "Price")

#q15
# colour, title status?

#q16
# groups conditions into good, restored, used, other, preowned
cond_cat = vposts # make a copy

levels(cond_cat$condition)[levels(cond_cat$condition) %in% c("excellent","like new", "nice", "nice teuck", "superb original", "very good", "good", "fair", "new") ] = "good"

levels(cond_cat$condition)[levels(cond_cat$condition) %in% c("muscle car restore", "needs restoration!", "needs total restore", "restoration", "restore","needs restored","needs work","nice rolling restoration","rebuildable project","restoration project","restored") ] = "restore"

levels(cond_cat$condition)[levels(cond_cat$condition)%in% c("0used", "used")] = "used"

levels(cond_cat$condition)[levels(cond_cat$condition)%in% c("ac/heater","certified","front side damage","hit and run :( gently","needs work/for parts","parts","pre-owned","preownes","project car","rough but runs","207,400","carfax guarantee!!","complete parts car, blown engine","honnda","mint","needs bodywork","needs work","not running","project","salvage")] = "other"

levels(cond_cat$condition)[levels(cond_cat$condition) %in% c("pre-owned","preownes","pre owned","preowned")] = "preowned"

# boxplots of condition vs odometer
plot(cond_cat$condition, cond_cat$odometer, ylim=c(0,450000), main = "Condition vs. Odometer", xlab = "Condition", ylab = "Odometer" )

# boxplot of condition vs price
plot(cond_cat$condition, cond_cat$price, main = "Condition vs. Price", xlab = "Condition", ylab = "Price", ylim=c(0,60000))

# boxplot of age and condition
cond_cat$age = 2015 - cond_cat$year
plot(cond_cat$condition, cond_cat$age, ylim = c(-1, 115), main = "Condition vs. Age", xlab = "Condition", ylab = "Age" )
