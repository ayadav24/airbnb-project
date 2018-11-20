### Airbnb Dataset of NewYork City ###

install.packages("dplyr")
library(dplyr)
install.packages("psych")
library(psych)
install.packages("corrplot")
library(corrplot)
install.packages("readr")
library(readr)
install.packages("gmodels")
library(gmodels)
install.packages("gplots")
library(gplots)
install.packages("stringr")
library(stringr)
install.packages("car")
library(car)
listings <- read.csv(file.choose(), header=T) 
options(scipen=99)
dim(listings) #[1] 44317    16
names(listings)
#[1] "id"                             "name"                           "host_id"                       
#[4] "host_name"                      "neighbourhood_group"            "neighbourhood"                 
#[7] "latitude"                       "longitude"                      "room_type"                     
#[10] "price"                          "minimum_nights"                 "number_of_reviews"             
#[13] "last_review"                    "reviews_per_month"              "calculated_host_listings_count"
#[16] "availability_365"   
str(listings)

##BOX PLOTS OF INITIAL RAW DATA with 44317 rows
boxplot(listings$price)   #Highly Skewed
boxplot(listings$number_of_reviews)   #Highly Skewed

##CLEANING OF DATA
list<- listings
#removing ID, Latitude and Longitude and reviews_per_month columns
list$id<-NULL
list$latitude <- NULL
list$longitude <- NULL
list$reviews_per_month <- NULL
#Converting neighbourhood group and room type into factors
list$neighbourhood_group<-as.factor(list$neighbourhood_group)
list$room_type<-as.factor(list$room_type)
str(list)
dim(list) #44317    12
# removing the data where minimum number of nights is greater than 1 year i.e. 365 days
list<-list[!(list$minimum_nights>365),]
#as per the data observed above and airbnb site, maximum price of observed is $3000 for all kind of room types 
list<-list[!(list$price >= 3000),]
#keeping minimum price of rooms as $10
list<-list[!(list$price<=10),]
#keeping minimum price of private room as $20
list<-list[!(list$price<=20 & list$room_type == "Private room"),]
#keeping minimum price of shared room as $30
list<-list[!(list$price<=30 & list$room_type == "Entire home/apt"),]
#removing host names which are not meaningful
list<-list[(list$name!=""),]
list <- list[(list$host_name == str_match_all(list$host_name, "[a-zA-Z]{1,}")),]
dim(list) #40800    12
# removing the data where a room is not really posted, i.e. its availability was for 0 days
list<-list[!(list$availability_365 == 0),]
# removing the data where the availability of a room or apartment is lesser than the minimum nights
list<-list[!(list$availability_365 < list$minimum_nights),]
dim(list) #[1] 25567    12
# removing the rows where number_of_reviews is equal to zero
list<- list[!(list$number_of_reviews==0),]
dim(list) #[1] 21867    12

##UNIVARIATE ANALYSIS
#Factors
# 1. Neighbourhood Groups
tab <- table(list$neighbourhood_group)
ptab <- prop.table(tab)
ptab<- ptab *100
barplot(ptab, main = "Bar Plot", xlab = "Neighbourhood Groups", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,50))
box()
#Manhattan has the most number of listings in the entire New York 

# 2. Room Type 
tab <- table(list$room_type)
ptab <- prop.table(tab)
ptab<- ptab *100
barplot(ptab, main = "Bar Plot", xlab = "room_type", ylab = "Proportion", col=c("orange", "steelblue" , "blue"), ylim=c(0,65))
box()
# Of all the listings Entire home/apt are there in a 
#greater proprtion then its followed by private room and shared room listings are the least.

#Numeric
# 1. Price
describe(list$price)
hist(list$price,breaks = 25, xlab= "Price" ,xlim = c(0,1000),col=c("steelblue", "red"), freq=F)
rug(jitter(list$price), col="darkgray")
lines(density(list$price), col="yellow", lwd=3)
plot(density(log(list$price))) #To convert the price to a near normal distribution we use the log function.
box()
#Price is highly right skewed data with min of 17 and max of 2800

# 2. Number of Reviews
describe(list$number_of_reviews)
hist(list$number_of_reviews,breaks = 25, xlim = c(0,500),col=c("steelblue", "red"), freq=F)
rug(jitter(list$number_of_reviews), col="darkgray")
lines(density(list$number_of_reviews), col="yellow", lwd=3)
plot(density(log(list$number_of_reviews))) #To convert number of reviews to a near normal distribution we use the log function.
box()
#The number  of reviews is a highly right skewed data that is a lot of listings have less than 28 reviews.

# 3. Availability_365
describe(list$availability_365)
hist(list$availability_365,col=c("steelblue", "red"), xlim = c(0,400), freq=F)
rug(jitter(list$availability_365), col="black")
lines(density(list$availability_365), col="yellow", lwd=3)
box()

# 4. Minimum Nights
describe(list$minimum_nights)
hist(list$minimum_nights,breaks = 500, xlim = c(0,35), 
     xlab = "Minimum_Nights" , main = "Histogram of Minimum_Nights",col=c("steelblue", "red"), freq=F)
rug(jitter(list$minimum_nights), col="darkgray")
lines(density(list$minimum_nights), col="yellow", lwd=3)
box()

#BIVARIATE ANALYSIS
# Two Numerics
#1. Price and Number of Reviews
plot(list$number_of_reviews~list$price, col="red", 
          main="Relationship of Price to the number of reviews", 
          xlab="Price", 
          ylab="Number of Reviews", 
          pch=16)
abline(lm(list$number_of_reviews~list$price), col="darkgreen", lwd=2.5)
lines(lowess(list$number_of_reviews~list$price), col="steelblue", lwd=2.5)
#Smoother is similar to regression line, not indicating any linear relationship
cor.test(list$price, list$number_of_reviews) #-0.02700574 (2.7% of negative correlation)

#2.availability_365 and Number of Reviews
plot(list$number_of_reviews~list$availability_365, col="red", 
     main="Relationship of availability_365 to the number of reviews", 
     xlab="Availability_365", 
     ylab="Number of Reviews", 
     pch=16)
abline(lm(list$number_of_reviews~list$availability_365), col="darkgreen", lwd=2.5)
lines(lowess(list$number_of_reviews~list$availability_365), col="steelblue", lwd=2.5)
cor.test(list$availability_365, list$number_of_reviews) # 0.132912 (13.2% of positive correlation) 

#3. minimum_nights and Number of reviews
plot(list$number_of_reviews~list$minimum_nights, col="red", 
     main="Relationship of minimum_nights to the number of reviews", 
     xlab="minimum_nights", 
     ylab="Number of Reviews", 
     pch=16)
abline(lm(list$number_of_reviews~list$minimum_nights), col="darkgreen", lwd=2.5)
lines(lowess(list$number_of_reviews~list$minimum_nights), col="steelblue", lwd=2.5)
cor.test(list$minimum_nights, list$number_of_reviews) #-0.0942254 (9.4% of negative correlation)

#Two Factors
#1. neighbourhood group and room type
tab <- table(list$room_type, list$neighbourhood_group)
ptab <- prop.table(tab)
xtabs(~list$room_type+list$neighbourhood_group)
prop.table(xtabs(~list$room_type+list$neighbourhood_group))
mosaicplot(tab, main="Room Types in different neighbourhood Groups", col=c("red", "greenyellow", "blue" ,"grey" , "orange"))
barplot(tab, col=c("red", "greenyellow", "blue"),main="Room Types in different neighbourhood Groups") 
legend("topright", 
       legend = c("Entire home/Apt", "Private Room", "Shared Room"), 
       fill = c("red", "greenyellow", "blue"))	  # In this particular plot the bottom right area is probably best for adding a legend.
chisq.test(tab) 
# It shows there is a relationship between Neighbourhood group and room type

#Factor and Numeric
#1. Number of Reviews and Neighbourhood Groups
tab <- table(list$number_of_reviews, list$neighbourhood_group)
boxplot(number_of_reviews ~ neighbourhood_group, data=list, main="Number of Reviews in Different Neighbourhood groups", 
        xlab="Neighbourhood Group", ylab="Number of Reviews",
        col=c("orange", "lightblue4")) 
tab.aov<- aov(number_of_reviews~neighbourhood_group, data=list)
tab.aov
summary(tab.aov)
#log transformation for Number of Reviews in the above boxplot
boxplot(log(number_of_reviews) ~ neighbourhood_group, data=list, main="Number of Reviews in Different Neighbourhood groups", 
        xlab="Neighbourhood Group", ylab="Number of Reviews",
        col=c("orange", "lightblue4")) 

#verify relation of number_of_reviews vs neighbourhood_group using ANOVA
aggregate(list$number_of_reviews ~list$neighbourhood_group, FUN="mean") 
# From Above, Average number of reviews are highest in  Manhattan
list %>% group_by(neighbourhood_group) %>% summarize(avg = mean(number_of_reviews), std = sd(number_of_reviews)) 
list.aov <- aov(number_of_reviews~neighbourhood_group, data=list)
list.aov
summary(list.aov)
# As we can see p value is less than 0.01 so we reject null hypothesis
#Number of Reviews varies with Neighbourhood Group
# We use Tukey pairwise comparisons
list.aov.tk<-TukeyHSD(list.aov)
round(list.aov.tk$neighbourhood_group,3) 

#2. Number of Reviews and Room Type
tab <- table(list$number_of_reviews, list$room_type)
boxplot(number_of_reviews ~ room_type, data=list, main="Number of Reviews for Different Room Type", 
        xlab="Room Type", ylab="Number of Reviews",
        col=c("red", "blue", "greenyellow"))
tab.aov<- aov(number_of_reviews~room_type, data=list)
tab.aov
summary(tab.aov)
#log transformation for Number of Reviews in the above boxplot
boxplot(log(number_of_reviews) ~ room_type, data=list, main="Number of Reviews for Different Room Type", 
        xlab="Room Type", ylab="Number of Reviews",
        col=c("red", "blue", "greenyellow"))
  
#verify relation of number_of_reviews vs room_type using ANOVA
aggregate(list$number_of_reviews ~list$room_type, FUN="mean") 
# From Above, Average number of reviews are highest for Entire home/apt 
list %>% group_by(room_type) %>% summarize(avg = mean(number_of_reviews), std = sd(number_of_reviews)) 
list.aov1 <- aov(number_of_reviews~room_type, data=list)
list.aov1
summary(list.aov1)
# As we can see p value is less than 0.01 so we reject null hypothesis
# Number of Reviews varies with Room_Type
# We use Tukey pairwise comparisons
list.aov1.tk<-TukeyHSD(list.aov1)
round(list.aov1.tk$room_type,3)

##CORRELATION
# correlation test on all the numeric fields in the dataset(list in our case)
listnum<-list[,c("price","minimum_nights","calculated_host_listings_count","availability_365","number_of_reviews")]
cormat<-cor(listnum)
round(cormat,2)
pairs(listnum)
scatterplotMatrix(~price+minimum_nights+calculated_host_listings_count+availability_365+ number_of_reviews, data=listnum, main="Correlations of Numeric Variables in the list Data")
corrplot(cormat, method="circle", addCoef.col="black") 
#From the above corrplot we can see that the maximum correlation(negative) is between Number of Reviews(dependent variable)
#and Minimum Nights

#HYPOTHESIS 1 - The number of reviews of an Airbnb listing in New York City is higher for a listing at 
#lower price

cor(list$number_of_reviews,list$price)
cor.test(list$number_of_reviews,list$price,
         alternative ="two.sided",
         method = "pearson")
#The p-value is very low and a negative correlation of 2.7% between price and number of reviews

# creating factor levels of our  price into three categories
list$pricecat[list$price <=100]<- "Economic"
list$pricecat[list$price >100  & list$price<=300]<- "Deluxe"
list$pricecat[list$price >300]<- "Luxury"

# Price Category as Factor (UNIVARIATE)
tab <- table(list$pricecat)
ptab <- prop.table(tab)
ptab<- ptab *100
barplot(ptab, main = "Bar Plot", xlab = "Prce Category", ylab = "Proportion", col=c("orange", "red" , "blue"), ylim=c(0,50))
box()

# Number of Reviews and Price Category (Numeric-Factor BIVARIATE)
tab <- table(list$number_of_reviews, list$pricecat)
boxplot(number_of_reviews ~ pricecat, data=list, main="Number of Reviews for Different Price Category ", 
        xlab="Price Category ", ylab="Number of Reviews",
        col=c("red", "blue", "greenyellow"))
list<- list[!(list$number_of_reviews>=350),]
dim(list) #21843    15
tab.aov<- aov(number_of_reviews~pricecat, data=list)
tab.aov
summary(tab.aov)
#log transformation for Number of Reviews in the above boxplot
boxplot(log(number_of_reviews) ~ pricecat, data=list, main="Number of Reviews in Different Price Category", 
        xlab="Price Category", ylab="Number of Reviews",
        col=c("red", "blue", "greenyellow"))

#verify relation of number_of_reviews vs Price Category using ANOVA
#Null Hypothesis- The price category and number_of_reviews is independent of each other i.e. the mean of number_of_reviews is same for all price categories
#alternative hypothesis is that the average is not the same for all groups
aggregate(list$number_of_reviews ~list$pricecat, FUN="mean") 
list %>% group_by(pricecat) %>% summarize(avg = mean(number_of_reviews), std = sd(number_of_reviews)) 
list.aov2 <- aov(number_of_reviews~pricecat, data=list)
list.aov2
summary(list.aov2)
# As we can see p value less than 0.01 so we reject null hypothesis
# We use Tukey pairwise comparisons
list.aov2.tk<-TukeyHSD(list.aov2)
round(list.aov2.tk$pricecat,3)

cor(list$price[list$pricecat=="Luxury" & list$neighbourhood_group=="Bronx"],list$number_of_reviews[list$pricecat=="Luxury" & list$neighbourhood_group=="Bronx"])
#Bronx neighbourhood has only Luxury price category so correlation is 1
#Inference-   For Luxury category rooms, the customers are not price centric. 
#Therefore, the number of reviews and price are directly proportional

##Price and number of reviews of Staten Island
cor(list$price[list$pricecat=="Economic" & list$neighbourhood_group=="Staten Island"],list$number_of_reviews[list$pricecat=="Economic" & list$neighbourhood_group=="Staten Island"])
##-0.1029916
cor(list$price[list$pricecat=="Deluxe" & list$neighbourhood_group=="Staten Island"],list$number_of_reviews[list$pricecat=="Deluxe"  & list$neighbourhood_group=="Staten Island"])
#-0.2493094
#Inference -  Economic and Deluxe price category in Staten Island Neighborhood group, 
#the number of reviews are decreasing with increased prices

#HYPOTHESIS 2 - The number of reviews is higher for the listings hosted in Fall or Summer season.

#creating factor levels of date into four season categories
list$last_review<-as.Date(list$last_review)
list$last_review
tmp <-  as.Date(list$last_review,'/%y%m/%d')
list$last_review_year<- format(tmp,'%Y')# year is selected out of the last review column
list$last_review_year<-as.factor(list$last_review_year)
list$last_review_month<- format(tmp,'%m')# the month is selected out from the last review column in the form of date


list$last_review_month<-as.numeric(list$last_review_month)
class(list$last_review_month)

list$Seasons[list$last_review_month==12]<-"Winter" 
list$Seasons[list$last_review_month==01]<-"Winter"
list$Seasons[list$last_review_month==02]<-"Winter"
list$Seasons[list$last_review_month==03]<-"Spring"
list$Seasons[list$last_review_month==04]<-"Spring"
list$Seasons[list$last_review_month==05]<-"Spring"

list$Seasons[list$last_review_month==06]<-"Summer"
list$Seasons[list$last_review_month==07]<-"Summer"
list$Seasons[list$last_review_month==08]<-"Summer"

list$Seasons[list$last_review_month==09]<-"Fall"
list$Seasons[list$last_review_month==10]<-"Fall"
list$Seasons[list$last_review_month==11]<-"Fall"
# segregation of months into seaosns


list$Seasons<- as.factor(list$Seasons)#seasons convereted into factor as we need to divide it into 4 seasons

#hypothesis testing
# null hypothesis- mean of number of reviews doesnt change with change in seasons
#i.e. mean of number of reviews is same for all seasons
#Alternate hypothesis- means of number of reviews is not same for all seasons
Season_Reviews.aov <- aov(list$number_of_reviews~list$Seasons, data=list)
Season_Reviews.aov
summary(Season_Reviews.aov) # Null rejected.p-value is too low Signif. codes:  0 '*' 0.001 '*' 0.01 '' 0.05 '.' 0.1 ' ' 1
#therefore, The number of reviews is dependent on the change in seasons

Season_Reviews.tk<-TukeyHSD(Season_Reviews.aov)

Fall_reviews<-(aggregate(list$number_of_reviews[list$Seasons=="Fall"] ~list$room_type[list$Seasons=="Fall"], FUN="mean"))
Winter_reviews<-(aggregate(list$number_of_reviews[list$Seasons=="Winter"] ~list$room_type[list$Seasons=="Winter"], FUN="mean"))
Summer_reviews<-(aggregate(list$number_of_reviews[list$Seasons=="Summer"] ~list$room_type[list$Seasons=="Summer"], FUN="mean"))
Spring_reviews<-(aggregate(list$number_of_reviews[list$Seasons=="Spring"] ~list$room_type[list$Seasons=="Spring"], FUN="mean"))
# calculation of number of reviews for each room type in the 4 seasons

boxplot(list$number_of_reviews~list$Seasons, data=list, col=2:4, xlab="Seasons") 
plotmeans(list$number_of_reviews~list$Seasons, xlab="Seasons", ylab="Number of Reviews", lwd=3, col="red", p=0.99)
identify(boxplot(list$number_of_reviews~list$Seasons, data=list, col=2:4, xlab="Seasons"))
# Boxplot shows that the median values for fall and summer is the highest
#bookings for Airbnb listings are observed for holiday seasons i.e. Fall and Summer

#inference-  Maximum number of reviews(bookings) for Airbnb listings are observed for holiday seasons i.e. Fall and Summer


#HYPOTHESIS 3 - The number of reviews for a listing is higher for a neighborhood situated in the 
#vicinity of a tourist destination

# null hypothesis- mean of number of reviews doesnt change with change in neighborhood
#i.e. mean of number of reviews is same for all neighborhoods
#Alternate hypothesis- means of number of reviews is not same for all neighborhoods 
Tourist_Destination.aov <- aov(list$number_of_reviews~list$neighbourhood, data=list)
Tourist_Destination.aov
summary(Tourist_Destination.aov)
#p- value is very low therefore we reject the null hypothesis
#Therefore, The number of reviews change with the change in neighborhood
# Brooklyn and Manhattan are the 2 top most tourist visited areas in new york  (refrence -https://www.tripadvisor.com/Travel-g60763-s204/New-York-City:New-York:Neighborhoods.html)
Brooklyn <- list[list$neighbourhood_group == "Brooklyn", ]
Brooklyn_reviews_sorted <- as.data.frame(aggregate(Brooklyn$number_of_reviews ~Brooklyn$neighbourhood, FUN="mean"))
Brooklyn_reviews_sorted <- Brooklyn_reviews_sorted[order(-Brooklyn_reviews_sorted$`Brooklyn$number_of_reviews`),]
Brooklyn_reviews_sorted

#top 3 neighborhoods of Brooklyn have been considered to check for tourist spots from internet

Manhattan <- list[list$neighbourhood_group == "Manhattan", ]
Manhattan_reviews_sorted <- as.data.frame(aggregate(Manhattan$number_of_reviews ~Manhattan$neighbourhood, FUN="mean"))
Manhattan_reviews_sorted <- Manhattan_reviews_sorted[order(-Manhattan_reviews_sorted$`Manhattan$number_of_reviews`),]
Manhattan_reviews_sorted
#top2 neighborhoods of Manhattan have been considered to check for tourist spots from internet
# Through internet we analysed that the top neighborhoods with the highest number of reviews were in the vicinity of tourist spots.
#Inference - The number of reviews for a listing is higher for a neighborhood situated in the vicinity of a tourist destination.


#REGRESSION
##Model 1 Number of reviews vs Independent variable(numerics)
first_mod<-lm(number_of_reviews~price+availability_365+minimum_nights,data=list)
summary(first_mod)
par(mfrow=c(2,2))
plot(first_mod)
##Adjusted R-squared:  0.02832 
##F-statistic: 216.8 on 3 and 21863 DF,  p-value: < 0.00000000000000022
## we have just 2.87% fitness which is very poor so we try to improve this model by including various other factors

##Model 2 Number of reviews vs Independent variable along with factors
mod2<- lm(number_of_reviews~price+availability_365+minimum_nights+room_type+neighbourhood_group, data = list)
summary(mod2)
plot(mod2)
## Adjusted R-squared:  0.0361  F-statistic: 91.98 on 9 and 21857 DF,  p-value: < 2.2e-16
## We have included our factors room_type and neighbourhood_group to the model to increase the 
##fitness of our model has increased from 2.87 to 3.61%

## Model 3 log transformation of number of reviews
mod3<- lm(log(number_of_reviews)~price+availability_365+minimum_nights+room_type+neighbourhood_group, data = list)
summary(mod3)
plot(mod3)
##Adjusted R-squared:  0.04508 F-statistic: 115.7 on 9 and 21857 DF,  p-value: < 0.00000000000000022
## After we use log value on number of reviews our model is now 4.45% from 3.6%

## Model 4 Converting last_review into factor called seasons
list$Seasons <- relevel(list$Seasons, ref=2)
mod4<- lm(log(number_of_reviews)~price+Seasons+availability_365+minimum_nights+room_type+neighbourhood_group, data = list)
summary(mod4)
plot(mod4)
##Adjusted R-squared:  0.1248 F-statistic: 260.8 on 12 and 21846 DF,  p-value: < 0.00000000000000022
## we can see that after adding the Seasons factor our adjusted R square value has drastically improved from
## 4.45% to 12.48 % that is fitness of our model has increased to 12.48%

##Model 5 Log Transformation of price
mod5<-lm(log(number_of_reviews)~log(price)+Seasons+availability_365+minimum_nights+room_type+neighbourhood_group, data = list)
summary(mod5)
plot(mod5)
##Adjusted R-squared:  0.125 after log transformation of price
##F-statistic: 261.3 on 12 and 21846 DF,  p-value: < 0.00000000000000022
## Also increase in F-statistic value indicates stronger evidence against the null hypothesis
## we also tried factoring price into 3 categories and including in the model it did not show any significant 
##change so we continued with log of price

##Model 6 Converting MinimumNights_Cat into factor
list$Seasons <- relevel(list$Seasons, ref=2)
list$minimum_nightcat <- list$minimum_nights
list$minimum_nightcat[list$minimum_nights >=10]<- "greater than or equal to 10"
list$minimum_nightcat <- as.factor(list$minimum_nightcat)

mod6<-lm(log(number_of_reviews)~log(price)+Seasons+availability_365+minimum_nightcat+room_type+neighbourhood_group,
         data = list)
summary(mod6)
summary(mod6)$adj.r.squared
plot(mod6)
## Adjusted R-squared:  0.138 
##F-statistic:   176 on 20 and 21838 DF,  p-value: < 0.00000000000000022
## fitness of our model has increased from 12.5% to 13.8%

## Model 7 Eliminating the outliers from bivariate analysis
##We tried with removing the outliers and observed that the adjusted R-squared value decreased and hence, sixth model was finalised as the BEST Model.

## Regression Diagnostics 
#=
plot(predict(mod6), residuals(mod6)) # we are looking for a "no pattern"/non-linearity 
par(mfrow=c(2,2)) 
plot(mod4)
# we can also find outliers this way 
plot(hatvalues(mod6)) 
identify(hatvalues(mod6), col="red")
tail(sort(hatvalues(mod6))) ## not sure how to go about this
sqrt(vif(mod6)) > 2
##This test confirmed that our regression model is safe and not affected by multicollinearity
##In conclusion we have improved our model fitness  from 2.876% to 13.8% that is 379.83% increase