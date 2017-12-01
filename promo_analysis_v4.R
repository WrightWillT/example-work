########################################################################################################################
# COMPETITOR PROMO ANALYSIS
########################################################################################################################

# PURPOSE:      To understand how competitors do promotions; frequency, duration, number of products, dates, holidays, etc.
# METHODOLOGY:  After pulling weekly data from [censored] for week-ending 9/4/16 through 9/3/17, my plan is to create
#               a model for each product trend and then calculate where the largest deviations from the E(X) are.  My
#               expectation is that we should be able to determine if those periods are promotional and investigate.
# VERSION NOTES
# V1: Set up the basics
# V2: completed the matrix of residuals
# V3: starting to get into the weeds; exploring to answer which promos do best and if retailers pass on discounts or not
# V4: added in process to generate modeler() for wholesale
#     removed a bunch of exploratory work that wasn't doing anything and started over to focus on wholesale vs retail
#     moved all cleaning to the front of the line

########################################################################################################################
# READ DATA AND LOAD PACKAGES
########################################################################################################################

library(dplyr)
library(anytime)
library(forecast)
library(ggplot2)
library(tidyr)

# data is in two .csv files since it was over the 1MM limit for Excel as I was plugging this all together
dataPart1 <- read.csv("[censored]_2017DB_weekly.csv") # looks like there's an error with importing the col name
colnames(dataPart1)[1] <- "product"
dataPart2 <- read.csv("[censored]_2016DB_weekly.csv")

items <- rbind(dataPart1, dataPart2)
# convert week_ending to a date
items$week_ending <- as.Date(items$week_ending, format = "%m/%d/%Y")
items <- items[order(items$week_ending),] #sort on date

########################################################################################################################
# CREATE DATA FEATURES AND CLEAN DATA
########################################################################################################################

# let's just get rid of retail alltogether and just have the adjusted number
items <- mutate(items, retail = retail *1.45, sales = units * retail, revenue = units * wholesale)

items <- as.data.table(items)
items[, wholesale_wow := wholesale / shift(wholesale) - 1 , by = product]
items[, revenue_wow := revenue / shift(revenue) - 1 , by = product]
items[, retail_wow := retail / shift(retail) - 1 , by = product]
items[, sales_wow := sales / shift(sales) - 1, by = product]
items[, units_wow := units / shift(units) - 1, by = product]

########################################################################################################################
# CREATE TIME SERIES TABLES FOR VISUALIZATION
########################################################################################################################

# retail prices
retail <- items[,c(1,6,7)]
retail <- spread(retail, product, retail)
retail <- ts(retail[,-1], frequency = 52, start = c(2016, 35))

# wholesale prices
wholesale <- items[,c(1,5,7)]
wholesale <- spread(wholesale, product, wholesale)
wholesale <- ts(wholesale[,-1], frequency = 52, start = c(2016, 35))

# sales
sales <- items[,c(1,4,7)]
sales <- spread(sales, product, sales)
sales <- ts(sales[,-1], frequency = 52, start = c(2016, 35))

# revenue
revenue <- items[,c(1,8,7)]
revenue <- spread(revenue, product, revenue)
revenue <- ts(revenue[,-1], frequency = 52, start = c(2016, 35))

# units
units <- items[,c(1,3,7)]
units <- spread(units, product, units)
units <- ts(units[,-1], frequency = 52, start = c(2016, 35))


### NOT USEFUL
# in case I ever want to create the residuals and look at all of them at once (don't think this was very useful)
# retailFitRes <- matrix(NA,nrow = nrow(retailPrices), ncol = ncol(retailPrices))
# for(i in 1:ncol(retailFitRes)){
#   fit <- forecast(retailPrices[,i])
#   for(j in 1:nrow(retailFitRes)){
#     retailFitRes[j,i] <- residuals(fit)[j]
#   }
# }
# 
# # standardize residuals
# retailFitResSd <- matrix(NA,nrow = nrow(retailPrices), ncol = ncol(retailPrices))
# for(i in 1:ncol(retailFitResSd)){
#   retailFitResSd[,i] <- scale(retailFitRes[,i])
# }
#### END NOT USEFUL

########################################################################################################################
# CREATE VISUALIZER FUNCTION TO VIEW TIME SERIES DATA
########################################################################################################################

# create visual exploration tool which shows all the pertinent details over time for a product
# the purpose is so I can more easily understand what the  circumstances are for a particular product

# random product generator
randProduct <- function() sample(items$product,1)

visualizer <- function(itemName){
  par(mfrow = c(5,1))
  
  plot.ts(wholesale[,which(colnames(wholesale)==itemName)], main = paste0("Weekly Performance Metric Trends for ",itemName), ylab = "Wholesale Price")
  lines(fitted(forecast(wholesale[,which(colnames(wholesale)==itemName)])), col = "red") 
  
  plot.ts(retail[,which(colnames(retail)==itemName)], ylab = "Retail Price")
  lines(fitted(forecast(retail[,which(colnames(retail)==itemName)])), col = "red") 
  
  plot.ts(units[,which(colnames(units)==itemName)], ylab = "Units Sold")
  lines(fitted(forecast(units[,which(colnames(units)==itemName)])), col = "red")
  
  plot.ts(revenue[,which(colnames(revenue)==itemName)], ylab = "Revenue")
  lines(fitted(forecast(revenue[,which(colnames(revenue)==itemName)])), col = "red") 
  
  plot.ts(sales[,which(colnames(sales)==itemName)], ylab = "Sales")
  lines(fitted(forecast(sales[,which(colnames(sales)==itemName)])), col = "red") 
  
  par(mfrow = c(1,1)) # resets it
}



########################################################################################################################
# ANSWER QUESTIONS
########################################################################################################################

# QUESTION 1: DO RETAILERS PASS ON DISCOUNTS? IF THEY DO, THEN WHEN? IS IT A BRAND LEVEL THING?

plot(items$wholesale_wow, items$retail_wow)
# there are outliers in both direction of retail_wow and wholesale_wow that obfuscate any patten. Take a subset

itemsBoiled <- items[which(abs(items$wholesale_wow)<1 & 
                             abs(items$retail_wow)<1 &
                             abs(sales_wow)<10 &
                             abs(revenue_wow)<10),]
# 1.29M in itemsBoiled vs 1.44M in items
plot(itemsBoiled$wholesale_wow, itemsBoiled$retail_wow, 
     xlab = "Wholesale Prices WoW", 
     ylab = "Retail Prices WoW", 
     main = "How Changes in Wholesale Prices Affect Retail Prices")
# plot a line showing the relationship between the two variables
abline(lm(itemsBoiled$retail_wow~itemsBoiled$wholesale_wow), col = "blue", lwd = 3)

# plot a fitted line for wholesale wow less than 0 (discounts)
abline(lm(itemsBoiled$retail_wow[which(itemsBoiled$wholesale_wow<0)]~itemsBoiled$wholesale_wow[which(itemsBoiled$wholesale_wow<0)]), col = "red", lwd = 3)
# plot a fitted line for the wholesale and retail less than 0 (where the discount is passed on)
abline(lm(itemsBoiled$retail_wow[which(itemsBoiled$wholesale_wow<0 & itemsBoiled$retail_wow<0)]~itemsBoiled$wholesale_wow[which(itemsBoiled$wholesale_wow<0 & itemsBoiled$retail_wow<0)]), col = "blue", lwd = 3)
# plot a 1:1 line
abline(a = 0, b = 1, lwd = 3, col = "red")

top <- tbl_df(itemsBoiled) %>%
  group_by(brand) %>%
  summarize(total_sales = sum(sales)) %>%
  arrange(desc(total_sales)) %>%
  head(1)
top <- top[,1]

points(x = itemsBoiled$wholesale_wow[which(itemsBoiled$brand %in% top$brand)],
       y = itemsBoiled$retail_wow[which(itemsBoiled$brand %in% top$brand)], 
       col = "red")

abline(lm(itemsBoiled$retail_wow~itemsBoiled$wholesale_wow), lwd = 3)
abline(lm(itemsBoiled$retail_wow[which(itemsBoiled$brand %in% top$brand)]~itemsBoiled$wholesale_wow[which(itemsBoiled$brand %in% top$brand)]), col = "red", lwd = 3)

brandTest <- "The [censored] Company"
points(x = itemsBoiled$wholesale_wow[which(itemsBoiled$brand==brandTest)],
       y = itemsBoiled$retail_wow[which(itemsBoiled$brand==brandTest)], 
       col = "blue")
abline(lm(itemsBoiled$retail_wow[which(itemsBoiled$brand==brandTest)]~itemsBoiled$wholesale_wow[which(itemsBoiled$brand==brandTest)]), col = "blue", lwd = 3)

# would be interesting to see the top 5 brands' products in different colors, also, use ggplot

g <- ggplot(itemsBoiled, aes(x = wholesale_wow, y = retail_wow)) +
  geom_point()

# let's look just at "discounts" and not prices increases

plot(itemsBoiled$wholesale_wow[which(itemsBoiled$wholesale_wow<0)], itemsBoiled$retail_wow[which(itemsBoiled$wholesale_wow<0)], 
     xlab = "Wholesale Prices WoW", 
     ylab = "Retail Prices WoW", 
     main = "How Changes in Wholesale Prices Affect Retail Prices")


# ANSWER: NOT REALLY

# can I get a subset of items where most of the wholesale:retail WoW changes coincide? I'd like to use the visualizer on those products so I can maybe
# get a sense of well.. im not sure, it just feels right
# yeah, what % of cases does a drop in wholesale lead to a similar drop in retail?

# let's generate a table of unique items and the correlation between wholesale and retail.. and perhaps correlates between the other metrics too
itemsComplete <- na.omit(items)
itemsComplete <- itemsComplete[is.finite(itemsComplete$wholesale_wow),]
cor(itemsComplete$wholesale_wow, itemsComplete$retail_wow, method = "pearson") # 3% of the variations in retail price are explained by variations in wholesale price

itemsCor <- data.frame(product = unique(itemsComplete$product), cor = NA)
for(i in 1:100){
  itemsCor$cor[i] <- cor(itemsComplete$wholesale_wow[which(itemsComplete$product == itemsCor$product[i])], 
                         itemsComplete$retail_wow[which(itemsComplete$product == itemsCor$product[i])], method = "pearson")
}

# this takes FOREVER to run.  I think what I should maybe do instead is compare the top 100 products to products that are more middle-of-the-road or low-end
# what do you expect to see?  Products that perform better in terms of sales should have a higher correlation
# might be interesting to see this at the brand level too.  my hunch is that certain brands are better at getting retailers to offer the discount

top100 <- tbl_df(itemsComplete) %>%
  group_by(product) %>%
  summarize(total_sales = sum(sales)) %>%
  arrange(desc(total_sales)) %>%
  head(100)

top100$cor <- NA
for(i in 1:nrow(top100)){
  top100$cor[i] <- cor(itemsComplete$wholesale_wow[which(itemsComplete$product == top100$product[i])], 
                         itemsComplete$retail_wow[which(itemsComplete$product == top100$product[i])], method = "pearson")
}


mid100 <- tbl_df(itemsComplete) %>%
  group_by(product) %>%
  summarize(total_sales = sum(sales)) %>%
  arrange(desc(total_sales)) 

mid100 <- mid100[50001:50100,]

mid100$cor <- NA
for(i in 1:nrow(mid100)){
  mid100$cor[i] <- cor(itemsComplete$wholesale_wow[which(itemsComplete$product == mid100$product[i])], 
                       itemsComplete$retail_wow[which(itemsComplete$product == mid100$product[i])], method = "pearson")
}

# alright lets look at this by brand and make sure we're only considering wholesale discounts

# i wonder if we looked at the correlation between "promo" changes in prices (wholesale_wow<-0.15) if we'd see a higher correlation?
# let's do this, but only for the top 2000 products
top2000 <- tbl_df(itemsComplete) %>%
  group_by(product) %>%
  summarize(total_sales = sum(sales)) %>%
  arrange(desc(total_sales)) %>%
  head(2000)

top2000 <- itemsComplete[which(itemsComplete$product %in% top2000$product),]

top2000Promos <- top2000[which(top2000$wholesale_wow<=-0.15),]
cor(top2000Promos$wholesale_wow, top2000Promos$retail_wow, method = "pearson")
plot(top2000Promos$wholesale_wow, top2000Promos$retail_wow)
fit<-lm(top2000Promos$retail_wow~top2000Promos$wholesale_wow)
abline(fit)

# so basically, there is a negative correlation between the top 2000 products when they are having a 15% wholesale discount
# makes no sense

plot(itemsComplete$wholesale[itemsComplete$wholesale<200], itemsComplete$sales[itemsComplete$wholesale<200])

plot(itemsComplete$wholesale_wow[which(abs(itemsComplete$wholesale_wow)<3 & abs(itemsComplete$sales_wow)<8)], 
     itemsComplete$sales_wow[which(abs(itemsComplete$wholesale_wow)<3 & abs(itemsComplete$sales_wow)<8)],
     main = "Relationship Between Wholesale Prices WoW and Sales WoW",
     xlab = "Wholesale WoW",
     ylab = "Sales WoW")
abline(v = 0, col = "red", lwd = 3)
abline(h = 0, col = "red", lwd = 3)

plot(itemsComplete$wholesale_wow[which(abs(itemsComplete$wholesale_wow)<1 & abs(itemsComplete$sales_wow)<8 & abs(itemsComplete$units_wow)<15)], 
     itemsComplete$units_wow[which(abs(itemsComplete$wholesale_wow)<1 & abs(itemsComplete$sales_wow)<8 & abs(itemsComplete$units_wow)<15)],
     main = "Relationship Between Wholesale Prices WoW and Units Sold WoW",
     xlab = "Wholesale Price WoW",
     ylab = "Units Sold WoW")
abline(v = 0, col = "red", lwd = 1)
abline(h = 0, col = "red", lwd = 1)


length(itemsComplete$wholesale_wow[which(itemsComplete$wholesale_wow<0)]) #231267
length(itemsComplete$wholesale_wow[which(itemsComplete$wholesale_wow<0 & itemsComplete$units_wow>0)]) #112437

112437/231267 #48.6%

### stopping this analysis until I hear back from [censored] regarding when their wholesale price data is captured relative to retail price data

########################################################################################################################
# TESTING TO SEE WHAT THE DISTRIBUTION OF DELAY BETWEEN WHOLESALE AND RETAIL DISCOUNTS ARE
########################################################################################################################

# so [censored] got back to me and let me know that there is, in fact, a delay between the wholesale and retail.
# what this means is that we don't know exactly when the sell-in happens so when I see a table like 'items' with
# columns for retail and wholesale and date, the date is accurate for the retail price, but not the wholesale.
# An item could be sold at 20% off, sit on the shelf for a month, then sell when the wholesale promo discount is over.
# What I'd like to understand is if I can get a sense for the distribution of delay between wholesale and retail.  
# Basically, I have [censored] data for when they discount certain product subcategories and I'll just test to see when the prices
# in [censored] drop.

# Subcat to test: chocolates.  They did 20% off in February, May, and August

# Step 1: which [censored] products fall into the chocolates subcat?

[censored] <- tbl_df(items[which(items$brand=="The [censored] Company"),]) %>%
  group_by(product) %>%
  summarize()

[censored]Choc <- [censored][c(15,16,17,18,21,22),]
visualizer([censored]Choc$product[1])











