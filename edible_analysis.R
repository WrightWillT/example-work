################################################################################
# NOTES
################################################################################
# Purpose: using a variety of data sources, extract data relevant to edibles to 
#   find out which stores carry our product, what the composition of product
#   categories is per store, how that varies by market, and establish tiers
#   of chocolate brands based on selling price.

################################################################################
# READ AND PREP DATA/PACKAGES
################################################################################

library(dplyr)
# read latest menu
menu.wip8<-read.csv("menu_wip8_072417.csv", header = T)

# read lookup
lookup.502 <- read.csv("502_[censored]_stores_lookup_v4.csv",header = T) #382 rows with 341 having a [censored] match
colnames(lookup.502)[1]<-"[censored]_id" #fix name

# read sales data
sales <- read.csv("502_store_sales_v2.csv", header = T)

menu.502<-menu.wip8[menu.wip8$dispensary_id %in% lookup.502$[censored]_id_truncated,]

################################################################################
# Menu Category Compositions and Sales
################################################################################
# get a count of each category per store
comp <- tbl_df(menu.wip8) %>%
  group_by(dispensary_id,dispensary,state,city,lat,long,category) %>%
  summarize(cat_count = n())

# create category composition
comp$cat_comp<-NA
for(i in 1:nrow(comp)){
  comp$cat_comp[i] <- round(comp$cat_count[i]/sum(comp$cat_count[comp$dispensary_id==comp$dispensary_id[i]])*100,2)
}

# subset by those ids which are in my [censored]/502 list; don't use WA, just use the list

waComp <- comp[comp$dispensary_id %in% lookup.502$[censored]_id_truncated,]
# add lcb id
waComp <- merge(waComp,lookup.502, by.x = "dispensary_id", by.y = "[censored]_id_truncated")
# add sales (keep all of the stores from the sales doc so we can see all 382)
waStoreDetails <- merge(sales, waComp, by.x = "lcb_id", by.y = "lcb_id", all.x = T )
# clean this up a bit since i grabbed a bunch o stuff i dont need
waStoreDetails <- waStoreDetails[,c(1,4,2,7:12,3)]


waEdibleComp <- subset(waStoreDetails, waStoreDetails$category == "Edible" | is.na(waStoreDetails$category)==T)
waEdibleComp <- waEdibleComp[order(-waEdibleComp$sales_2017),]

caEdibleComp <- comp[comp$state=="CA" & comp$category == "Edible",]
caEdibleComp <- caEdibleComp[order(-caEdibleComp$cat_comp),]


################################################################################
# find the stores with high edible volume, but not carrying [censored]
################################################################################
# need a flag for carrying The [censored] from menu.wip8
# then join on dispensary id

waEdibleComp$carry[censored] <- NA
for(i in 1:nrow(waEdibleComp)){
  menu.temp<-subset(menu.wip8, menu.wip8$dispensary_id==waEdibleComp$dispensary_id[i])
  if(sum(grepl("[censored]", menu.temp$brand))>0) waEdibleComp$carry[censored][i] <- 1
  else waEdibleComp$carry[censored][i] <- 0
}

# potential issue: there are cases where they may have had [censored] a long time ago, but no longer
# in order to identify, I'd need to subset by the flag for hidden or deleted or whatever

################################################################################
# find stores with high sales of competitors ([competitor 1], [competitor 2], [competitor 3])
################################################################################
waEdibleComp$carry[competitor 1] <- NA
for(i in 1:nrow(waEdibleComp)){
  menu.temp<-subset(menu.wip8, menu.wip8$dispensary_id==waEdibleComp$dispensary_id[i])
  if(sum(grepl("[competitor 1]", menu.temp$brand))>0) waEdibleComp$carry[competitor 1][i] <- 1
  else waEdibleComp$carry[competitor 1][i] <- 0
}

waEdibleComp$carryEvergreenHerbal <- NA
for(i in 1:nrow(waEdibleComp)){
  menu.temp<-subset(menu.502, menu.502$dispensary_id==waEdibleComp$dispensary_id[i])
  if(sum(grepl("[competitor 2]", menu.temp$all_details))>0) waEdibleComp$carryEvergreenHerbal[i] <- 1
  else waEdibleComp$carryEvergreenHerbal[i] <- 0
}


#for [competitor 3], i dont have it in the brand library so I'm going to attempt to parse it from the "all_details" field
waEdibleComp$carry[competitor 3] <- NA
for(i in 1:nrow(waEdibleComp)){
  menu.temp<-subset(menu.wip8, menu.wip8$dispensary_id==waEdibleComp$dispensary_id[i])
  if(sum(grepl("[competitor 3]", menu.temp$all_details))>0) waEdibleComp$carry[competitor 3][i] <- 1
  else waEdibleComp$carry[competitor 3][i] <- 0
}

#add a flag for if any competitors are sold
for( i in 1:nrow(waEdibleComp)){
  if((waEdibleComp$carry[competitor 1][i] + waEdibleComp$carryEvergreenHerbal[i] + waEdibleComp$carry[competitor 3][i])>0) waEdibleComp$anyCompetitorFlag[i] <- 1 
  else waEdibleComp$anyCompetitorFlag[i] <- 0
}

write.csv(waEdibleComp, "wa_stores_edible_analysis_v1.csv", row.names = F)

################################################################################
# Pricing differences by geo
################################################################################
# Are there areas where premium edibles sell better? think urban/rural maybe
# lets plan on grouping by city? county? i dont really want to tag urban/suburban for all the cities.. how many are there?
# can i create a lookup for population density?
# I can recycle the code I used for the gram analysis of inside the i-5 corridor to outside maybe?
# I can create a mapping table for city to pop density and then make a flag for above a certain amount = urban?
# let's just do the right grouping first, then decide if its worth the extra effort. keep in mind you have other deadlines

geoView <- tbl_df(menu.wip8) %>%
  filter(category=="Edible") %>%
  group_by(city ,state, subcategory) %>%
  summarize(latitude = first(lat), longitude = first(long), count = n(), avg_price = round(mean(price),2)) %>%
  filter(count>=30)

waGeoView<-subset(geoView, geoView$state=="WA")

# now let's look at just chocolate and see what the distribution looks like in leaflet maybe?
waGeoChoc <- subset(waGeoView, waGeoView$subcategory=="Chocolate")
waGeoChoc$latitude <- as.numeric(as.character(waGeoChoc$latitude))
waGeoChoc$longitude <- as.numeric(as.character(waGeoChoc$longitude))

# let's also turn prices into a 5 point scale for very low, low, average, high, and very high
# use quantile? I think? and mutate avg_price in a for loop?

quantile1 <- quantile(waGeoChoc$avg_price, probs = .2)
quantile2 <- quantile(waGeoChoc$avg_price, probs = .4)
quantile3 <- quantile(waGeoChoc$avg_price, probs = .6)
quantile4 <- quantile(waGeoChoc$avg_price, probs = .8)


# function to stuff in the for loop that takes the average price and outputs 1 to 5 based on tier
tierer<-function(x){
  if(x<quantile1) print("1_Lowest")
  else if(x<quantile2) print("2_Low")
  else if(x<quantile3) print("3_Average")
  else if(x<quantile4) print("4_High")
  else print("5_Highest")
}

waGeoChoc$price_tier <- NA
for(i in 1:nrow(waGeoChoc)){
  waGeoChoc$price_tier[i]<-tierer(waGeoChoc$avg_price[i])
}
# convert to factor
# waGeoChoc$price_tier <- as.factor(waGeoChoc$price_tier)

library(RColorBrewer)
library(ggmap)
# generate bounding box
wa_bbox <- make_bbox(lat = latitude, lon = longitude, data = waGeoChoc)
# grab map from google
wa_big <- get_map(location = wa_bbox, source = "google", maptype = "roadmap")
# plot the points and color!
myColors <- brewer.pal(5,"Set1")
colScale <- scale_color_manual(name = "grp", values = myColors)

ggmap(wa_big) +
  geom_point(data = waGeoChoc, mapping = aes(x = longitude, y= latitude, color = price_tier, size = 10)) +
  colScale

write.csv(waGeoChoc, "chocolate_prices_wa.csv", row.names = F)
#did box analysis in excel
waGeoChocBox <- read.csv("chocolate_prices_wa_withBox.csv", header = T)
# test if different price in and out of box
t.test(waGeoChocBox$avg_price[waGeoChocBox$inBox==1],waGeoChocBox$avg_price[waGeoChocBox$inBox==0])

