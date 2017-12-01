########################################################################################################################
# NOTES
########################################################################################################################
# Purpose: use the ggmap package to create maps of competitor locations

########################################################################################################################
# MENU CLEANING
########################################################################################################################

# start with menu.wip8

edibles<-menu.wip8[menu.wip8$category=="Edible",] #160,884 menu items

# test<-menu.wip8[which(grepl("[brand 4]",menu.wip8$all_details)),] # one item
# I checked out their website and where they claim to sell product, but the [censored company] menu shows no [brand 4]
# https://www.[brand 4].com/stockists/
# I checked a few to no avail; looks like they do a lot of deliveries


# let's assign brand first, then break out the sizes
edibles$brand <- NA
for(i in 1:nrow(edibles)){
  if(grepl("[brand 1]",tolower(edibles$product_name[i]))==TRUE) edibles$brand[i] <- "[brand 1]" 
  if(grepl("[brand 2]",tolower(edibles$product_name[i]))==TRUE) edibles$brand[i] <- "[brand 2]" 
  if(grepl("[brand 3]",tolower(edibles$product_name[i]))==TRUE) edibles$brand[i] <- "[brand 3]" 
  if(grepl("[brand 4]",tolower(edibles$product_name[i]))==TRUE) edibles$brand[i] <- "[brand 4]" 
}

ediblesBrand <- edibles[!is.na(edibles$brand),] 
# get rid of the hidden rows too
ediblesBrand <- ediblesBrand[ediblesBrand$hidden=="f",] #2847 menu items
# limit to CA stores
ediblesBrand <- ediblesBrand[ediblesBrand$state=="CA",] #2456 menu items


ediblesBrand$mg <- NA
# this is a bit different than the THC appender since it isn't looking for XXXmg, but a specific amount
# this beign the case, I think we can just grepl it
for(i in 1:nrow(ediblesBrand)){
  if(grepl("^.*[^0-9]+180[^0-9]+",ediblesBrand$product_name[i])==TRUE) ediblesBrand$mg[i] <- "180mg"
  if(grepl("^.*[^0-9]+60[^0-9]+",ediblesBrand$product_name[i])==TRUE) ediblesBrand$mg[i] <- "60mg"
  if(grepl("^.*[^0-9]+45[^0-9]+",ediblesBrand$product_name[i])==TRUE) ediblesBrand$mg[i] <- "45mg"
}

# convert lat and long to numeric
ediblesBrand$lat <- as.numeric(as.character(ediblesBrand$lat))
ediblesBrand$long <- as.numeric(as.character(ediblesBrand$long))
# get rid of NAs and invalid lat longs
ediblesBrand <- ediblesBrand[!is.na(ediblesBrand$lat),]
ediblesBrand <- ediblesBrand[ediblesBrand$long<=-100,]
ediblesBrand$brand <- as.factor(ediblesBrand$brand) #convert to factor

write.csv(ediblesBrand,"edibleCompetitors_analysis.csv", row.names = F)

########################################################################################################################
# MAPPING
########################################################################################################################
library(RColorBrewer)
library(ggmap)
# generate bounding box
ca_bbox <- make_bbox(lat = lat, lon = long, data = ediblesBrand)
# some changes to the box to make it look better
ca_bbox[[1]] <- -125.000
ca_bbox[[2]] <- 30.0000
ca_bbox[[3]] <- -113.0890
ca_bbox[[4]] <- 45.5000

# grab map from google
ca_big <- get_map(location = ca_bbox, source = "google", maptype = "roadmap")
# plot the points and color!
myColors <- brewer.pal(5,"Set1")
colScale <- scale_color_manual(name = "grp", values = myColors)

ggmap(ca_big) +
  geom_point(data = ediblesBrand, mapping = aes(x = long, y= lat, color = brand, size = 10)) +
  colScale

# individual brands
ggmap(ca_big) +
  geom_point(data = ediblesBrand[ediblesBrand$brand=="[brand 1]",], mapping = aes(x = long, y= lat, color = brand, size = 10)) +
  colScale

ggmap(ca_big) +
  geom_point(data = ediblesBrand[ediblesBrand$brand=="[brand 2]",], mapping = aes(x = long, y= lat, color = brand, size = 10)) +
  scale_color_manual(values="blue")

ggmap(ca_big) +
  geom_point(data = ediblesBrand[ediblesBrand$brand=="[brand 3]",], mapping = aes(x = long, y= lat, color = brand, size = 10)) +
  scale_color_manual(values="green")


########################################################################################################################
# MAPPING CURRENT CA CLIENTS
########################################################################################################################
# Data came from DOMO where I subset on Sales orders from May 1st through August 21.  I then cross-checked those stores
# against the [censored company] database to match where possible to cut down on manual efforts.  For the remainder of non-[censored company] stores, I
# Googled it and captured the lat/long.

caClients <- read.csv("2017-08_CA_dispensary_location_sales_data.csv")
colnames(caClients)[2] <- "sales"


library(RColorBrewer)
library(ggmap)
# generate bounding box
ca_bbox <- make_bbox(lat = lat, lon = long, data = caClients)
# same box as above
ca_bbox[[1]] <- -125.000
ca_bbox[[2]] <- 30.0000
ca_bbox[[3]] <- -113.0890
ca_bbox[[4]] <- 45.5000

ca_big <- get_map(location = ca_bbox, source = "google", maptype = "roadmap")
# plot the points and color!
myColors <- brewer.pal(5,"Set1")
colScale <- scale_color_manual(name = "grp", values = myColors)

ggmap(ca_big) +
  geom_point(data = caClients, mapping = aes(x = long, y= lat, size = 10)) +
  ggtitle("Current CA Clients")
