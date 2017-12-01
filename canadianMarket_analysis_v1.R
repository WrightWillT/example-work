########################################################################################################################
# SUBSET TO INDIVIDUAL MARKETS AND COMPARE
########################################################################################################################

library(dplyr)
library(RPostgreSQL)

menuCanada <- menu[which(menu$country=="CA"),] #86K
menuWa <- menu[which(menu$state=="WA"),] #273K
menuCa <- menu[which(menu$state=="CA"),] #416K


pw<-{
  "[censored]"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "[censored company]",
                 host = "[censored]", port = 5432,
                 user = "[censored]", password = pw)
rm(pw) 

dispensaries <- dbGetQuery(con,"SELECT * FROM dispensary")

# subset to Canada
dispensariesCanada <- dispensaries[which(dispensaries$country=="CA"),] #488
dispensariesCa <- dispensaries[which(dispensaries$state=="CA"),] #4742
dispensariesWa <- dispensaries[which(dispensaries$state=="WA"),] #1667

# clean out bad data for lat/long
dispensariesCanada <- dispensariesCanada[which(dispensariesCanada$latitude>=40 & dispensariesCanada$longitude<=-60),] #478



# generate a table to show how many dispensaries we have in each province
dispensariesByState <- as.data.frame(table(dispensariesCanada$state))
dispensariesByState$Freq <- as.numeric(dispensariesByState$Freq)
dispensariesByState$Var1 <- as.character(dispensariesByState$Var1)
dispensariesByState <- rbind(dispensariesByState, c("Total",sum(dispensariesByState$Freq)))
colnames(dispensariesByState) <- c("Province","Dispensary Count")

########################################################################################################################
# MAP OF CURRENT DISPENSARIES
########################################################################################################################

library(RColorBrewer)
library(ggmap)
# generate bounding box
ca_bbox <- make_bbox(lat = latitude, lon = longitude, data = dispensariesCanada, f= 0.1)
# some changes to the box to make it look better
ca_bbox[[1]] <- -140
ca_bbox[[2]] <- -60
ca_bbox[[3]] <- -128.7
ca_bbox[[4]] <- 57.75

# grab map from google
ca_big <- get_map(location = ca_bbox, source = "google", maptype = "roadmap")
# plot the points and color!
myColors <- brewer.pal(5,"Set1")
colScale <- scale_color_manual(name = "grp", values = myColors)

ggmap(ca_big) +
  geom_point(data = dispensariesCanada, mapping = aes(x = longitude, y= latitude, color = state, size = 10)) #+
  #colScale

ca_big <- get_openstreetmap(bbox = ca_bbox, urlonly = FALSE)
ca_big <- get_map(location = ca_bbox, source = "osm")
ggmap(ca_big) +
  geom_point(data = dispensariesCanada, mapping = aes(x = longitude, y= latitude, color = state, size = 10)) #+


get_openstreetmap(bbox = c(left = -95.80204, bottom = 29.38048, right =
                             -94.92313, top = 30.14344), scale = 606250, format = c("png", "jpeg",
                                                                                    "svg", "pdf", "ps"), messaging = FALSE, urlonly = FALSE,
                  filename = "ggmapTemp", color = c("color", "bw"))

########################################################################################################################
# ASSIGNING CANADIAN BRANDS
########################################################################################################################

library(foreach) # used in parallel processing
library(doSNOW) # used in parallel processing

timeBrand <- system.time({
  
  # read the canadian lookup for brands
  brandLookup <- read.csv("canadian_brandLookup_v2.csv")
  
  brandRegexMatch <- paste0("\\b",brandLookup$brandKey,"\\b")
  
  # function to assign brand based on selecting the highest confidence brand
  brandAssigner <- function(textInput){
    
    # create an index vector of matches
    matches <- which(sapply(brandRegexMatch, function(x) grepl( x, tolower(textInput), perl = T)))
    
    # if no matches to any brandKey, then assign NA to the brand, else select the best match
    ifelse(length(matches)<1, 
           assign("brandAssignment", NA, envir = .GlobalEnv),
           # assign brand to the first match with the maximum confidence
           assign("brandAssignment", as.character(brandLookup$brand[matches[
             which(brandLookup$brandKey_conf[matches]==max(brandLookup$brandKey_conf[matches]))[1]]], 
             envir = .GlobalEnv)))
    
    brandAssignment #output
  }
  
  # parallel processing
  cl <- makeCluster(8, type="SOCK") # for 8 core machine
  registerDoSNOW (cl)
  menuCanada$canadaBrand <- NA
  menuCanada$canadaBrand <- foreach(i = 1:nrow(menuCanada), .combine=c) %dopar% {
    brandAssigner(tolower(paste(menuCanada$name[i], menuBranded$Canada[i], sep = " ")))
  }
})

stopCluster(cl)

# RESULTS:
# CANADA
# TRUE POSITIVE: 100% in a 300 sample test
# Assignment rate: 8.4% (NA are 7215/85857)

# for comparison:
# WASHINGTON
# TRUE POSITIVE: 99.3% on last test of ~250
# Assignment rate: 46.5% (NA are 146099/273059)
# CALIFORNIA
# TRUE POSITIVE: 99.3% (joint test with WA)
# Assignment rate: 15.2% (NA are 352385/415738)

canadaBrandFreq<-as.data.frame(table(menuCanada$canadaBrand))
canadaBrandFreq <- canadaBrandFreq[order(desc(canadaBrandFreq$Freq)),]

# flower strain frequency
strainFreq <- as.data.frame(table(menuCanada$strain_key[which(menuCanada$category=="Flower")]))
strainFreq <- strainFreq[order(desc(strainFreq$Freq)),]


########################################################################################################################
# CROSS CATEGORY SUCCESS
########################################################################################################################
# I want to measure the degree to which brands are successful in multiple categories to identify how we should be 
# structuring our brands.  How to do this?  I want to look at the percentile of sales per product category in 2017 
# using Headset data. 

# start by pulling in the headset data for 2017 through september
headset <- read.csv("headset_2017DB_09.csv")
headset$month <- as.Date(headset$month, format = "%m/%d/%Y")

# now what we want is to group by brand and category, then generate a percentile of rank
library(tidyr)

headsetSummary <- tbl_df(headset) %>%
  group_by(brand,category) %>%
  summarize(total_sales = sum(sales)) %>%
  group_by(category) %>%
  mutate(cat_rank = row_number(-total_sales), cat_pctile = cat_rank/length(cat_rank))
  
headsetSummary <- headsetSummary[,c(1,2,5)] %>%
  group_by(brand) %>%
  spread(category, cat_pctile)

# I want to add in an overall brand rank so let's calculate that and join it on the brand

headsetBrandRanks <- tbl_df(headset) %>%
  group_by(brand) %>%
  summarize(total_sales = sum(sales)) %>%
  mutate(rank = row_number(-total_sales))

headsetSummary <- left_join(headsetSummary, headsetBrandRanks[,c(1,3)], by = "brand")
headsetSummary <- headsetSummary[,c(1,11,2:10)]
headsetSummary <- headsetSummary[order(headsetSummary$rank),]

# nice, now we want to see the delta in cat_pctiles between every category.  36 columns in total is what I should expect
# for the deltas.  What's the best way to do this?

# writing to excel to continue there
write.csv(headsetSummary, "headset_brand_category_percentiles.csv", row.names = F)

