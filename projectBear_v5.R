
########################################################################################################################
# PROJECT BEAR - PURPOSE AND VERSION NOTES
########################################################################################################################
# since we can easily tell which brands a store carries with the brand appender and we can tell where each brand
# ranks by category, we should be able to point to the terrible brands and suggest to buyers that they should swap
# them out for the brands we carry

# V1: The brandAssignmer is working.  Still trying to get the menu append operational
# V2: Added in the next step involving reading the brandStats
# V3: Added in code to create brand performance metrics as well as a quick chocolate project for Paul
#     Added in code to merge the headset brand performance metrics with the [censored] menu! It works!
# V4: Updated brandAssigner to be based on the [censored] menu version (much faster)
# V5: With the updated brand assignment algorithm, there is no need to have a separate brand for 'brand'
#       This being the case, V5 removes all brand assignment steps and replaces 'headsetBrand' with 'brand'

########################################################################################################################
# READ MENU, LOOKUP, AND LOAD PACKAGES
########################################################################################################################

library(dplyr)
library(anytime)
library(lubridate)

menu <- read.csv("menu_wip8_2017-08-30.csv")

# brandStats has the brand as a lookupid and contains performance metrics
brandData <- read.csv("headset_2017DB_09.csv")

# we'll really only need to do this for WA
menuWa <- menu[which(menu$state=="WA"),]

########################################################################################################################
# CREATE BRAND HEALTH METRICS
########################################################################################################################

# what are the metrics I'm interested in? I guess I want lots of rankings and then change over time
# what are the columns I'd need? brand, month, sales rank, unit rank, category sales rank, category unit rank
# then I want a separate table that summarizes brand from the table above and gives current month rank, change since Jan, and QoQ
# don't I also want percent of sales

brandData$month <- as.Date(brandData$month, format = "%m/%d/%Y")

brandToplineSummary <- tbl_df(brandData) %>%
  group_by(month, brand) %>%
  summarise(sales = sum(sales), units = sum(units)) %>%
  mutate(salesRank = dense_rank(desc(sales)), unitRank = dense_rank(desc(units)), pct_sales = sales/sum(sales), pct_units = units/sum(units)) %>%
  arrange(month, desc(sales))

# add in a category grouping variable so we can look within categories
brandCatSummary <- tbl_df(brandData) %>%
  group_by(month, category, brand) %>%
  summarise(sales = sum(sales), units = sum(units)) %>%
  arrange(month, category, desc(sales)) %>%
  mutate(salesRank = dense_rank(desc(sales)), unitRank = dense_rank(desc(units)), pct_sales = sales/sum(sales), pct_units = units/sum(units))

# great, now what we need is changes over time. let's start with the all-up

# Sales MoM
brandToplineSummary$sales_MoM <- NA
for(i in 1:nrow(brandToplineSummary)){
  tryCatch({
    if(month(brandToplineSummary$month[i])>1){
    brandToplineSummary$sales_MoM[i] <- brandToplineSummary$sales[i]/
      brandToplineSummary$sales[
        which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                brandToplineSummary$month==brandToplineSummary$month[i]-months(1))]-1
    }
  }, error = function(e){})
}
# sales rank MoM
brandToplineSummary$salesRank_MoM <- NA
for(i in 1:nrow(brandToplineSummary)){
  tryCatch({
    if(month(brandToplineSummary$month[i])>1){
      brandToplineSummary$salesRank_MoM[i] <- brandToplineSummary$salesRank[i] -
        brandToplineSummary$salesRank[
          which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                  brandToplineSummary$month==brandToplineSummary$month[i]-months(1))]
    }
  }, error = function(e){})
}


# now do the same for units sold and unit rank
brandToplineSummary$units_MoM <- NA
for(i in 1:nrow(brandToplineSummary)){
  tryCatch({
    if(month(brandToplineSummary$month[i])>1){
      brandToplineSummary$units_MoM[i] <- brandToplineSummary$units[i]/
        brandToplineSummary$units[
          which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                  brandToplineSummary$month==brandToplineSummary$month[i]-months(1))]-1
    }
  }, error = function(e){})
}

brandToplineSummary$unitRank_MoM <- NA
for(i in 1:nrow(brandToplineSummary)){
  tryCatch({
    if(month(brandToplineSummary$month[i])>1){
      brandToplineSummary$unitRank_MoM[i] <- brandToplineSummary$unitRank[i] -
        brandToplineSummary$unitRank[
          which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                  brandToplineSummary$month==brandToplineSummary$month[i]-months(1))]
    }
  }, error = function(e){})
}

# Sales QoQ for August
brandToplineSummary$sales_QoQ <- NA
for(i in 1:nrow(brandToplineSummary)){
  tryCatch({
    if(month(brandToplineSummary$month[i])==8){
      #add up the past 3 months
      brandToplineSummary$sales_QoQ[i] <- (
          
          brandToplineSummary$sales[i] +
          brandToplineSummary$sales[
            which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                    brandToplineSummary$month==brandToplineSummary$month[i]-months(1))] +
          brandToplineSummary$sales[
            which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                    brandToplineSummary$month==brandToplineSummary$month[i]-months(2))]        
      )/( #divide by the sum of the past 3 months
        brandToplineSummary$sales[
        which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                brandToplineSummary$month==brandToplineSummary$month[i]-months(3))] +
          brandToplineSummary$sales[
            which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                    brandToplineSummary$month==brandToplineSummary$month[i]-months(4))] +
          brandToplineSummary$sales[
            which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                    brandToplineSummary$month==brandToplineSummary$month[i]-months(5))] 
      )-1
    }
  }, error = function(e){})
}

# Jan-August change in sales
brandToplineSummary$sales_janToAug <- NA
for(i in 1:nrow(brandToplineSummary)){
  tryCatch({
    if(month(brandToplineSummary$month[i])==8){
      brandToplineSummary$sales_janToAug[i] <- brandToplineSummary$sales[i]/
        brandToplineSummary$sales[
          which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                  brandToplineSummary$month==brandToplineSummary$month[i]-months(7))]-1
    }
  }, error = function(e){})
}
# Jan-August change in sales rank
brandToplineSummary$salesRank_janToAug <- NA
for(i in 1:nrow(brandToplineSummary)){
  tryCatch({
    if(month(brandToplineSummary$month[i])==8){
      brandToplineSummary$salesRank_janToAug[i] <- brandToplineSummary$salesRank[i] -
        brandToplineSummary$salesRank[
          which(brandToplineSummary$brand==brandToplineSummary$brand[i] &
                  brandToplineSummary$month==brandToplineSummary$month[i]-months(7))]
    }
  }, error = function(e){})
}

######## This is where I can plug in more code later to start looking at it in terms of category

########################################################################################################################
# MENU MERGING
########################################################################################################################
# so what we need to do now is group the menu by dispensary and brand (after applying the brand appender)
# then, we simply merge in the brandToplineSummary data and order in order to see their worst brands right away

# start by grouping and summarizing based on dispensary and brand
menuWaSummary <- tbl_df(menuWa) %>%
  group_by(dispensary, brand) %>%
  summarize(itemCount = n()) %>%
  merge(brandToplineSummary[which(brandToplineSummary=="2017-08-01"),], by.x = "brand", by.y = "brand") %>%
  arrange(dispensary, desc(salesRank_janToAug))

write.csv(menuWaSummary, "projectBear_v01.csv", row.names = F)

# this version is for Shaurya who wants all months
menuWaSummary <- tbl_df(menuWa) %>%
  group_by(dispensary, brand) %>%
  summarize(itemCount = n()) %>%
  merge(brandToplineSummary, by.x = "brand", by.y = "brand") %>%
  arrange(dispensary, desc(salesRank_janToAug))

write.csv(menuWaSummary, "projectBear_allMonths_v01.csv", row.names = F)

