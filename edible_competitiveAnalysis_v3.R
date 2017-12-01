########################################################################################################################
# EDIBLES COMPETITIVE ANALYSIS - PURPOSE AND VERSION NOTES
########################################################################################################################

# purpose: answer key questions about the edible competitive landscape.
#   In particular, for each subcategory (brownies, chew/gummies, chocolate, cookies, hard candy, mints), I want to know
#   Who the top brands are with performance metrics.  I want to know which products are doing best and by size or any other
#   relevant sub-subcategory.  I want to know number of SKUs and even tier of prices, if I can.  Basically, I want this to 
#   be everything I could want to know to spot opportunities and plan for new products.

# V1: didn't append V1 name, but I'm setting this up
# V2: added in a data cleansing section to take care of removing outliers per subcat/mg_total/pack_size
#         Also, fixed pack size issues (but not all) and added outlier flag instead of removing
# V3: There was a follow-up ask to look at flavor data so I created a lookup (to avoid the ugly append lines)

########################################################################################################################
# READ DATA AND LOAD PACKAGES
########################################################################################################################

# data comes from a monthly category export from headset
products <- read.csv("headset_2017DB.csv")
products$month <- as.Date(products$month, format = "%m/%d/%Y")

########################################################################################################################
# BUILD DATA FEATURES FOR EDIBLES
########################################################################################################################

edibles <- products[which(products$category=="edible"),]

# first thing is first, we need to append subcategory
edibles$subcategory <- NA
for(i in 1:nrow(edibles)){
  if(grepl("brownie",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "brownie" 
  if(grepl("gummy",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy" 
  if(grepl("mint",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "mint"
  if(grepl("chocolate",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate" 
  if(grepl("cookie",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "cookie" 
  if(grepl("hard candy",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "hard candy"
  if(grepl("kooki",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "cookie"
  if(grepl("fruit chew",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("marmas",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("panda candies",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("caramel",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "caramels, chews, and taffy"
  if(grepl("taffy",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "caramels, chews, and taffy"
  if(grepl("weedtart",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "hard candy"
  if(grepl("pebbles",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "hard candy"
  if(grepl("zootrocks",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "hard candy"
  if(grepl("fruit gems",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("\\blush\\b",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("couch potato",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate" 
  if(grepl("peanut butter cup",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate" 
  if(grepl("chewees",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "caramels, chews, and taffy"
  if(grepl("juicy candies",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("munchie pack",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "cookie"
  if(grepl("sour drops",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "hard candy"
  if(grepl("illumination",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("pastille",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "mint"
  if(grepl("panda candy",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("raspberry chewy",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("pebble",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "hard candy"
  if(grepl("ginger snap",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "cookie"
  if(grepl("sa'more",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate"
  if(grepl("juicy 10-pack",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("juicy pieces",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("blue balls",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate"
  if(grepl("malt balls",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate"
  if(grepl("cloud bar",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate"
  if(grepl("truffle",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate"
  if(grepl("choco bite",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate"
  if(grepl("coconut snowball",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate"
  if(grepl("powdered thc",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "sweetener"
  if(grepl("fruit gem",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
  if(grepl("pearlz",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "hard candy"
  if(grepl("creamsicle bar",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "chocolate"
  if(grepl("fruit jellies",tolower(edibles$product[i]))==TRUE) edibles$subcategory[i] <- "gummy"
}

# 1st pass: 45% subcategory assignment rate on the first pass and 99% true positive rate. Seems I just need to bring down false negatives
# 2nd pass: 66% subcat assignment
# 3rd pass: 72% assignment overall
# 4th pass is 74.4%

# now append mg with regex
edibles$mg_total <- NA
for(i in 1:nrow(edibles)){
  edibles$mg_total[i] <- as.numeric(gsub(".*[ \\(]([0-9]+) *mg.*","\\1",tolower(edibles$product[i])))
}

# pack size I know there are some packs that have less than 10mg per unit, but let's first coat them all in the general
# rule that 10mg is the standard, then we'll look at special cases where the pack size means there is less mg per
edibles <- mutate(edibles, pack_size = mg_total/10)

edibles$pack_size_v2 <- NA
for(i in 1:nrow(edibles)){
  edibles$pack_size_v2[i] <- as.numeric(gsub(".*[^0-9]([0-9]+)(-| pack).*","\\1",tolower(edibles$product[i])))
  if(grepl("juicy.*pieces",tolower(edibles$product[i]))==TRUE) edibles$pack_size_v2[i] <- 10 #make exception catcher for pieces variant
}

# now, in cases where there is a pack v2, replace the first pack with that, then delete the column
# probably a better way to do this, but it'll work
for(i in 1:nrow(edibles)){
  if(is.na(edibles$pack_size_v2[i])==F) edibles$pack_size[i] <- edibles$pack_size_v2[i]
}
edibles <- edibles[,-12]

# create column for mg per unit
edibles <- mutate(edibles, mg_per = round(mg_total/pack_size,2))

# lets not do the analysis yet and continue with building data features.  In particular, I'm interested in CBD and 
# pulling out the CBD mg and ratio
# cbd <- edibles[which(grepl("cbd", tolower(edibles$product))),]
# it looks like whnever there isn't a "1:1" or "10:1", it's 100% CBD (or as close as possible)

# append CBD ratio
edibles$cbd_ratio <- NA
for(i in 1:nrow(edibles)){
  if(grepl("[0-9]:",edibles$product[i])==T)
    edibles$cbd_ratio[i] <- paste0("\'",gsub(".*[^0-9]([0-9]+:[0-9]+)[^0-9].*","\\1",tolower(edibles$product[i]))) #pasting a ' so that excel doesnt convert to time
}

# append sugar-free
edibles$sugarFree_flag <- NA
for(i in 1:nrow(edibles)){
  if(grepl("sugar-free",tolower(edibles$product[i]))==T) edibles$sugarFree_flag[i] <- 1 else edibles$sugarFree_flag[i] <- 0
}

for(i in 1:nrow(edibles)){
  if(grepl("sugar free",tolower(edibles$product[i]))==T) edibles$sugarFree_flag[i]
}

# append gluten-free flag
edibles$glutenFree_flag <- NA
for(i in 1:nrow(edibles)){
  if(grepl("gluten-free",tolower(edibles$product[i]))==T) edibles$glutenFree_flag[i] <- 1 else edibles$glutenFree_flag[i] <- 0
}
for(i in 1:nrow(edibles)){
  if(grepl("gluten free",tolower(edibles$product[i]))==T) edibles$glutenFree_flag[i] <- 1
} #trying this since it's dumb

# append vegan flag
edibles$vegan_flag <- NA
for(i in 1:nrow(edibles)){
  if(grepl("vegan",tolower(edibles$product[i]))==T) edibles$vegan_flag[i] <- 1 else edibles$vegan_flag[i] <- 0
}

# add average adjusted retail (1.45x) and avg markup
edibles <- mutate(edibles, avg_adj_retail = 1.45*avg_retail, avg_markup = avg_adj_retail/avg_wholesale)

#reorder
edibles <- edibles[,c(7,9,8,2,1,3,4,5,6,17,18,10:16)]



########################################################################################################################
# OUTLIER IDENTIFICATION
########################################################################################################################

# mk, so we'll target $15 at the cutoff for it-has-to-be-a-pack-above-this threshold
# the outlier removal process below should take care of most problematic pack sizes, but I do want to address
# the high-impact incorrect pack sizes

# the outlier removal below should tackle most of what I'm hoping to clean, but I still want to ensure we're not blind
# to the top-sellers.  Like, if a product is in the top 20 of a subcat, let's not scrub it out, lets just correct the pack size
# the first thing to note is that the majority of bad pack assignments are for 10mg that should be larger packs; it's not the other way

# on second thought, I'm just going to refine the pack assignment (above) and use the outlier flag. In excel, if I see a data point that I want to include, I'll just change it there

### REMOVE OUTLIERS ###
# basic idea is  x[!x %in% boxplot.stats(x)$out]

# loop to remove outliers
# needs to identify every subcategory/mg_total/pack_size combination and remove outliers from every grouping

# create a table of combos to loop through
combos <- edibles %>%
  group_by(subcategory, mg_total, pack_size) %>%
  summarize(itemCount = n())

# the following loop creates an index of outliers for each subcat/mg_total/pakc_size combo
edibles$id <- 1:nrow(edibles)

allOutliers <- as.numeric(NA)
for(i in 1:nrow(combos)){
  ediblesSubset <- edibles[which(edibles$subcategory==combos$subcategory[i] & 
                                         edibles$mg_total==combos$mg_total[i] & 
                                         edibles$pack_size==combos$pack_size[i]),]
  ediblesSubsetIds <- edibles$id[which(edibles$subcategory==combos$subcategory[i] & 
                                   edibles$mg_total==combos$mg_total[i] & 
                                   edibles$pack_size==combos$pack_size[i])]
  subsetOutliers <- boxplot.stats(edibles$avg_adj_retail[which(edibles$id %in% ediblesSubsetIds)])$out
  subsetOutlierIds <- ediblesSubsetIds[which(ediblesSubset$avg_adj_retail %in% subsetOutliers)]
  
  allOutliers <- c(allOutliers, subsetOutlierIds)
}

allOutliers <- na.omit(allOutliers) #remove initial NA

edibles$outlier_flag <- NA
for(i in allOutliers){
  edibles$outlier_flag[i] <- 1
}

for(i in 1:nrow(edibles)){
  if(is.na(edibles$outlier_flag[i])==TRUE) edibles$outlier_flag[i] <- 0
}

# edibleIns <- edibles[-allOutliers,] #remove outliers
# edibleOuts <- edibles[allOutliers,]

write.csv(edibles, "headset_edibles_2017_monthly.csv", row.names = F)

########################################################################################################################
# CREATE DATA FEATURE FOR FLAVOR AND ANALYZE
########################################################################################################################

# read flavors list, which has the input variations and output, which takes care of mispellings/plurality
flavors <- read.csv("flavors.csv")

# loop through append
edibles$flavor <- NA
for(i in 1:nrow(edibles)){
  for(j in 1:nrow(flavors)){
    if(grepl(paste0("\\b",flavors$flavor_input[j],"\\b"), tolower(edibles$product[i]))==T) edibles$flavor[i] <- as.character(flavors$flavor_output[j])
  }
}

write.csv(edibles, "headset_edibles_2017_monthly_flavorAppended.csv", row.names = F)

########################################################################################################################
# ANALYZE RESULTS
########################################################################################################################

# group by subcat/unit/brand

ediblesSummary <- edibles %>%
  group_by(subcategory,mg_total,brand) %>%
  summarize(itemCount = n(), units = sum(units), sales = sum(sales), avg_wholesale = mean(avg_wholesale), avg_retail = mean(avg_retail),
            avg_adj_retail = mean(avg_adj_retail))

# might want to create a weighted average that takes into account the units sold so low-selling products dont skew the average

write.csv(ediblesSummary, "ediblesSummary.csv", row.names = F)

# write a function to plot a histogram of the results
library(ggplot2)
par(mfrow = c(1,1))
hister <- function(subcategory,mg_total){
  ggplot(data = edibles[which(edibles$subcategory==subcategory & edibles$mg_total==mg_total),], 
         aes(edibles$avg_adj_retail[which(edibles$subcategory==subcategory & edibles$mg_total==mg_total)])) +
    geom_histogram() +
    geom_density()
}












