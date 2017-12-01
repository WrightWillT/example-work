########################################################################################################################
# VERSION NOTES
########################################################################################################################

# V2: I replaced all menu.wip8 references with 'menu'

# Purpose: I want to write a function that takes a store name (or possibly id) and returns all the pertinent details about a store
# Basically, I received a request to do this for one store and figure that I should probably make this programmatic so we can quickly analyze any store
# The outputs I want are number of censored menu items in total with breakouts for each category/subcategory, avg price,
# and I want to be able to be able to cross check their menu items against the top-selling items by category in Headset
# maybe a view of the composition of items by category and a comparison to nearby stores (via distance within lat/long range)
# I'm getting ahead of myself.. let's just see if we can do the basics and build from there


library(dplyr)
# read the most recent menu into a 'menu' object

menu.stores<-tolower(unique(menu$dispensary)) # create list of stores to match against by converting to lower

# I want the result of this function to be  View() on many tables to give me an overview
storeInfo<-function(store){
  
  # Grab the data you need
  if(sum(grepl(store,menu.stores))>1) print("multi-match error") # we're trying to take user input (which could be leaving off parts of the name) and return the correct name; if there are multi-matches, it should fail
  store.name<-menu.stores[which(grepl(tolower(store), menu.stores))] #store the refined store name
  assign("store.menu", menu[which(tolower(menu$dispensary)==store.name & menu$hidden=="f"),], envir = .GlobalEnv)
  item.count<-nrow(store.menu)
  View(store.menu)
  
  # Category Composition
  cats<-tbl_df(store.menu) %>%
    group_by(category) %>%
    summarize(count = n(), pct = count/item.count*100) %>%
    arrange(desc(count))
  View(cats)
  
  # Brand Composition
  brands<-tbl_df(store.menu) %>%
    group_by(brand) %>%
    summarize(count = n(), pct=count/item.count*100) %>%
    arrange(desc(count))
  View(brands)  
  
  max.gram.price<-40 # set an upper bound for the mean price (lots of really dirty data)
  gram.price<-mean(na.omit(store.menu$price[
    store.menu$category=="Flower" 
    & store.menu$size_unit=="1.0g" 
    & store.menu$price<max.gram.price]))
  
}
  

#function to return menu items that contain something
# x is the item name/description
# menuItemList<-function(x) View(menu[grepl(x,menu$all_details)==T,])

####function to return the store ids of nearby competitors

# recycle distBetweenPoints to get relative distance between other dispensaries and the one in quesiton
distBetweenPoints<-function(x1,y1,x2,y2){
  a<-x2-x1
  b<-y2-y1
  c<-sqrt(a^2+b^2)
}

# create table of store ids, names, lats, and longs
stores.raw <- read.csv("stores_06292017.csv", header = T, stringsAsFactors = FALSE)
stores.raw$Id<-gsub("stores-","",stores.raw$Id)
store.list<-stores.raw[,c("Id","Name","Latitude","Longitude")]
#convert to lower
store.list$Name<-tolower(store.list$Name)
#convert to numeric
store.list$Latitude<-as.numeric(store.list$Latitude)
store.list$Longitude<-as.numeric(store.list$Longitude)
store.list<-na.omit(store.list) #get rid of NAs

## special case for point loma to remove the duplicates
store.list<-store.list[-c(9461,9463),]

compDist<-function(store){
  
  if(sum(grepl(store,store.list$Name))>1) print("multi-match error") # we're trying to take user input (which could be leaving off parts of the name) and return the correct name; if there are multi-matches, it should fail
  store.name<-store.list$Name[which(grepl(tolower(store), tolower(store.list$Name)))] #store the refined store name
  store.x<-store.list$Latitude[store.list$Name==store.name]
  store.y<-store.list$Longitude[store.list$Name==store.name]
  
  # calc latlong euclidean distances
  store.list$euc_dist<-NA
  for(i in 1:nrow(store.list)){
    store.list$euc_dist[i]<-distBetweenPoints(store.x,store.y,store.list$Latitude[i],store.list$Longitude[i])
  }
  #convert to km
  
  store.list$euc_dist_km<-NA
  for(i in 1:nrow(store.list)){
    store.list$euc_dist_km[i]<-round(store.list$euc_dist[i]*111,5) #111km in a degree of lat/long
  }
  
  #convert to mi
  store.list$euc_dist_mi<-NA
  for(i in 1:nrow(store.list)){
    store.list$euc_dist_mi[i]<-round(store.list$euc_dist_km[i]*0.621371,5) # km to mi ratio
  }
  
  assign("store.list.active",store.list[store.list$Name!=store.name,], envir = .GlobalEnv) #make this available outside the function
}

# The function that pulls it all together
closeComp<-function(store,dist=5){
  compDist(store)
  assign("store.near",store.list.active[store.list.active$euc_dist_mi<dist,], envir = .GlobalEnv)
  store.near.menu<-menu[menu$dispensary %in% store.near$Name,]
  assign("store.near.menu",store.near.menu, envir = .GlobalEnv)
}

# function to compare to dispensaries in the same city
cityComp<-function(city){
  assign("store.city.menu",menu[menu$city==city & !is.na(menu$category),], envir = .GlobalEnv) 
  assign("store.city.menu",store.city.menu[!is.na(store.city.menu$category),], envir = .GlobalEnv) #no idea why this had to be two steps but apparently it doesn't work at all in one line
}

# great, I just did some brainstorming on how to use this best and I think I want to 
# compare the store in question to the other nearby stores
# I'm going to aggregate the menus of all the nearby competition and then create
# a function that automagically compares two entities (either individual stores or groups of stores)
# then outputs comparison metrics for a cat/subcat compositions, prices, strains, brands

comparer<-function(menu1, menu2){
  #menu1 should be the menu of the first entity and menu2 of the second
  #I'm imagining a table of results which shows categories and column for composition of menu1 and menu2
  # and another table that compares brand composition and another table for etc.
  # the end result should be a View() or assignment of a global object that can be further analyzed
  
  # categories
  cats1<-tbl_df(menu1) %>%
    group_by(category) %>%
    summarize(count = n(), pct = round(count/nrow(menu1)*100,2))
  
  cats2<-tbl_df(menu2) %>%
    group_by(category) %>%
    summarize(count = n(), pct = round(count/nrow(menu2)*100,2))
  
  # merge together
  cat.compare<-merge(cats1,cats2,by.x = "category", by.y="category", all.x=T,all.y=T)
  # add delta
  cat.compare<-mutate(cat.compare, delta=round(pct.x-pct.y,2))
  
  
  # Brand Composition
  brands1<-tbl_df(menu1) %>%
    group_by(brand) %>%
    summarize(count = n(), pct=round(count/nrow(menu1)*100,2))
  
  brands2<-tbl_df(menu2) %>%
    group_by(brand) %>%
    summarize(count = n(), pct=round(count/nrow(menu2)*100,2))
  
  # merge together
  brand.compare<-merge(brands1,brands2,by.x = "brand", by.y="brand", all.x=T,all.y=T)
  # add delta
  brand.compare<-mutate(brand.compare, delta=round(pct.x-pct.y,2))
  
  
  # strains
  strains1<-tbl_df(menu1) %>%
    filter(menu1$category=="Flower" & is.na(menu1$strain)==F) %>%
    group_by(strain) %>%
    summarize(count = n(), pct = round(count/nrow(menu1[menu1$category=="Flower" & is.na(menu1$strain)==F,])*100,2))
  
  strains2<-tbl_df(menu2) %>%
    filter(menu2$category=="Flower" & is.na(menu2$strain)==F) %>%
    group_by(strain) %>%
    summarize(count=n(), pct = round(count/nrow(menu2[menu2$category=="Flower" & is.na(menu2$strain)==F,])*100,2))
  
  # merge together
  strain.compare<-merge(strains1,strains2,by.x="strain",by.y="strain",all.x=T,all.y=T)
  # add delta
  strain.compare<-mutate(strain.compare, delta=round(pct.x-pct.y,2))
  
  
  # so there's an issue with the size appender where some flower grams are more than a hundred dollars.  I need to find out the cause, but for now, I'm just going to spot clean
  #I'm doing this before price so that it only affects that segment
  menu1<-menu1[!(menu1$category=="Flower" & menu1$price>40),]
  menu2<-menu2[!(menu2$category=="Flower" & menu2$price>40),]
  
 
  # prices
  price1<-tbl_df(menu1) %>%
    group_by(category, subcategory, size_unit) %>%
    summarize(count = n(), avg_price = round(mean(price),2))
  
  price2<-tbl_df(menu2) %>%
    group_by(category, subcategory, size_unit) %>%
    summarize(count = n(), avg_price = round(mean(price),2))
  
  # merge together
  price.compare<-merge(price1,price2,by.x=c("category","subcategory","size_unit"),by.y=c("category","subcategory","size_unit"),all.x=T,all.y=T)
  # add delta
  price.compare<-mutate(price.compare, delta=round(avg_price.x-avg_price.y,2))
  

  
  # View results and store in global objects
  assign("brand.compare",brand.compare,envir = .GlobalEnv)
  assign("cat.compare",cat.compare,envir = .GlobalEnv)
  assign("price.compare",price.compare,envir = .GlobalEnv)
  assign("strain.compare",strain.compare,envir = .GlobalEnv)
  View(brand.compare)
  View(cat.compare)
  View(price.compare)
  View(strain.compare)
}


# Beautiful, now the final piece: the function that does everything

storeAnalyzer<-function(store,dist=5){
  closeComp(store,dist)
  if(sum(grepl(store,menu.stores))>1) print("multi-match error") # we're trying to take user input (which could be leaving off parts of the name) and return the correct name; if there are multi-matches, it should fail
  store.name<-menu.stores[which(grepl(tolower(store), menu.stores))] #store the refined store name
  menu1<-menu[which(tolower(menu$dispensary)==store.name & menu$hidden=="f"),] 
  #closeComp creates store.near.menu to use for menu2
  comparer(menu1,store.near.menu)
}

#same, but based on city (this was never actually completed; looks like there might be somthing wrong with the city comparison)
storeAnalyzerCity<-function(store,city){
  if(sum(grepl(store,menu.stores))>1) print("multi-match error") # we're trying to take user input (which could be leaving off parts of the name) and return the correct name; if there are multi-matches, it should fail
  store.name<-menu.stores[which(grepl(tolower(store), menu.stores))] #store the refined store name
  menu1<-subset(menu, tolower(menu$dispensary)==store.name & menu$hidden=="f" & is.na(menu$category)==F)
    # menu[tolower(menu$dispensary)==store.name & menu$hidden=="f" & is.na(menu$category)==F,]
  cityComp(city) #assigns store.city.menu
  menu2<-store.city.menu
  comparer(menu1, menu2)
}

le.brands<-c("goodship","headlight","dutchy","censored")
landseyeMenu<-store.city.menu[store.city.menu$brand %in% le.brands,]
length(unique(landseyeMenu$dispensary))/length(unique(store.city.menu$dispensary))
# 55% carry at least one LE brand


################################################################################
# work area to use this code
################################################################################
# So I've got two dispensaries I want to check out:
# Point Loma and Apothekare
# Point Loma had 3 stores, but I manually removed the two that were invalid so I could run the analysis
# Apothekare isn't on the menu so I'll need to manually key its menu into a csv for comparison (fuuuuun)
# no no, they are both right next to san diego so just compare to san diego

# Let's start with Point Loma because we already have a menu to look at and we want to compare to 
# need to remove the 2 stores with "point loma" that aren't the ones I'm after
menu.stores <- menu.stores[-c(1529,2771)]
storeAnalyzerCity("point loma","San Diego") # run the analyzer
# looks like we have 6000+ NAs for the menu, time to see why

apothekare <- read.csv("apothekare_menu.csv", header = T)
# I already have the san diego menu in store.city.menu from the point loma comparison
comparer(apothekare, store.city.menu)

########################################################################################################################
# plot stores where MN is sold
########################################################################################################################
# first, which stores sell MN

mn <- tbl_df(menu) %>%
  filter(brand=="censored", state=="CA") %>%
  group_by(dispensary) %>%
  summarize(dispensary_id = first(dispensary_id), itemCount = n(), lat = first(lat), long = first(long)) %>%
  filter(itemCount>1)

test<-mn[mn$itemCount==1,]
testMenu<-subset(menu, menu$dispensary_id %in% test$dispensary_id & grepl("censored", menu$all_details)==TRUE)
# well, it turns out that this might not be the best way to do it.  The algorithm is pretty accurate for brand appending, but
# it gets it wrong 5% of the time and menu items like "censored rolling paper" will get flagged as censored as well as
# "censored bong" and this flags stores incorrectly as clients.  

# if we want an accurate list, then the best thing to do would be to pull real sales data and this comes from DOMO
# I've done this (and it's a super dirty list...) and I can potentially manaully map it to CA dispensaries, but wow, that data is shit
# so I got clean data from Fish and now I need to map the 78 stores in CA that we sell to to a censored_id so we can get lat/long and see the menus
# my hope is that most (if not all) of the stores have a menu










