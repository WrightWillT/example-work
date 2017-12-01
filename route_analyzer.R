################################################################################
# ROUTE STOP PROBABILITY
################################################################################

library(RODBC) # load connector to mySQL
library(dplyr)
library(anytime)
channel <- odbcConnect("censored", uid="censored", pwd="censored") # connect by entering credentials

# query to pull manifest stop data
query <- "SELECT * FROM `sync_manifest.manifest_stop_data`"

# append date via anydate()
p <- sqlQuery(channel, query)
routes<-tbl_df(p) %>%
  mutate(date = anydate(sessiontime))

# create table of the max stops per manifest id
manifests<-tbl_df(routes) %>%
  group_by(manifestid) %>%
  summarize(max_stops = max(stopnumber))

# append max stops to routes
routes$route_stops<-NA
for(i in 1:nrow(routes)){
  routes$route_stops[i]<-manifests$max_stops[manifests$manifestid==routes$manifestid[i]]
}

#create empty dataframe to fill with stop data
store.stops<-matrix(ncol=8,nrow=length(unique(routes$name)))
store.stops <- as.data.frame(store.stops)
# I know I'm hardcoding in the stop columns, but a better way (if I need to) would be to make this programmatic
colnames(store.stops)<-c("store","stop1","stop2","stop3","stop4","stop5","stop6","stop7")

# add store names
store.stops$store<-unique(routes$name)

# function to return the count of routes per store/stop input
stopper<-function(store,stops){
  nrow(routes[routes$name==store & routes$route_stops==stops,])
}

# plug stop data into the store.stops via stopper()
for(i in 1:nrow(store.stops)){
  store.stops$stop1[i]<-stopper(store.stops$store[i],1)
  store.stops$stop2[i]<-stopper(store.stops$store[i],2)
  store.stops$stop3[i]<-stopper(store.stops$store[i],3)
  store.stops$stop4[i]<-stopper(store.stops$store[i],4)
  store.stops$stop5[i]<-stopper(store.stops$store[i],5)
  store.stops$stop6[i]<-stopper(store.stops$store[i],6)
  store.stops$stop7[i]<-stopper(store.stops$store[i],7)
}

# convert stop frequency to stop probability
store.stops.prob<-store.stops

## concert to pct
percenter <- function(stops,stopvals){
  pct <- round(stops/sum(stopvals),2)
  pct
}

#apply to table
for(i in 1:nrow(store.stops.prob)){
  store.stops.prob$stop1[i]<-percenter(store.stops$stop1[i],store.stops[i,2:8])
  store.stops.prob$stop2[i]<-percenter(store.stops$stop2[i],store.stops[i,2:8])
  store.stops.prob$stop3[i]<-percenter(store.stops$stop3[i],store.stops[i,2:8])
  store.stops.prob$stop4[i]<-percenter(store.stops$stop4[i],store.stops[i,2:8])
  store.stops.prob$stop5[i]<-percenter(store.stops$stop5[i],store.stops[i,2:8])
  store.stops.prob$stop6[i]<-percenter(store.stops$stop6[i],store.stops[i,2:8])
  store.stops.prob$stop7[i]<-percenter(store.stops$stop7[i],store.stops[i,2:8])
}

################################################################################
# DOOR DENSITY
################################################################################

# I'm going to take code from delivery_cost_analysis_v01.R

loc<-read.csv("client_lat_long_v2.csv", header = T, stringsAsFactors = F)
r<-(15/111) #set radius equal to 15km

# make sure that door is input as loc[,i] so we can run a for loop through it
# output of this should be TRUE or FALSE
circle.tester<-function(door1, door2){ 
  door1.x<-loc[door1,3]
  door1.y<-loc[door1,2]
  door2.x<-loc[door2,3]
  door2.y<-loc[door2,2]
  if(((door1.x-door2.x)^2+(door1.y-door2.y)^2) < r^2) print(TRUE)
  else print(FALSE)
}

# now write a function that takes the sum of the outputs of circle.tester applied to all loc doors for a single door (next will be to do this for all doors)
# door is a number for the door in loc
door.density<-function(door){
  results<-NA
  for(i in 1:nrow(loc)){ #we'll test all locations, including the door itself
    invisible(capture.output(result<-(circle.tester(i,door)))) #the door we're testing is door 2 in circle.tester since it's the middle; invisible to avoid the long output
    results<-c(result,results)
  }
  results<-na.omit(results) #to remove the first NA introduced with results
  sum(results)
}

# next is to create a column in loc for density that uses the door.density function
loc$door_density<-NA
for(i in 1:nrow(loc)) loc$door_density[i] = door.density(i)

# if you need distances from censored, use the original code, which has a section
# for appending Euclidean distance in lat/long, km, and mi

################################################################################
# ANALYZE DENSITY AND STOP PROBABILITY
################################################################################

# First, append the loc$door_density to store.stops.prob
# I'm noticing that there are 47 stores in the loc (which is based on current customers)
# but we've made stops at 131 stores in the store.stops table
# I guess we'll append where there is a match and try to understand why we're
# making stops at places that aren't customers.  My hunch is the ones that
# aren't customers have had like 1 or 2 stops of samples only

#problem where I can't get them to match exactly since names are different.  I can
# either go upstream and fix or manually pair the 47, which is fine for now, but not sustainable

# https://www.r-bloggers.com/fuzzy-string-matching-a-survival-skill-to-tackle-unstructured-information/
# this has info on fuzzy matching algorithms

store.stops.prob$density<-NA
# I'm doing this next bit in Excel because it involves matching in a way that is easier just to do manually
# Ideally, I'd have some id to match to... is that possible?

write.csv(store.stops.prob,"store_stops_prob.csv",row.names = F)
write.csv(loc,"door_density.csv",row.names = F)
write.csv(store.stops, "store_stops.csv", row.names = F)


################################################################################
# APRIL AND MAY: ANALYZE DENSITY AND STOP PROBABILITY
################################################################################

##### Grab a subset of only April and May through the 25th (most recent data)
# Goal is to look at just 2 months of data since the most recent months are likely more predictive

recent.routes <- filter(routes, routes$date > as.Date("2017-04-01"))

# create table of the max stops per manifest id
manifests<-tbl_df(recent.routes) %>%
  group_by(manifestid) %>%
  summarize(max_stops = max(stopnumber))

# append max stops to routes
recent.routes$route_stops<-NA
for(i in 1:nrow(recent.routes)){
  recent.routes$route_stops[i]<-manifests$max_stops[manifests$manifestid==recent.routes$manifestid[i]]
}

#create empty dataframe to fill with stop data
store.stops<-matrix(ncol=8,nrow=length(unique(recent.routes$name)))
store.stops <- as.data.frame(store.stops)
# I know I'm hardcoding in the stop columns, but a better way (if I need to) would be to make this programmatic
colnames(store.stops)<-c("store","stop1","stop2","stop3","stop4","stop5","stop6","stop7")

# add store names
store.stops$store<-unique(recent.routes$name)

# function to return the count of routes per store/stop input
stopper<-function(store,stops){
  nrow(recent.routes[recent.routes$name==store & recent.routes$route_stops==stops,])
}

# plug stop data into the store.stops via stopper()
for(i in 1:nrow(store.stops)){
  store.stops$stop1[i]<-stopper(store.stops$store[i],1)
  store.stops$stop2[i]<-stopper(store.stops$store[i],2)
  store.stops$stop3[i]<-stopper(store.stops$store[i],3)
  store.stops$stop4[i]<-stopper(store.stops$store[i],4)
  store.stops$stop5[i]<-stopper(store.stops$store[i],5)
  store.stops$stop6[i]<-stopper(store.stops$store[i],6)
  store.stops$stop7[i]<-stopper(store.stops$store[i],7)
}

# convert stop frequency to stop probability
store.stops.prob<-store.stops

## concert to pct
percenter <- function(stops,stopvals){
  pct <- round(stops/sum(stopvals),2)
  pct
}

#apply to table
for(i in 1:nrow(store.stops.prob)){
  store.stops.prob$stop1[i]<-percenter(store.stops$stop1[i],store.stops[i,2:8])
  store.stops.prob$stop2[i]<-percenter(store.stops$stop2[i],store.stops[i,2:8])
  store.stops.prob$stop3[i]<-percenter(store.stops$stop3[i],store.stops[i,2:8])
  store.stops.prob$stop4[i]<-percenter(store.stops$stop4[i],store.stops[i,2:8])
  store.stops.prob$stop5[i]<-percenter(store.stops$stop5[i],store.stops[i,2:8])
  store.stops.prob$stop6[i]<-percenter(store.stops$stop6[i],store.stops[i,2:8])
  store.stops.prob$stop7[i]<-percenter(store.stops$stop7[i],store.stops[i,2:8])
}



################################################################################
# Calculating Euclidean Distance
################################################################################
# calculate distance from LE
censored<-c(47.680403, -122.541375)

loc$euc_dist<-NA
# good ole pythagorean theorem:
distBetweenPoints<-function(x1,y1,x2,y2){
  a<-x2-x1
  b<-y2-y1
  c<-sqrt(a^2+b^2)
  print(c)
}

# using euclidean lat/long distance
for(i in 1:nrow(loc)){
  loc$euc_dist[i]<-distBetweenPoints(loc$latitude[i], loc$longitude[i], censored[[1]], censored[[2]])
}

#convert to miles
loc$euc_dist_km<-NA
for(i in 1:nrow(loc)){
  loc$euc_dist_km[i]<-round(loc$euc_dist[i]*111,5) #111km in a degree of lat/long
}

loc$euc_dist_mi<-NA
for(i in 1:nrow(loc)){
  loc$euc_dist_mi[i]<-round(loc$euc_dist_km[i]*0.621371,5) # km to mi ratio
}

################################################################################
# Visualizing
################################################################################

loc<-read.csv("client_lat_long_v3.csv", header = T, stringsAsFactors = F)
loc<-tbl_df(loc) %>%
  filter(loc$Apr_partialMay_revenue != 0)

library(leaflet)

leaflet(loc) %>% addTiles() %>%
  addLabelOnlyMarkers(~latitude,~longitude,label = ~total_cost_per,
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)
  )








