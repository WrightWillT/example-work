########################################################################################################################
# VERSION NOTES
########################################################################################################################

# Notes started with V4
# V4: this version uses a stores list that isn't a subset and is more up-to-date
# V5: so I discovered that my original SQL query was leaving off the very important 'is_deleted' field
#   I'm taking this opportunity to fix that and streamline the process of getting the recent menu by only grabbing the non-deleted items.
#   This should make it so the cleaning only has to happen on a small fraction of the 3.2MM total items
#   One of the goals for this iteration is to be able to run the entire script without having to do it piecemeal so make sure
#   to clean up the errant lines of code.
#   NOTE: I pulled all the code out for brand generation and testing; use v4 if you want that again
#   NOTE: I also removed the section for exploratory data analysis that I wasn't using
#   Note: I also pulled the area in the THC append for visualizing the distribution of THC for distillates
# V6: I fixed an issue where I accidentally removed columns I actually need between menu.wip7 and menu.wip8
# V7: Added a 'Methodology' section and started work on streamlining the whole process
#   Updated the password/username to reflect the most recent change
#   Added the 'last_modified' field to be picked up by the SQL query
#   Completely reworked the SQL query to bypass the need to use the stores .csv
#   Implemented a new brand lookup which is 100% from BDS and Headset and also uses an updated methodology for more accuracy
#   Cleaned the code for the size/unit assignment
#   Rewrote the size_unit code in modern terms (no improvements) as well as the subcat appender (major improvements)
# V8: New version since I updated the SQL pull to also join on the strain table and get cultivar; removed section for cultivar
#   Modernized the THC assigner
#   Rewrote the brand assigner to reduce assignment time by 42% with ifelses and sapply instead of loops
# V9: Updated brand assigner to make use of parallel processing.  Relative to V7, this takes 20% of the time
#     Applied parallelization to each append process and updated some of the loops with ifelses
#     Added system.time to each segment so I can see how long each phase of the process takes
#     After testing to ensure I could pull the entire [censored] database without fail, removed old script that chunked it
#     Updated the size/unit appender to use V4 of the lookup
# V10: Rewrote the non-brand functions to remove all for loops

########################################################################################################################
# METHODOLOGY
########################################################################################################################

# Partial Menu Updating
# 1. Read existing menu and extract the date of the latest updated menu item
# 2. Use a SQL query to the [censored] database to pick up all new menu items
# 3. Append dispensary
# 4. Append brand
# 5. Append Size/Unit
# 6. Append Subcategory (needs complete mapping and further cleaning)
# 7. Append Cultivar
# 8. Append THC %
# 9. Append Headset brand
# 10. Cleanup

# Complete Menu Updating
# Like the partial menu cleaning, but refreshes the entire [censored] database.  This is useful for situations where
# improvements to the methodology need to be applied to the full dataset

# TODO:
# optimize functions with ifelse and sapply
# Append CBD %
# Append mg of THC and CBD
# Append flavor
# Appent outlier flag
# Append form factor (maybe)
# Could write this whole thing as two functions; one for partial and one for complete updates?
# Ml (for liquids)
# Pack size
# oz
# CBD:THC ratio
# vegan and sugar-free flags

########################################################################################################################
# READ PACKAGES + SET UP PARALLEL PROCESSING
########################################################################################################################

library(dplyr)
library(RPostgreSQL)
library(foreach) # used in parallel processing
library(doSNOW) # used in parallel processing



########################################################################################################################
# READ LATEST MENU + GET TIME OF LATEST MENU UPDATE
########################################################################################################################

# read menu
# menu <- read.csv("menu_wip8_2017-08-30.csv") # update when you've updated the menu writing at the end and use current menu for now

# lastUpdate <- max(menu$last_modified) # does not exist yet


########################################################################################################################
# CONNECT TO [censored] DATABASE + ACQUIRE DATA
########################################################################################################################
timeSql <- system.time({
  # CONNECT TO THE VPN FIRST
  
  # create a connection to the [censored] database
  pw<-{
    "censored"
  }
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  con <- dbConnect(drv, dbname = "censored",
                   host = "censored", port = 5432,
                   user = "censored", password = pw)
  rm(pw) # removes the password
  
  # turn off scientific notation
  options(scipen = 999)
  
  # SQL query to [censored] data base which joins the menu_item, dispensary, and strain tables
  # The cross lateral join is to expand the JSON column with price and size data
  
  lastUpdate <- "2017-10-18"
  menuRaw <- dbGetQuery(con,paste0(
    "SELECT 
      m.id, 
      m.name, 
      m.description AS details, 
      m.category, 
      m.created, 
      m.strain_id, 
      m.strain_key, 
      s.category AS cultivar,
      x.\"name\" AS description, 
      x.\"stockQuantity\" AS quantity, 
      x.\"packagePrice\" AS price, 
      x.\"packageSize\" AS size, 
      x.\"packageUnit\" AS unit, 
      x.\"hidden\" AS hidden, 
      m.last_modified, 
      m.dispensary_id, 
      d.name AS dispensary_name,
      d.address1, 
      d.address2, 
      d.city, 
      d.state, 
      d.country, 
      d.latitude, 
      d.longitude
    FROM menu_item m
    CROSS JOIN LATERAL 
      jsonb_to_recordset(m.variants) as
        x(\"name\" text, 
          \"stockQuantity\" numeric, 
          \"packagePrice\" numeric,
          \"packageSize\" numeric, 
          \"packageUnit\" text, 
          \"hidden\" text,
          \"externalKey\" text, 
          \"partnerKey\" text, 
          \"globalKey\" text)
    LEFT JOIN dispensary d
    ON d.id = m.dispensary_id
    LEFT JOIN strain s
    ON s.id = m.strain_id
    WHERE 
      m.is_deleted = FALSE 
      AND m.last_modified > '",lastUpdate,"'"))
})


########################################################################################################################
# APPENDING BRAND
########################################################################################################################

timeBrand <- system.time({
  menuBranded <- menuRaw
  
  # V7 of this script uses the v2 of brand_lookup
  brandLookup<-read.csv("brand_lookup_v2.csv")
  
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
  cl <- makeCluster(8, type="SOCK") # for 8 core machine
  registerDoSNOW (cl)
  menuBranded$brand <- NA
  menuBranded$brand <- foreach(i = 1:nrow(menuBranded), .combine=c) %dopar% {
    brandAssigner(tolower(paste(menuBranded$name[i], menuBranded$details[i], sep = " ")))
  }
})

stopCluster(cl)
# 99.3% true positive rate with brand_lookup_v2
# 13.8hrs for 1.49MM
# 97.8% of the time is spent here

########################################################################################################################
# APPENDING SIZE/UNIT
########################################################################################################################

timeSize <- system.time({
  menuSized <- menuBranded
  
  # V4 of the lookup removes "1_One" (which is 19% of cases) since it's ambiguous
  sizeLookup<-read.csv("[censored]_menu_size_unit_lookup_v4.csv", stringsAsFactors = F)
  
  # Function to append size/unit from the lookup.  Returns NA if there is no match
  sizeUnitAssigner <- function(textInput) {
    ifelse(textInput %in% sizeLookup$sizeUnit_raw, 
           sizeLookup$sizeUnit_clean[which(sizeLookup$sizeUnit_raw==textInput)],
           NA)
  }
  
  # Loop through the menu, pasting together both size and unit to be found in the lookup as [size]_[unit]
  menuSized$sizeUnit <- sapply(paste(menuSized[,12],menuSized[,13], sep = "_"), FUN = sizeUnitAssigner)

  
  # The case of "1_One" for flower only is always 1.0oz.  I may need to consider rewriting the lookup to include
  #   other conditions (e.g. category==x or grepl("1g,x)==TRUE)
  # For now, because this exception is 10% of cases, I'll handle it individually
  
  menuSized$sizeUnit[which(paste(menuSized$size,menuSized$unit, sep = "_")=="1_One" & menuSized$category=="Flower")] <- "1.0oz"
  
})

  
# TODO: still need to improve accuracy of true positives 

########################################################################################################################
# APPENDING SUBCATEGORY
########################################################################################################################

timeSubcat <- system.time({
  menuSubcatted <- menuSized
  
  subcatLookup <- read.csv("subcat_lookup_v2.csv", stringsAsFactors = F)
  
  # Unlike the brand assigner where we use the greatest confidence followed by being the highest in the list to determine
  # which brand gets assigned, subcat only uses the order of the elements in the table to make this determination.
  # This being the case, "chocolate brittle" returns "chocolate" subcat since chocolate is matched after brittle.
  # If need be, move the cases I want to give more priority to to the bottom of the list

  
  # Create a function to assign subcategory
  subcatAssigner <- function(category, textInput = NA){
    ifelse(category %in% subcatLookup$cat,
           output <- subcatLookup$subcat[which(subcatLookup$cat==category)][
              which(
                sapply(
                  subcatLookup$subcatKey[
                    which(subcatLookup$cat==category)], 
                  grepl, 
                  tolower(textInput), perl = T))[1]],
      output <- NA)
      as.character(output)
  }
  
  category <- menuSubcatted[,4]
  textInput <- paste(menuSubcatted[,2], menuSubcatted[,3], sep = " ")

  menuSubcatted$subcategory <- mapply(subcatAssigner, category, textInput)
  
  
})



# about 5 seconds per 10K row assignment
# 99.1% accuracy with subcat_lookup_v2

########################################################################################################################
# APPENDING THC
########################################################################################################################

timeThc <- system.time({
  menuThced <- menuSubcatted
  
  thcAssigner <- function(textInput){
    output <- gsub(".*THC:? ?([0-9][0-9]\\.?[0-9]?[0-9]?).*","\\1",textInput) # accounts for "THC:80.5%" situations
    output <- gsub("^.*([0-9][0-9]\\.[0-9]?[0-9]?)%? THC.*", "\\1", output) # accounts for "80.5% THC" situations
    # if there is no gsub, then the input and output should be identical. We want an NA output in those cases
    ifelse(identical(textInput,output), 
           output <- NA,
           output <- as.numeric(output))
    output
  }
  
  # loop through menu and append THC
  menuThced$thc_content <- sapply(paste(menuThced[,2],menuThced[,3], sep = " "), FUN = thcAssigner)
  
})

# 75s for 1.5MM
# TODO: improve accuracy of the regex and simplify the regex to one line

########################################################################################################################
# REORDER COLUMNS AND FINALIZE MENU 
########################################################################################################################

# date<-Sys.Date()
# write.csv(menu.wip8,paste0("[censored]MenuClean_",date,".csv"),row.names=F)

# 34.91 seconds per 1000 cleansing
# for 1.2MM items, this is 11.6hrs; 13% of the estimated 90hrs for the previous version

########################################################################################################################
# DEBRIEF - FUNCTION TIMES
########################################################################################################################

timeSql     # 37.66s per 99,650 = 0.38s per 1000
timeBrand   # 32.89 per 1000
timeSize    # 0.58 per 1000
timeSubcat  # 0.62 per 1000
timeThc     # 0.58 per 1000



