# load packages
library(RGoogleAnalytics)
library(gplots)
library(httpuv)

# Assign client id, secret, and view id
client.id <- "censored"
client.secret <- "censored"
view.id <- "censored"

# Create Token and Authorize
# Create new token
token <- Auth(client.id, client.secret)
# Save token in Environment for use later, save .RData in accessible folder
# Validate
ValidateToken(token)

## Establish date range
start <- "2016-01-01"
end <- "2016-12-31"
## Build Query. table.id = view.id for the account you are querying against (master, app)
query.list <- Init(start.date = start,
                   end.date = end,
                   dimensions = "ga:date,ga:eventCategory, ga:eventLabel, ga:eventAction",
                   metrics = "ga:totalEvents",
                   max.results = 9999999,
                   sort = "-ga:date",
                   filters = URLencode("ga:eventCategory==Location Details,ga:eventCategory==VNext Finder,ga:eventCategory==Home"),
                   table.id = view.id)
ga.query <- QueryBuilder(query.list)
ga.data <- GetReportData(ga.query, token, paginate_query = TRUE)

# Query to pull pageviews by state in the US with Destination Page as either hybrid, sativa, or indica, but not availability, photos, or reviews
## Establish date range
start <- "2016-01-01"
end <- "2017-02-28"
## Build Query. table.id = view.id for the account you are querying against (master, app)
query.list <- Init(start.date = start,
                   end.date = end,
                   dimensions="ga:region,ga:yearMonth,ga:pagePath",
                   metrics = "ga:pageviews",
                   max.results = 10000,
                   sort = "ga:region,-ga:yearMonth",
                   # in filters, to use regular expressions, use the regex operator "~"
                   # in filters, use ";" to separate multiple expressions
                   # in filters, use "!~" to exclude (not the intuitive !=~)
                   # seems like if you try to put this into multiple lines, it breaks
                   filters = URLencode(
                      "ga:country=~United States;ga:region!~(not set);ga:pagePath=~^/hybrid|^/sativa|^/indica;ga:pagePath!~availability|photos|reviews"),
                   table.id = view.id)
ga.query <- QueryBuilder(query.list)
ga.data <- GetReportData(ga.query, token, paginate_query = TRUE)

# only WA for strain analysis
query.list <- Init(start.date = start,
                   end.date = end,
                   dimensions="ga:yearMonth,ga:pagePath",
                   metrics = "ga:pageviews,ga:users",
                   max.results = 999999,
                   sort = "-ga:yearMonth",
                   # in filters, to use regular expressions, use the regex operator "~"
                   # in filters, use ";" to separate multiple expressions
                   # in filters, use "!~" to exclude (not the intuitive !=~)
                   # seems like if you try to put this into multiple lines, it breaks
                   filters = URLencode(
                     "ga:country=~United States;ga:region!~(not set);ga:region=~Washington;ga:pagePath=~^/hybrid|^/sativa|^/indica;ga:pagePath!~availability|photos|reviews"),
                   table.id = view.id)
ga.query <- QueryBuilder(query.list)
ga.data <- GetReportData(ga.query, token, paginate_query = FALSE)

# mutate the yearMonth column to be in a date format
library(dplyr)
library(zoo)
ga.data<-tbl_df(ga.data)
# for reference, the following is what I'm after:
# as.Date(as.yearmon('201601', format = '%Y%m'))
ga.data<-mutate(ga.data, year_month = as.Date(as.yearmon(ga.data$yearMonth, format = '%Y%m')))
# not sure that was necessary since I can group by month with the original yearmon...
topStrains_byState <- ga.data %>%
  select(region,pagePath,pageviews) %>%
  group_by(region, pagePath) %>%
  summarize(annualPV = sum(pageviews)) %>%
  arrange(region,desc(annualPV)) %>%
  #slice(1:20)
  slice(1)

write.csv(topStrains_byState, "C:/users/will.wright/Documents/topStrains_byState.csv")

# As I disovered, I could have just stuck with the new API pull with pagination set to FALSE and not worry about this, oh well
# mutate the yearMonth column to be in a date format
library(dplyr)
library(zoo)
ga.data<-tbl_df(ga.data)
# for reference, the following is what I'm after:
# as.Date(as.yearmon('201601', format = '%Y%m'))
ga.data<-mutate(ga.data, year_month = as.Date(as.yearmon(ga.data$yearMonth, format = '%Y%m')))
# not sure that was necessary since I can group by month with the original yearmon...
WA.strains <- ga.data %>%
  filter(region=="Washington")

write.csv(WA.strains, "C:/users/will.wright/Documents/WA_strainData.csv")



