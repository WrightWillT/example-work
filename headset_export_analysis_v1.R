################################################################################
# Read data
################################################################################
raw<-read.csv("headset_export_salesData.csv", header = T)

################################################################################
# Clean data
################################################################################
# filtering
raw<-filter(raw,raw$Gender!="")  #scrubbing 22.3% of data which had no gender; 11,059 rows down to 8595
# raw<-filter(raw,raw$Sales<quantile(raw$Sales,probs=0.98)) #filter out sales in the top 2% (decided against since this isn't per person)
raw<-filter(raw,raw$Customer.Age<110) #filter out people over 110 years old

# bucketing
# need age groups to be the same as the survey: 21-29, 30-44, 45-59, 60+
for(i in 1:dim(raw)[1]) {
  if(raw$Customer.Age[i]<30) raw$age.bucketed[i]<-"21-29"
  if(raw$Customer.Age[i]>30) raw$age.bucketed[i]<-"30-44"
  if(raw$Customer.Age[i]>44) raw$age.bucketed[i]<-"45-59"
  if(raw$Customer.Age[i]>59) raw$age.bucketed[i]<-"60+"
}
# group by age bucketed
clean<-raw %>%
  group_by(Gender, age.bucketed, Category, Package.Size1) %>%
  summarize(quantity = sum(Quantity), sales = sum(Sales))

################################################################################
# Explore data
################################################################################
# lets first look at total sales by each age/gender group
summary.sales<-clean %>%
  group_by(age.bucketed, Gender) %>%
  summarize(totalSales = sum(sales)) %>%
  mutate(pct = totalSales/sum(summary.sales$totalSales))

#plot
library(ggplot2)
ggplot(summary.sales, aes(x=age.bucketed, y=totalSales, fill=Gender))+
  geom_bar(stat="identity",position = "dodge") +
  xlab("Age Group") +
  ylab("Total Sales") +
  ggtitle("2016 Cannabis Sales by Age/Gender Groups")

ggplot(summary.sales, aes(x=age.bucketed, y=pct, fill=Gender))+
  geom_bar(stat="identity",position = "dodge") +
  xlab("Age Group") +
  ylab("Percent of Total Sales") +
  ggtitle("2016 Cannabis Sales by Age/Gender Groups") +
  scale_y_continuous(labels = percent) +
  geom_text(aes(label = paste0(round(pct,3)*100,"%"), vjust = -0.5))




