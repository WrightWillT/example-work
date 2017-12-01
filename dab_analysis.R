
# read the most recent version of the menu
menu.wip8<-read.csv("menu_wip8.csv", header = T)

# filter to distiallates
distillates<-subset(menu.wip8, menu.wip8$subcategory=="Distillate")

distillates$thc_content[distillates$thc_content>100]<-NA #eliminate values above 100
boxplot(distillates$thc_content)
mean(na.omit(distillates$thc_content)) 
median(na.omit(distillates$thc_content)) 

#### a little bit of visualization

library(ggplot2)
library(scales)
g <- ggplot(distillates, aes(thc_content))
g+geom_histogram(fill = "darkgreen",colour = "white",binwidth = 5, aes(y= (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent)+ 
  theme_light() + 
  ggtitle("Distillate THC content in [censored] Menu Items") +
  xlab("THC Content") +
  ylab("Percent of Menu Items") +
  stat_bin(aes(y= (..count..)/sum(..count..), 
               label = round((..count..)/sum(..count..),3)*100), 
           geom = "text", size = 4, vjust = -.2, binwidth = 5) 

distillates<-distillates[is.na(distillates$thc_content)==F,] #NA's need to be removed for the next version

g <- ggplot(distillates, aes(thc_content))
g+geom_histogram(fill = "darkgreen",colour = "white",binwidth = 5, aes(y= (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent)+ 
  theme_light() + 
  ggtitle("Distillate THC content in [censored] Menu Items") +
  xlab("THC Content (%)") +
  ylab("Percent of Menu Items") +
  scale_x_continuous(breaks = seq(0,max(distillates$thc_content), 5)) +
  stat_density()

boxplot(distillates$thc_content)

# calculate percentile
percentile<-ecdf(distillates$thc_content)
percentile(92.8)
quantile(distillates$thc_content, probs = .9)

# follow-up is to determine the distribution of THC for dabbable concentrates

dabs <- subset(menu.wip8, menu.wip8$subcategory=="Distillate" |
                    menu.wip8$subcategory=="Shatter" |
                    menu.wip8$subcategory=="Rosin" | 
                    menu.wip8$subcategory=="Live Resin" |
                    menu.wip8$subcategory=="Honeycomb_Crumble" |
                    menu.wip8$subcategory=="Wax")

dabs<-dabs[is.na(dabs$thc_content)==F & dabs$thc_content<100,]

mean(dabs$thc_content)
quantile(dabs$thc_content, probs = .9)
write.csv(dabs,"[censored]_menu_dabbableConcentrates.csv",row.names=F)
library(dplyr)
region<-tbl_df(dabs) %>%
  group_by(state) %>%
  summarize(mean_thc = mean(thc_content), 
            median_thc = median(thc_content),
            percentile_75 = quantile(thc_content, probs = .75),
            percentile_85 = quantile(thc_content, probs = .85),
            percentile_90 = quantile(thc_content, probs = .90),
            percentile_95 = quantile(thc_content, probs = .95),
            n = n()
  )

region<-region[-c(1,5,7,14,18),]
write.csv(region, "dabbable_concentration_by_state.csv",row.names = F)

subcat<-tbl_df(dabs) %>%
  group_by(subcategory) %>%
  summarize(mean_thc = mean(thc_content), 
            median_thc = median(thc_content),
            percentile_75 = quantile(thc_content, probs = .75),
            percentile_85 = quantile(thc_content, probs = .85),
            percentile_90 = quantile(thc_content, probs = .90),
            percentile_95 = quantile(thc_content, probs = .95),
            n = n()
  )

write.csv(subcat, "dabbable_concentration_by_subcat.csv",row.names = F)
