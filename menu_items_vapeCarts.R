################################################################################
# READ DATA
################################################################################
cart.raw<-read.csv("leafly_menu_items_vapeCarts.csv", header = T)

################################################################################
# Data Cleansing
################################################################################
# grab only the rows for which 'THC' is present
cart.1<-cart.raw[grep("THC",cart.raw$description),]
# apply regex to get to the numbers
cart.2<-as.data.frame(as.character(gsub("^.*THC:? ?([0-9][0-9]\\.?[0-9]?[0-9]?).*","\\1",cart.1$description)))
colnames(cart.2) <- "THC_content"
cart.3<-as.data.frame(as.character(gsub("^.*([0-9][0-9]\\.[0-9]?[0-9]?)%? THC.*", "\\1", cart.2$THC_content)))
colnames(cart.3) <- "THC_content"

#get rid of rows where text remains
cart.4<-as.data.frame(as.character((cart.3[-grep("[a-z|A-Z]",cart.3$THC_content),])))
unused<-as.data.frame(as.character(cart.3[grep("[a-z|A-Z]",cart.3$THC_content),]))
colnames(cart.4)<-"thc_content"

# dim(cart.4)[1]/dim(cart.raw)[1]
# 67.4% of the rows were usable after cleaning; more cleaning could improve

# convert to numeric
cart.5<-as.data.frame(as.numeric(as.character(cart.4$thc_content)))
colnames(cart.5)<-"thc_content"
# remove THC>=98
cart.6<-subset(cart.5,thc_content<98)

# max(boxplot(cart.6$thc_content)$out)
# 21.6 and below are outliers
cart.7<-subset(cart.6,thc_content>21.6)

################################################################################
# Stats
################################################################################
mean(cart.7$thc_content) 
quantile(cart.7$thc_content, probs = 0.90) 
quantile(cart.7$thc_content, probs = 0.95) 
mean(cart.7$thc_content <= 90) 


################################################################################
# Visualization
################################################################################

boxplot(cart.7$thc_content)
hist(cart.7$thc_content, xlab = "THC Content", main = "Vape Cartridge THC Potency Histogram")

library(scales)
ggplot(cart.7, aes(thc_content)) +
  geom_histogram(fill = "darkgreen",colour = "white",binwidth = 5, aes(y= (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent)+ 
  theme_light() + 
  ggtitle("Vape Cartridge THC Potency Histogram") +
  xlab("THC Content") +
  ylab("Percent of Vape Cartridges") +
  stat_bin(aes(y= (..count..)/sum(..count..), 
               label = paste0(round((..count..)/sum(..count..),3)*100,"%")), 
           geom = "text", size = 4, vjust = -.2, binwidth = 5) +
  geom_vline(aes(xintercept=quantile(cart.7$thc_content, probs = 0.90)),color = "#BB0000", linetype = "dashed")
