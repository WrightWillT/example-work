########################################################################################################################
# HASH PRICING ANALYSIS - PURPOSE
########################################################################################################################
# Purpose: determine what the distribution of prices looks like for hash

########################################################################################################################
# READ DATA AND LOAD PACKAGES
########################################################################################################################

# start by reading the menu if you don't have it in memory
hash <- menu[which(menu$subcategory=="Hash" & menu$state=="CA"),] #5003 products

hash <- mutate(hash, size_unit_coarse = paste0(size, unit))
View(table(hash$size_unit_coarse))

hash <- hash[which(hash$size_unit_coarse=="1Gram" | hash$size_unit_coarse=="1HalfGram" | hash$size_unit_coarse=="1Half Gram"),]

hashGram <- hash[which(hash$size_unit_coarse=="1Gram"),]

boxplot(hashGram$price)

# remove outliers
hashGram <- hashGram[which(!hashGram$price %in% boxplot.stats(hashGram$price)$out),]
boxplot(hashGram$price)
boxplot.stats(hashGram$price)

mean(hashGram$price)

hist(hashGram$price)
abline(v=mean(hashGram$price), col = "red")
abline(v=median(hashGram$price))
text(x = mean(hashGram$price), y = -20, labels = "mean = $23.04")

percentile <- ecdf(hashGram$price)

quantiles <- data.frame(matrix(nrow = 10, ncol = 2))
colnames(quantiles) <- c("quantile", "price")
for(i in 1:nrow(quantiles)){
  quantiles$quantile[i] <- i/10
  quantiles$price[i] <- quantile(hashGram$price, probs = i/10)
}

nativ <- hashGram[which(grepl("nativ", hashGram$brand)),]
