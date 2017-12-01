########################################################################################################################
# PURPOSE AND VERSION NOTES
########################################################################################################################

# PURPOSE: In order to ensure the most accurate brand assignment, the purpose of this script is to generate ngrams
#   and calculate word frequencies so I can generate a *current* list of brands to assign to the menu items

# V1: Starting from the original code, but now adding experience insights for improvements

########################################################################################################################
# LOAD LIBRARIES
########################################################################################################################

library(tm)
library(RWeka)
library(dplyr)

########################################################################################################################
# READ CURRENT MENU AND STOPWORDS
########################################################################################################################

# I don't have a current menu and I'm updatnig the Leafly menu cleaning script so on the interim, I stuck together the 
# latest data with the old menu data (which has all historical data before 8/30/17).  My goal is to do the text mining
# on the resulting dataset where 2 columns with product details should have all the brand names

temp1 <- menuRaw[,c(2,3)]
colnames(temp1) <- c("product_name","product_details")
temp2 <- menu[,c(2,3)]
menuDetails <- rbind(temp1,temp2)

# combine the name + details fields together so I can perform text mining on a single column
menuDetails <- menuDetails %>%
  mutate(all_details = paste(product_name, product_details, sep = " "))

menuDetails <- menuDetails[,3] #grab the combined column

stopwords <- read.csv("stopwords.csv")

########################################################################################################################
# UPDATED TEXT MINING FOR BRANDS
########################################################################################################################

library(ngram)
library(tm)

# concatenate all strings in the vector into a single string; I later find that processing 2.2M items is hard
# menuCollapsed <- concatenate(menuDetails, collapse = " ")
# string.summary(menuCollapsed)

# only 100K sample for testing
set.seed(1234)
menuSampled <- sample(menuDetails,100000)
# collapse the vector into a single string
menuCollapsed <- concatenate(menuSampled, collapse = " ") 

text_corpus <- Corpus(VectorSource(menuCollapsed))
text_corpus <- tm_map(text_corpus, PlainTextDocument)
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removeWords, c(stopwords('english'),"this","that"))
text_corpus <- tm_map(text_corpus, stripWhitespace)

menuCollapsedClean <- as.character(text_corpus$content)

unigrams <- ngram(menuCollapsedClean, n=1)
bigrams <- ngram(menuCollapsedClean, n=2)
trigrams <- ngram(menuCollapsedClean, n=3)
quadgrams <- ngram(menuCollapsedClean, n=4)

unigramFreq <- get.phrasetable(unigrams)
bigramFreq <- get.phrasetable(bigrams)
trigramFreq <- get.phrasetable(trigrams)
quadgramFreq <- get.phrasetable(quadgrams)

ngrams <- rbind(unigramFreq, bigramFreq, trigramFreq, quadgramFreq)
ngrams <- ngrams[order(desc(ngrams$freq)),]
rownames(ngrams) <- c()

# limit to terms that appear at least 10 times
topNgrams <- ngrams[which(ngrams$freq>=10),]

write.csv(topNgrams, "top_ngrams_brandBuilder.csv", row.names = F)

