## The purpose of this script is to read in do text mining analysis on strain reviews
## V02 was created to filter the reviews down to just visible reviews (thus eliminating spam)
##    and also reduce the review threshold to 20 since I lost so many

## Load libraries
library(dplyr)

## Read Data
df <- read.csv("Strain_Reviews_censored_DB_3_3_17.csv", header = TRUE, sep = ",")

## convert to tbl_df
strain.data <- tbl_df(df)

## Select relevant columns and filter for complete cases
strain.data <- select(strain.data, Visible, StrainId, Notes, IP, Form, Method)


################################################################################
# Using the Strength field to find the strongest rated strains
################################################################################

strain.data2 <- tbl_df(df) %>%
  select(Visible, StrainId, Strength) %>%
  filter(Visible==TRUE, Strength >0) %>%
  group_by(StrainId) %>%
  summarize(avg_strength = mean(Strength), n = n()) %>%
  select(StrainID = StrainId, avg_strength, n) %>%
  filter(n>20) %>%
  merge(strain_lookup_tbl) %>%
  arrange(desc(avg_strength))

write.csv(strain.data2, "strongest_strains.csv")

#trying again to see why oI only have 238 strains with the above methodology
# I believe its the case that if any strain has an NA for strength, its excluded
#strain.data3 <- tbl_df(df) %>%
  #select(Visible, StrainId, Strength) %>%
  #311,799 rows
  #filter(Visible ==TRUE)%>%
  # 234,992 visible
  # sum(is.na(strain.data3$Strength))
  # 203,082 NAs in Strength
  # sum(complete.cases(strain.data3$Strength))
  # 31,910 complete cases
  #group_by(StrainId) %>%
  #filter(Strength>-1) %>%
  #summarize(avg_strength = mean(Strength), n = n()) %>%
  #select(StrainID = StrainId, avg_strength, n) %>%
  #filter(n>20) %>%
  #merge(strain_lookup_tbl) %>%
  #arrange(desc(avg_strength))

## MORAL OF THE STORY: only 13.5% of reviews have a strength rating

 
################################################################################
# Create an IP lookup
################################################################################
#  Next is to create a dataframe lookup of IPs and states to be applied to strain.data
distinct_ip <- strain.data %>%
  distinct(IP) %>%
  select(IP) %>%
  filter(IP!="")

distinct_ip$IP <- as.character(distinct_ip$IP)
# call freegeoip to get location data based on IP 
ip_data1 <- freegeoip(distinct_ip$IP[1:5000])
ip_data2 <- freegeoip(distinct_ip$IP[5001:10000])
ip_data3 <- freegeoip(distinct_ip$IP[10001:length(distinct_ip$IP)])

ip_data <- rbind(ip_data1,ip_data2,ip_data3)

# Write to disk so I can just pull the ip_data later instead of re-querying the freegeoip function
write.csv(ip_data, "ip_data.csv")

# clean up missing data and create a data frame with just ip and state ("region_name")
ip_data_tbl <- tbl_df(ip_data) %>%
  select(ip, region_name) %>%
  filter(region_name != "")

wa_ip_data <- filter(ip_data_tbl, region_name=="Washington")
colnames(wa_ip_data)[1] <- "IP"

# Inner join to create a dataframe with just WA reviews
wa_strain_data <- merge(strain.data, wa_ip_data)

# after exploring and discovering ~2k strain reviews that are just WA with the average
# strain review at ~5 reviews per strain, I decided to just use the entire dataset

# filter strain reviews to only reviews where the Notes and IP aren't blank
# in V02 I removed the filter for IP != "" since that seemed to filter down to basically nothing


################################################################################
# CLEAN DATA
################################################################################
strain.data$StrainId <- as.factor(strain.data$StrainId)
# percent with Notes (74.9%)
# sum(strain.data$Notes !="")/length(strain.data$Notes)
clean.strain.data <- filter(strain.data, Notes != "", Visible==TRUE)
# percent both visible and with notes (57.4%)
# length(clean.strain.data$Notes)/length(strain.data$Notes)

top.strain.counts <- clean.strain.data %>%
  group_by(StrainId) %>%
  summarize(strainCount = n())

# only want to examine strains with >20 reviews
top.strain.data <- merge(clean.strain.data,top.strain.counts) %>%
  filter(strainCount > 20) %>%
  select(StrainId,Notes)
top.strain.data.agg <- aggregate (Notes ~ StrainId, data = top.strain.data, paste, collapse = ";") 
# lenth(top.strain.data$Notes) #962 strains have notes, are visible, and have 21+ reviews
  
################################################################################
# TEXT MINING
################################################################################

library(tm)
library(SnowballC)

# Create and clean corpora
text_corpus <- Corpus(VectorSource(top.strain.data.agg$Notes))
text_corpus <- tm_map(text_corpus, PlainTextDocument)
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeWords, c(stopwords('english'),"this","that"))
# text_corpus <- tm_map(text_corpus, stemDocument)
text_corpus <- tm_map(text_corpus, stripWhitespace)
# convert to dtm and tdm just to have options
dtm <- DocumentTermMatrix(text_corpus)
tdm <- TermDocumentMatrix(text_corpus)

# Tokenization
library("RWeka")

Tokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))}
unigram <- list(tokenize=words, bounds=list(global=c(10,Inf)))

BigramTokenizer <- function(x){RWeka::NGramTokenizer(x, RWeka::Weka_control(min=2,max=2))}
bigram <- list(tokenize = BigramTokenizer, bounds=list(global=c(10,Inf)))

TrigramTokenizer <- function(x){RWeka::NGramTokenizer(x, RWeka::Weka_control(min=3,max=3))}
trigram <- list(tokenize = TrigramTokenizer, bounds=list(global=c(10,Inf)))

# Apply the tokenizing functions to the corpus

unigram.tdm <- TermDocumentMatrix(text_corpus, control=unigram)
#10,745 unique unigrams
bigram.tdm <- TermDocumentMatrix(text_corpus, control=bigram)
#35,614 unique bigrams
trigram.tdm <- TermDocumentMatrix(text_corpus, control=trigram)
#7875 unique trigrams
#ngrams.tdm <- do.call(tm:::c.TermDocumentMatrix, list(unigram.tdm, bigram.tdm, trigram.tdm))
# 54,234 unique uni+bi+trigrams
#ngrams.tdm.df <- as.data.frame(inspect(ngrams.tdm))


## trying again, but dtm to keep words in columns
unigram.dtm <- DocumentTermMatrix(text_corpus, control=unigram)
unigram.dtm.df<- as.data.frame(inspect(unigram.dtm))
#10,745 unique unigrams
bigram.dtm <- DocumentTermMatrix(text_corpus, control=bigram)
bigram.dtm.df<- as.data.frame(inspect(bigram.dtm))
#35,614 unique unigrams
trigram.dtm <- DocumentTermMatrix(text_corpus, control=trigram)
trigram.dtm.df<- as.data.frame(inspect(trigram.dtm))
#7875 unique trigrams

# pull it all together (THIS IS USED BELOW)
ngrams.dtm.df <- cbind(unigram.dtm.df, bigram.dtm.df, trigram.dtm.df)

# collapse ngrams.dtm.df (which has strains in rows) to a single row of word counts for all review
ttl.ngrams<-colSums(ngrams.dtm.df)
ttl.ngrams.pct<-as.data.frame(ttl.ngrams/sum(ttl.ngrams))


#ngrams.dtm <- do.call(tm:::c.DocumentTermMatrix, list(unigram.dtm, bigram.dtm, trigram.dtm))
# 54,234 unique uni+bi+trigrams
#ngrams.dtm.df <- as.data.frame(inspect(ngrams.dtm))

################################################################################
# word frequency
################################################################################

# Get overall frequency of ngrams by summing all columns
ngrams.freq<-colSums(ngrams.dtm.df)
ngrams.freq.df<-as.data.frame(ngrams.freq)
# put words (which are rownames) into the first column
names<-rownames(ngrams.freq.df)
rownames(ngrams.freq.df)<-NULL
ngrams.freq.df<-cbind(names,ngrams.freq.df)
# order on word count
ngrams.freq.df<-ngrams.freq.df[order(-ngrams.freq.df$ngrams.freq),]
rownames(ngrams.freq.df)<-NULL

write.csv(ngrams.freq.df,"review_ngram_frequency.csv")

# testing word frequencies by strain (skipping the tokenization above)
dtm_df <- as.data.frame(inspect(dtm))
# 130,217 unique words in 962 strains


################################################################################
# Messing around with wordclouds an vizualization
################################################################################

library("slam")

# get ngram frequency by document with words in the rows and docs in columns
unigram_freq <- rowapply_simple_triplet_matrix(unigram.tdm,sum)
bigram_freq <- rowapply_simple_triplet_matrix(bigram.tdm,sum)
trigram_freq <- rowapply_simple_triplet_matrix(trigram.tdm,sum)
# ngrams_freq <- rowapply_simple_triplet_matrix(ngrams.tdm,sum) #changed input


par(mfrow = c(1,4), oma=c(0,0,3,0))
hist(unigram_freq, breaks = 50, main = 'unigram count', xlab='log(frequency)', ylab='')
hist(bigram_freq, breaks = 50, main = 'bigram count', xlab='log(frequency)', ylab='')
hist(trigram_freq, breaks = 50, main = 'trigram count', xlab='log(frequency)', ylab='')
hist(ngrams_freq, breaks = 50, main = 'trigram count', xlab='log(frequency)', ylab='')
title("Combined Data NGram Counts",outer=T)


# Wordclouds
library(wordcloud)

# wordcloud
par(mfrow = c(1,1))
wordcloud(
  words=names(unigram_freq),
  freq=unigram_freq, 
  scale=c(2,0.5), 
  max.words=100, 
  min.freq=3,
  random.order=FALSE, 
  rot.per=0.35, 
  use.r.layout=TRUE,
  colors=brewer.pal(8, "Paired"))

wordcloud(
  words=names(bigram_freq),
  freq=bigram_freq, 
  scale=c(2,0.5), 
  max.words=100, 
  min.freq=3,
  random.order=FALSE, 
  rot.per=0.35, 
  use.r.layout=TRUE,
  colors=brewer.pal(8, "Paired"))

wordcloud(
  words=names(trigram_freq),
  freq=trigram_freq, 
  scale=c(2,0.5), 
  max.words=100, 
  min.freq=3,
  random.order=FALSE, 
  rot.per=0.35, 
  use.r.layout=TRUE,
  colors=brewer.pal(8, "Paired"))

wordcloud(
  words=names(ngrams_freq),
  freq=trigram_freq, 
  scale=c(2,0.5), 
  max.words=100, 
  min.freq=3,
  random.order=FALSE, 
  rot.per=0.35, 
  use.r.layout=TRUE,
  colors=brewer.pal(8, "Paired"))


################################################################################
# Compare to keyword sets
################################################################################

#base words
brand1_kw <- c("strong", "strongest", "potent", "powerful")
brand2_kw <- c("sharing", "friends", "buddies", "party", "birthday", "gathering", "weak", "mellow", "beginner")
brand3_kw <- c("brand3", "music", "jamaica", "create", "creativity", "art")
brand4_kw <- c("rare", "exotic", "amazing", "unique", "special", "different")
brand5_kw <- c("yoga", "mom", "girl", "girlfriend", "bath", "relax", "meditation")
brand6_kw <- c("value", "affordable", "budget", "cheap", "space", "star", "trek")

# Expanded
brand1_kw <- c("strong", "strongest", "stronger", "potent", "powerful", "potency", "intense", "dank")
brand2_kw <- c("sharing", "friends", "buddies", "peeps", "party", "homies", "birthday", "gathering", "weak", "mellow", "beginner", "social")
brand3_kw <- c("brand3", "music", "reggae", "jamaica", "create", "creativity")
brand4_kw <- c("rare", "exotic", "amazing", "unique", "special", "different", "uncommon", "unusual", "extraordinary", "exceptional")
brand5_kw <- c("yoga", "mom", "girl", "girlfriend", "woman", "bath", "relax", "relaxing", "meditation")
brand6_kw <- c("value", "affordable", "budget", "cheap", "bargain")

# Expanded based on frequency analysis
brand1_kw <- c("strong", "super","heavy", "potent","intense","extremely","dank","powerful","chronic","pure","potency","overwhelming","stronger","hits hard","one hit","knocked","high thc","strongest","potent strain","right away","power","strong high","overpowering","strong head","thc content","strong strain","really high","intense high")
brand2_kw <- c("mellow","light","chill","people","friends","easy","joint","social","mild","friend","classic","smooth smoke","balanced","laughing","mellow high","talk","playing","talking","nice mellow","together","beginners","conversation","recreational","weak","weekend","common","conversations","group")
brand3_kw <- c("music","peace","peaceful", "brand3")
brand4_kw <- c("perfect","awesome","purple","fruity","orange","citrus","tasty","hairs","smell tastes","delicious","tasted","scent","crystals","great taste","unique","orange hairs","trichomes","red","frosty","color","fruit","top shelf","special","cherry","aftertaste","distinct","chocolate","dense buds","golden","rare","crystal","red hairs","coated")
brand5_kw <- c("yoga","wellness","meditation") #staying away from feminine and being more about wellness, but not a lot of words
brand6_kw <- c("pretty good","price","average","normal","regular","typical") #again, not enough good words that are brand-relevant

all_kw<- c(brand1_kw, brand2_kw, brand3_kw, brand4_kw, brand5_kw, brand6_kw)

# get word count per document
doc_words <- rowSums(as.matrix(ngrams.dtm.df))

# grab columns from the ngrams.dtm.df from the keyword list
ngrams.kw <- ngrams.dtm.df[,all_kw]

# read in the strain_lookup to append to StrainIds
strain_lookup <- read.csv("strain_lookup.csv", header = T)
strain_lookup_tbl <- tbl_df(strain_lookup) %>%
  select(Id,Name,Category) %>%
  mutate(StrainID = gsub("strains-","",Id)) %>%
  select(StrainID, Name, Category)

# select columns relevant to the brand words and get the % of total words
# and test for significance (that the keyword frequency is >E(X) for each strain)
# E(X) is the expected keyword frequency (of the keyword group) per strain

brand1_results <- ngrams.kw %>%
  select(1:length(brand1_kw)) %>%
  mutate(total_terms = rowSums(.), pct_words = total_terms/doc_words, strain_terms = doc_words) %>%
  cbind(StrainID = top.strain.data.agg$StrainId) %>%
  merge(strain_lookup_tbl) %>%
  arrange(desc(pct_words)) 
  
brand2_results <- ngrams.kw %>%
  select((length(brand1_kw)+1):(length(brand1_kw)+length(brand2_kw))) %>%
  mutate(total_terms = rowSums(.), pct_words = total_terms/doc_words, strain_terms = doc_words) %>%
  cbind(StrainID = top.strain.data.agg$StrainId) %>%
  merge(strain_lookup_tbl) %>%
  arrange(desc(pct_words))
  
brand3_results <- ngrams.kw %>%
  select((length(brand1_kw)+length(brand2_kw)+1):
           (length(brand1_kw)+length(brand2_kw)+length(brand3_kw))) %>%
  mutate(total_terms = rowSums(.), pct_words = total_terms/doc_words, strain_terms = doc_words) %>%
  cbind(StrainID = top.strain.data.agg$StrainId) %>%
  merge(strain_lookup_tbl) %>%
  arrange(desc(pct_words))
  
brand4_results <- ngrams.kw %>%
  select((length(brand1_kw)+length(brand2_kw)+length(brand3_kw)+1):
           (length(brand1_kw)+length(brand2_kw)+length(brand3_kw)+length(brand4_kw))) %>%
  mutate(total_terms = rowSums(.), pct_words = total_terms/doc_words, strain_terms = doc_words) %>%
  cbind(StrainID = top.strain.data.agg$StrainId) %>%
  merge(strain_lookup_tbl) %>%
  arrange(desc(pct_words))
  
brand5_results <- ngrams.kw %>%
  select((length(brand1_kw)+length(brand2_kw)+length(brand3_kw)+length(brand4_kw)+1):
           (length(brand1_kw)+length(brand2_kw)+length(brand3_kw)+length(brand4_kw)+length(brand5_kw))) %>%
  mutate(total_terms = rowSums(.), pct_words = total_terms/doc_words, strain_terms = doc_words) %>%
  cbind(StrainID = top.strain.data.agg$StrainId) %>%
  merge(strain_lookup_tbl) %>%
  arrange(desc(pct_words))

brand6_results <- ngrams.kw %>%
  select((length(brand1_kw)+length(brand2_kw)+length(brand3_kw)+length(brand4_kw)+length(brand5_kw)+1):
           (length(brand1_kw)+length(brand2_kw)+length(brand3_kw)+length(brand4_kw)+length(brand5_kw)+length(brand6_kw))) %>%
  mutate(total_terms = rowSums(.), pct_words = total_terms/doc_words, strain_terms = doc_words) %>%
  cbind(StrainID = top.strain.data.agg$StrainId) %>%
  merge(strain_lookup_tbl) %>%
  arrange(desc(pct_words))

# create expected value per keyword set
ex_brand1<-sum(brand1_results[,"total_terms"])/sum(brand1_results[,"strain_terms"])
ex_brand2<-sum(brand2_results[,"total_terms"])/sum(brand2_results[,"strain_terms"])
ex_brand3<-sum(brand3_results[,"total_terms"])/sum(brand3_results[,"strain_terms"])
ex_brand4<-sum(brand4_results[,"total_terms"])/sum(brand4_results[,"strain_terms"])
ex_brand5<-sum(brand5_results[,"total_terms"])/sum(brand5_results[,"strain_terms"])
ex_brand6<-sum(brand6_results[,"total_terms"])/sum(brand6_results[,"strain_terms"])

# filter for >x terms (should be significance, but I don't have the time to figure out)
# and filter for >E(X) 
# and arrange the columns/relabel so it makes more sense
brand1_results<-brand1_results %>% #filter(brand1_results, total_terms>=50, pct_words>ex_brand1) %>%
  select(StrainID,Name,Category,strain_ttl_terms = strain_terms,strain_kw = total_terms,kw_pct_terms = pct_words, everything(brand1_kw))
brand2_results<-brand2_results %>% #filter(brand2_results, total_terms>=50, pct_words>ex_brand2) %>%
  select(StrainID,Name,Category,strain_ttl_terms = strain_terms,strain_kw = total_terms,kw_pct_terms = pct_words, everything(brand2_kw))
brand3_results<-brand3_results %>% #filter(brand3_results, total_terms>=50, pct_words>ex_brand3) %>%
  select(StrainID,Name,Category,strain_ttl_terms = strain_terms,strain_kw = total_terms,kw_pct_terms = pct_words, everything(brand3_kw))
brand4_results<- brand4_results %>% #filter(brand4_results, total_terms>=50, pct_words>ex_brand4) %>%
  select(StrainID,Name,Category,strain_ttl_terms = strain_terms,strain_kw = total_terms,kw_pct_terms = pct_words, everything(brand4_kw))
brand5_results<-brand5_results %>% #filter(brand5_results, total_terms>=50, pct_words>ex_brand5) %>%
  select(StrainID,Name,Category,strain_ttl_terms = strain_terms,strain_kw = total_terms,kw_pct_terms = pct_words, everything(brand5_kw))
brand6_results<- brand6_results %>% #filter(brand6_results, total_terms>=50, pct_words>ex_brand6) %>%
  select(StrainID,Name,Category,strain_ttl_terms = strain_terms,strain_kw = total_terms,kw_pct_terms = pct_words, everything(brand6_kw))


################################################################################
# Write to xlsx
################################################################################

# Writing an .xlsx with each sheet being a result data frame
library(xlsx)
write.xlsx(brand1_results, file="strain_review_results.xlsx", sheetName="brand1", row.names = FALSE)
write.xlsx(brand2_results, file="strain_review_results.xlsx", sheetName="brand2", append=TRUE, row.names = FALSE)
write.xlsx(brand3_results, file="strain_review_results.xlsx", sheetName="brand3", append=TRUE, row.names = FALSE)
write.xlsx(brand4_results, file="strain_review_results.xlsx", sheetName="brand4", append=TRUE, row.names = FALSE)
write.xlsx(brand5_results, file="strain_review_results.xlsx", sheetName="brand5", append=TRUE, row.names = FALSE)
write.xlsx(brand6_results, file="strain_review_results.xlsx", sheetName="brand6", append=TRUE, row.names = FALSE)


################################################################################
# Select strain review data by strain ID
################################################################################
strainReviewsText <- function(strain.name) {
  strain.id<-as.numeric(strain_lookup_tbl[strain_lookup_tbl$Name==strain.name,1])
  reviewData<-top.strain.data[top.strain.data$StrainId==strain.id,]
  reviewData<-merge(reviewData, strain_lookup_tbl, by.x = "StrainId", by.y = "StrainID")
  View(reviewData)
  #write.csv(reviewData, "reviewData.csv")
}

strainReviews <- function(strain.id) {
  reviewData<-top.strain.data[top.strain.data$StrainId==strain.id,]
  reviewData<-merge(reviewData, strain_lookup_tbl, by.x = "StrainId", by.y = "StrainID")
  View(reviewData)
  #write.csv(reviewData, "reviewData.csv")
}




