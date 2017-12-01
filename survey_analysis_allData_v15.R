########################################################################################################################
# VERSION NOTES
########################################################################################################################
# V15: This was my first major project and I hadn't yet started keeping track of version notes.
#       I was super proud of this script at the time and it works, but if you're a reader of this note, please
#       know I've only included it in my example work to illustrate how far I've come and to show an example
#       of my first time using the PAM algorithm to cluster data



################################################################################
# Data Prep from Export
################################################################################
# .csv export cleanup from Survey MOnkey (minimize this so most is scripted)
## move all questions in row 1 to row 2 where row 2 says "Response"
## delete the empty columns for names/ip/etc
################################################################################
# Reading the data
################################################################################

# read data for WA and CA separately so we can append state in a column later
wa.raw<-read.csv("surveymonkey_WA_results_condensed_numeric_clean.csv", header = T)
ca.raw<-read.csv("surveymonkey_CA_results_condensed_numeric_clean_v2.csv", header = T)

# append a column for state to each dataset
wa.raw$state<-rep("1", times = dim(wa.raw)[1])
ca.raw$state<-rep("2", times = dim(ca.raw)[1])
data.raw1<-rbind(wa.raw,ca.raw)

################################################################################
# Cleaning the data
################################################################################
# clean the column names

x<-c("ID","collector","startDate","endDate","altID","cannabisUse","ethnicity",
     "ethnicityMult","edu","HHI","empStatus","ind","relationshipStatus",
     "children","medRec","medSymp","medOther","recCreativity","recSleep",
     "recPain","recStress","recRelax","recEuphoria","recDepression",
     "recProductive","recBoredom","recApetite","recOther","useFreq","useFreqIllegal",
     "useAlone","usePartner","useFriends","typeJoint","typeJointCon","typePipe",
     "typeHandVape","typeOtherVape","typeDabRig","typeHashPipe","typeEdible",
     "typeTopical","typeTincture","typeOther","typeFav","typeFavOther",
     "consFlower","consKief","consOil","consWax","consEdible","constopical",
     "consTincture","timeMorning","timeAfternoon","timeEvening","timeNight",
     "actStudy","actVideoGame","actSocialize","actClean","actAlcohol",
     "actMedia","actOutdoor","actWeb","actSex","actExercise","actWork",
     "actOther", "healthConcern","ownVapePenReplace","ownVapePenDisp",
     "ownVapeFlower","ownVapeOther","ownPipe","ownPaper","ownDabRig","ownNone",
     "ownOther","cannabisKnowledge","alcoholUse","drugUse","tooHigh",
     "purchaseFreq","sourceDispensary","sourceFriends","sourceDealer",
     "sourceGrow","ppCultivar","ppStrain","ppPrice","ppBrand","ppTHC","ppCBD",
     "ppSymptom","ppAppearance","ppGrower","ppOrganic","ppInsideOutside",
     "knowDispensary","knowSpend","knowBrand","knowCat","knowStrain","packTHC",
     "packCBD","packTtl","packHarvestDate","packMedUse","packGrower",
     "packBrand","packDiscrete","packAttractive","packDose","packTransparent",
     "packRecyclable","packOrganic","packPesticide","favStrain1","favStrain2",
     "favStrain3","favBrand1","favBrand2","favBrand3","edibleCandies",
     "edibleChocolates","edibleCookies","edibleCaramels","edibleMints",
     "edibleOther","age","gender","deviceType","state")
colnames(data.raw1)<-x

# remove columns that won't be used in the model
unused<-c("collector","startDate","endDate","altID","ethnicityMult","medOther",
          "recOther","typeOther","typeFavOther","actOther","ownOther",
          "favStrain1","favStrain2","favStrain3","favBrand1","favBrand2",
          "favBrand3","edibleOther","deviceType")
data.raw2<-data.raw1[,!(names(data.raw1) %in% unused)]

# filter out the non-users
data.raw3<-subset(data.raw2, data.raw2$cannabisUse==1)

# Group multi-checkbox questions together for cleaning
multi.checkbox.qs<-c("recCreativity","recSleep","recPain","recStress","recRelax","recEuphoria",
                     "recDepression","recProductive","recBoredom","recApetite","typeJoint",
                     "typeJointCon","typePipe","typeHandVape","typeOtherVape","typeDabRig",
                     "typeHashPipe","typeEdible","typeTopical","typeTincture","consFlower",
                     "consKief","consOil","consWax","consEdible","constopical","consTincture",
                     "ownVapePenReplace","ownVapePenDisp","ownVapeFlower","ownVapeOther","ownPipe",
                     "ownPaper","ownDabRig","ownNone","knowDispensary","knowSpend","knowBrand",
                     "knowCat","knowStrain","edibleCandies","edibleChocolates","edibleCookies",
                     "edibleCaramels","edibleMints")
multi.checkbox<-data.raw3[,multi.checkbox.qs]

# replace all values with 1 and all NAs with 0 since this should be binary 
# dependent on the respondent clicking the box or not
multi.checkbox[is.na(multi.checkbox)] <- 0
multi.checkbox[multi.checkbox>0] <- 1
# convert to factor (must use lapply because of some stuff I'm not quite sure about)
multi.checkbox<-data.frame(lapply(multi.checkbox,factor))


# More grouping and cleaning
multi.choice.qs<-c("cannabisUse","ethnicity","edu","HHI","empStatus","ind","relationshipStatus",
                   "children","medRec","medSymp","typeFav","alcoholUse","drugUse","tooHigh")
multi.choice<-data.raw3[,multi.choice.qs]
# '0' in medSym means they were rec and didn't answer the question (instead of NA)
multi.choice$medSymp[is.na(multi.choice$medSymp)] <- 0
# convert to factor
multi.choice<-data.frame(lapply(multi.choice,factor))

high.low.qs<-c("useFreq","useFreqIllegal","healthConcern","purchaseFreq", "cannabisKnowledge")
high.low<-data.raw3[,high.low.qs]
# sum(is.na(high.low$purchaseFreq))/length(high.low$purchaseFreq)
# QA reveals that the purchaseFreq was not a required question. 12.7% of cannabis-user responses were blank
# going to leave it as-is and see if the model can handle NAs or if we need to na.omit the 12.7%

never.always.qs<-c("useAlone","usePartner","useFriends","timeMorning",
                   "timeAfternoon","timeEvening","timeNight","actStudy",
                   "actVideoGame","actSocialize","actClean","actAlcohol",
                   "actMedia","actOutdoor","actWeb","actSex","actExercise",
                   "actWork","sourceDispensary","sourceFriends","sourceDealer",
                   "sourceGrow")
never.always<-data.raw3[,never.always.qs]
# test for % NA by column
# never.always.NA<-sapply(never.always, function(x){sum(is.na(x)/length(x)*100)})
# this shows that they all have about 5% NA except the source Q's, which are at up to 14%
# looks like there is a field under the requirement which specifies how many fields much be required; 
# The default is 1, which is why the source Qs have such a high NA%

rank.qs<-c("ppCultivar","ppStrain","ppPrice","ppBrand","ppTHC","ppCBD",
           "ppSymptom","ppAppearance","ppGrower","ppOrganic","ppInsideOutside","packTHC",
           "packCBD","packTtl","packHarvestDate","packMedUse","packGrower",
           "packBrand","packDiscrete","packAttractive","packDose","packTransparent",
           "packRecyclable","packOrganic","packPesticide")
rank<-data.raw3[,rank.qs]
# rank.NA<-sapply(rank, function(x){sum(is.na(x))/length(x)})
# looks like it's 12% NA; have no idea how we have any since it's required...

no.qa.qs<-c("ID","age","gender","state")
no.qa<-data.raw3[,no.qa.qs]
# no.qa.NA<-sapply(no.qa, function(x){sum(is.na(x))/length(x)})
# age and gender have 12% NA; ID is 0% NA
# convert to the correct classes
# no.qa$ID<-as.numeric(no.qa$ID) # already numeric, not sure why I did this
no.qa$age<-as.factor(no.qa$age)
no.qa$gender<-as.factor(no.qa$gender)
no.qa$state<-as.factor(no.qa$state)

##### Test to see if any questions are missing
# all.qs<-c(multi.checkbox.qs,multi.choice.qs,high.low.qs,low.high.qs,never.always.qs,rank.qs, no.qa.qs)
# testing to see which questions didn't make it through to all.qs
#'%nin%' <- Negate('%in%')
#missed<-colnames(data.raw3)[colnames(data.raw3) %nin% all.qs]

# pull it all together
data.raw4<-cbind(no.qa,rank,never.always,high.low,multi.choice,multi.checkbox)
#'%nin%' <- Negate('%in%')
# missed<-colnames(data.raw4)[colnames(data.raw4) %nin% colnames(data.raw4)]

# Remove responses with NAs
data.raw5<-na.omit(data.raw4)
# 1-dim(data.raw5)[1]/dim(data.raw4)[1]
# We lose 17.4% of responses due to NAs... that sucks; still 1776 total (the FREE-est number)

# I learned that ethnicity and typeFav have 0s and that's throwing off my functions for labeling later on
# To correct, I'm just going to add 1 to these columns
# I'm going to leave it data.raw5 so I don't have to worry about updating all of the references to it below

data.raw5$ethnicity<-as.factor(as.integer(data.raw5$ethnicity)) # TIL that converting a 0 factor into integer and back makes it a 1, ??
data.raw5$typeFav<-as.factor(as.integer(data.raw5$typeFav))

################################################################################
# Adjust bucketing to increase statistical significance (Unused)
################################################################################
# making a fake function to comment out the below
.f<- function() {
# column classes (already done, but just for reference) 
# high.low, never.always, and rank should be integers
# multi.checkbox and multi.choice should be factors
# within no.qa, ID should be integer; age, gender, and state should be factors

# to better the odds of getting to statistical significant, we need to reduce the number of buckets depending on the question
# only need to bucket the int columns and each has a different number of values
# first thing is first, let's figure out how many unique values are in each int-class column
int.col<-data.raw5[,c(high.low.qs,never.always.qs,rank.qs)]

# my methodology is to boil each column with unique values of 4-14 into groupings of only 3 (like low, med, high, but 1, 2, 3)
# best approach won't be even buckets like (1-2 = 1, 3-4 = 2, and 5-6 = 3) since consumers tend to group near the high end when rating
# approach im taking is to bucket responses such that about 1/3 fit into each bucket to maximize significance
# NOTE: I'll need to make sure this approach is relevant per quesion
# going through each question in order of the columns
# useFreq has multiple times per day at 1 and less than once per month at 6
## need to move 1 out of the group, then back in to avoid overlap (using 99)
int.col$useFreq[int.col$useFreq==1]<-99
int.col$useFreq[int.col$useFreq==2|int.col$useFreq==3]<-2
int.col$useFreq[int.col$useFreq==4|int.col$useFreq==5|int.col$useFreq==6]<-1
int.col$useFreq[int.col$useFreq==99]<-3
# useFreqIllegal is the same as useFreq except 7 is Never
int.col$useFreqIllegal[int.col$useFreqIllegal==1]<-99
int.col$useFreqIllegal[int.col$useFreqIllegal==2|int.col$useFreqIllegal==3|int.col$useFreqIllegal==4]<-2
int.col$useFreqIllegal[int.col$useFreqIllegal==5|int.col$useFreqIllegal==6|int.col$useFreqIllegal==7]<-1
int.col$useFreqIllegal[int.col$useFreqIllegal==99]<-3
# health concern (high is 1; 4 possible)
int.col$healthConcern[int.col$healthConcern==1|int.col$healthConcern==2]<-99
int.col$healthConcern[int.col$healthConcern==3]<-2
int.col$healthConcern[int.col$healthConcern==4]<-1
int.col$healthConcern[int.col$healthConcern==99]<-3
# purchaseFreq (high is 1; 5 possible)
int.col$purchaseFreq[int.col$purchaseFreq==1|int.col$purchaseFreq==2]<-99
int.col$purchaseFreq[int.col$purchaseFreq==3|int.col$purchaseFreq==4]<-2
int.col$purchaseFreq[int.col$purchaseFreq==5]<-1
int.col$purchaseFreq[int.col$purchaseFreq==99]<-3


# all the use, time, act, source, and cannabisUse columns have a high of 5 with 5 possible (write a function)
# based on the answer distribution, I'm bucketing 1=1, 2to3=2, and 3to4=3 to get the best results while still looping and not writing anything custom per question
for(i in 5:27) {
  int.col[i][int.col[i]==3]<-2
  int.col[i][int.col[i]==4|int.col[i]==5]<-3
}
# all the pp columns have a LOW of 11 with 11 possible (i.e. a rank of 11 is the lowest you can give)
# 1,2,3=3, 4,5,6,7=2, 8,9,10,11=1
for(i in 28:38) {
  int.col[i][int.col[i]==2|int.col[i]==3]<-3
  int.col[i][int.col[i]==4|int.col[i]==5|int.col[i]==6|int.col[i]==7]<-2
  int.col[i][int.col[i]==8|int.col[i]==9|int.col[i]==10|int.col[i]==11]<-3
}
# the pack columns are out of 14 and are ranks like the pp columns, which need to be re-bucketed similarly
# 1,2,3,4=3, 5,6,7,8,9=2 10,11,12,13,14=1
for(i in 39:52) {
  int.col[i][int.col[i]==2|int.col[i]==3|int.col[i]==4]<-3
  int.col[i][int.col[i]==5|int.col[i]==6|int.col[i]==7|int.col[i]==8|int.col[i]==9]<-2
  int.col[i][int.col[i]==10|int.col[i]==11|int.col[i]==12|int.col[i]==13|int.col[i]==14]<-2
}

# reclassify from num to int
int.col<-data.frame(lapply(int.col,as.integer))

# test column values to make sure there are only 3 buckets
#int.lengths<-sapply(int.col,function(x){length(table(x))})

# replace the data.raw5 columns we extracted and re-bucketed into data.raw6
'%nin%' <- Negate('%in%')
nint.col<-which(colnames(data.raw5) %nin% c(high.low.qs,never.always.qs,rank.qs))
data.clean<-cbind(data.raw5[,nint.col],int.col)
}
################################################################################
# Resampling Men
################################################################################
library(dplyr)
female<-data.raw5 %>% filter(gender==1)
male<-data.raw5 %>% filter(gender==2)
# break into age groups
m1<-subset(male, age==2)
m2<-subset(male, age==3)
m3<-subset(male, age==4)
m4<-subset(male, age==5)
set.seed(1234)
# trying to do multiple-resampling
# https://stats.stackexchange.com/questions/104040/resampling-simulation-methods-monte-carlo-bootstrapping-jackknifing-cross
#boot.m1<- list()
#boot.m2<- list()
#boot.m3<- list()
#boot.m4<- list()
#for(i in 1:1000) {
#  boot.m1[[i]] <- sample(male, 737, replace = T)
#  boot.m2[[i]] <- sample(male, 812, replace = T)
#  boot.m3[[i]] <- sample(male, 333, replace = T)
#  boot.m4[[i]] <- sample(male, 90, replace = T)
#}
# single resampling
boot.m1 <- sample_n(male, 737, replace = T)
boot.m2 <- sample_n(male, 812, replace = T)
boot.m3 <- sample_n(male, 333, replace = T)
boot.m4 <- sample_n(male, 90, replace = T)
boot.m<-rbind(boot.m1,boot.m2,boot.m3,boot.m4)

#combine results
data.boot<-rbind(boot.m,female)

################################################################################
# Creating Data Features (skip)
################################################################################
# goal is to create an age/gender feature that can work like any other variable in data.explore below
# if I convert the 8 combinations of age/gender into pasted integers, then I should be set for a lookup

data.featured<-data.boot[,-1] # remove ID

data.featured$ageGender<- NA # create an ageGender column

# paste age and gender together in ageGender
for(i in 1:dim(data.featured)[1]) {
  data.featured$ageGender[i]<-paste0(data.explore$age[i], data.explore$gender[i])
  # then replace
}



################################################################################
# Generating Summary Data (single chart version) (can skip)
################################################################################
# I want to write a function that loops through each column and generates summary views/statistics that I can export via .pdfs for presentation
# perhaps a function that allows you to cut the data by facets.  For instance, function(age,gender) would plot a histogram of age by gender
# perhaps this function loops through all combinations of variables to be completely thorough (excluding looking at paired cuts such as the age/gender of HHI)
library(ggplot2)
library(scales)
# need to convert int columns 
# problem is that converting to factor seems to fuck up the order for typeFav and possibly others like the ranks and ethnicity
# data.explore<-data.frame(lapply(data.boot,factor))[,-1]
data.explore<-data.frame(lapply(data.featured,factor)) #use featured for ageGender to be included
# data.explore<-data.frame(sapply(data.boot,factor))[,-1] #sapply was causing the problem with the order of the labels and results


# use the lookup as a tool to convert factors to have the right data
# lookup2 has some changes where there is a "1_", "2_", etc. to help the ordering of the labels; also there was a fix for state to make it an integer
# lookup3 fixes the NA issue for binaries by shifting from 0 and 1 to 1 and 2
# lookup4 corrects some NA issues that resulted from not having an Other and also corrects age 18-29 to be 21-29
# lookup5 fixes an issue with typeFav and ethnicity since they had "0" factors for other that were messing up the labels
lookup<-read.csv("survey_value_lookup5.csv", header = T)


# https://sebastiansauer.github.io/improved_bar_stacking_ggplot2_220/
# good stuff about the differences between geom_col and geom_bar
# geom_text(aes(label = ifelse(p < .05, NA, p2), y = p), position = "stack")
    # to prevent small labels from showing up

# function which takes as input two variable names from data.explore and cuts the first variable by the second
# Areas for improvement:
  # Get the plot to put the x-variable percentages above each bar (seems to be hard during testing given that I'm having the cut-by variable percentages in there)
explore<-function(var1,var2) {
  # condense the data for ggplot
  condense.df<-tbl_df(cbind(data.explore[[var1]], data.explore[[var2]])) %>%
    group_by(V1,V2) %>%
    summarize(n=n()) %>%
    mutate(percent = n/sum(n)*100)
  colnames(condense.df)<-c(colnames(data.explore[var1]),colnames(data.explore[var2]),"n","percent")
  condense.df<-data.frame(condense.df)

  # perform replacement on condense.df so that when it's read into the plot, it has the right labels
  for(i in 1:dim(condense.df)[1]) {
    condense.df[i,1]<-paste(colnames(condense.df[1]),condense.df[i,1],sep="")
    condense.df[i,2]<-paste(colnames(condense.df[2]),condense.df[i,2],sep="")
  }
  condense.df[1]<-lookup$replacement[match(unlist(condense.df[1]),lookup$colval)]
  condense.df[2]<-lookup$replacement[match(unlist(condense.df[2]),lookup$colval)]
  
  # plot
  ggplot(condense.df,aes(x=condense.df[[var1]], y= n, fill=condense.df[[var2]])) +
    geom_bar(stat="identity") +
    #geom_col() + # same as geom_bar(stat="identity")
    geom_text(aes(label = paste0(round(percent,0),"%")), 
              position = position_stack(vjust = 0.5)) +
    ylab("Frequency") + 
    xlab(colnames(condense.df[1])) +
    scale_fill_discrete(name=colnames(condense.df[2])) +
    ggtitle(paste(colnames(condense.df[1]),"cut by",
                  colnames(condense.df[2]), sep=" "))+
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
  
}


# function the same as explore, but accepts numbers for column references (useful for looping)
explore.num<-function(num1,num2) {
  var1<-colnames(data.explore[num1])
  var2<-colnames(data.explore[num2])
  # condense the data for ggplot
  condense.df<-tbl_df(cbind(data.explore[[var1]], data.explore[[var2]])) %>%
    group_by(V1,V2) %>%
    summarize(n=n()) %>%
    mutate(percent = n/sum(n)*100)
  colnames(condense.df)<-c(colnames(data.explore[var1]),colnames(data.explore[var2]),"n","percent")
  condense.df<-data.frame(condense.df)
  
  # perform replacement on condense.df so that when it's read into the plot, it has the right labels
  for(i in 1:dim(condense.df)[1]) {
    condense.df[i,1]<-paste(colnames(condense.df[1]),condense.df[i,1],sep="")
    condense.df[i,2]<-paste(colnames(condense.df[2]),condense.df[i,2],sep="")
  }
  condense.df[1]<-lookup$replacement[match(unlist(condense.df[1]),lookup$colval)]
  condense.df[2]<-lookup$replacement[match(unlist(condense.df[2]),lookup$colval)]
  
  # plot
  ggplot(condense.df,aes(x=condense.df[[var1]], y= n, fill=condense.df[[var2]])) +
    geom_col() +
    geom_text(aes(label = paste0(round(percent,0),"%")), 
              position = position_stack(vjust = 0.5)) +
    ylab("Frequency") + 
    xlab(colnames(condense.df[1])) +
    scale_fill_discrete(name=colnames(condense.df[2])) +
    ggtitle(paste(colnames(condense.df[1]),"cut by",
                  colnames(condense.df[2]), sep=" "))+
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
}

# function which accepts a number for a column reference in data.explore and cuts
#   that variable by every other variable.  Next, the generated plots are converted
#   to a .pdf with 1 page per plot
var.explore<-function(x){
  name<-paste0(sprintf("%03d",x),"_",colnames(data.explore[x]))
  num.plots<-length(colnames(data.explore))
  my.plots<-vector(num.plots, mode='list')
  for(i in 1:length(colnames(data.explore))) {
    #print was required to make it work, but I think this is slowing it down
    print(explore.num(x,i))
    my.plots[[i]]<-recordPlot()
  }
  
  graphics.off()
  
  pdf(paste0(name,".pdf"),onefile=TRUE)
  for (my.plot in my.plots) {
    replayPlot(my.plot)
  }
  graphics.off()
}

# function that performs var.explore on every variable
full.explore<-function(){
  for(i in 1:dim(data.explore)[2]){
    var.explore(i)
  }
}

##### All of the above functions, but with a 100% stacked chart

explore.stack<-function(var1,var2) {
  # condense the data for ggplot
  condense.df<-tbl_df(cbind(data.explore[[var1]], data.explore[[var2]])) %>%
    group_by(V1,V2) %>%
    summarize(n=n()) %>%
    mutate(percent = n/sum(n)*100)
  colnames(condense.df)<-c(colnames(data.explore[var1]),colnames(data.explore[var2]),"n","percent")
  condense.df<-data.frame(condense.df)
  
  # perform replacement on condense.df so that when it's read into the plot, it has the right labels
  for(i in 1:dim(condense.df)[1]) {
    condense.df[i,1]<-paste(colnames(condense.df[1]),condense.df[i,1],sep="")
    condense.df[i,2]<-paste(colnames(condense.df[2]),condense.df[i,2],sep="")
  }
  condense.df[1]<-lookup$replacement[match(unlist(condense.df[1]),lookup$colval)]
  condense.df[2]<-lookup$replacement[match(unlist(condense.df[2]),lookup$colval)]
  
  # plot
  ggplot(condense.df, aes(x=condense.df[[var1]], weight = n, fill=condense.df[[var2]], y=percent/100)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = paste0(round(percent,0),"%"),y = percent/100), position =position_stack(vjust = 0.5), color = "white") + 
    scale_y_continuous(labels=percent) +
    ggtitle(paste(colnames(condense.df[1]),"cut by",
                  colnames(condense.df[2]), sep=" "))+
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    ylab("Frequency") + 
    xlab(colnames(condense.df[1])) +
    scale_fill_discrete(name=colnames(condense.df[2]))
  
}


# function the same as explore, but accepts numbers for column references (useful for looping)
explore.num.stack<-function(num1,num2) {
  var1<-colnames(data.explore[num1])
  var2<-colnames(data.explore[num2])
  # condense the data for ggplot
  condense.df<-tbl_df(cbind(data.explore[[var1]], data.explore[[var2]])) %>%
    group_by(V1,V2) %>%
    summarize(n=n()) %>%
    mutate(percent = n/sum(n)*100)
  colnames(condense.df)<-c(colnames(data.explore[var1]),colnames(data.explore[var2]),"n","percent")
  condense.df<-data.frame(condense.df)
  
  # perform replacement on condense.df so that when it's read into the plot, it has the right labels
  for(i in 1:dim(condense.df)[1]) {
    condense.df[i,1]<-paste(colnames(condense.df[1]),condense.df[i,1],sep="")
    condense.df[i,2]<-paste(colnames(condense.df[2]),condense.df[i,2],sep="")
  }
  condense.df[1]<-lookup$replacement[match(unlist(condense.df[1]),lookup$colval)]
  condense.df[2]<-lookup$replacement[match(unlist(condense.df[2]),lookup$colval)]
  
  # plot
  ggplot(condense.df, aes(x=condense.df[[var1]], weight = n, fill=condense.df[[var2]], y=percent/100)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = paste0(round(percent,0),"%"),y = percent/100), position =position_stack(vjust = 0.5), color = "white") + 
    scale_y_continuous(labels=percent) +
    ggtitle(paste(colnames(condense.df[1]),"cut by",
                  colnames(condense.df[2]), sep=" "))+
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    ylab("Frequency") + 
    xlab(colnames(condense.df[1])) +
    scale_fill_discrete(name=colnames(condense.df[2]))
}

# function which accepts a number for a column reference in data.explore and cuts
#   that variable by every other variable.  Next, the generated plots are converted
#   to a .pdf with 1 page per plot
var.explore.stack<-function(x){
  name<-paste0(sprintf("%03d",x),"_",colnames(data.explore[x]),"_stacked")
  num.plots<-length(colnames(data.explore))
  my.plots<-vector(num.plots, mode='list')
  for(i in 1:length(colnames(data.explore))) {
    #print was required to make it work, but I think this is slowing it down
    print(explore.num.stack(x,i))
    my.plots[[i]]<-recordPlot()
  }
  
  graphics.off()
  
  pdf(paste0(name,".pdf"),onefile=TRUE)
  for (my.plot in my.plots) {
    replayPlot(my.plot)
  }
  graphics.off()
}




# function that performs var.explore on every variable
full.explore.stack<-function(){
  for(i in 1:dim(data.explore)[2]){
    var.explore.stack(i)
  }
}


########################################################################################################################
# CREATING COMBO VARIABLES
########################################################################################################################
# first of all, this script is fucked. it's out of order and was basically my first major project ever so don't
# hate me too much if you're trying to go through it.  data.expore is basically what we want to work with though, as far as i can tell

# I want to be able to look at combinations of variables as a single varible.  In this case, people who consume edibles
# and who rank price as their primary concern.  Then I want to generate a bunch o graphs like what explore() gives below

# It looks like I can simply create the new data feature and call it in explore so long as there is an associated set of rows
# in the lookup for the values to append

# I want 3 tiers of price where 1 or 2 is high sensitivity, 3-6 is med and 7=11 is low
consEdiblePriceSensitiver <- function(consEdible,ppPrice){
  if((consEdible==1 & ppPrice<=11)==T) output <- 1
  if((consEdible==1 & ppPrice<=8)==T) output <- 2
  if((consEdible==1 & ppPrice<=3)==T) output <- 3
  if((consEdible==2 & ppPrice<=11)==T) output <- 4
  if((consEdible==2 & ppPrice<=8)==T) output <- 5
  if((consEdible==2 & ppPrice<=3)==T) output <- 6
  output
}

# loop to append new variable
data.explore$consEdiblePriceSensitivity <- NA
for(i in 1:nrow(data.explore)){
  data.explore$consEdiblePriceSensitivity[i] <- consEdiblePriceSensitiver(as.numeric(data.explore$consEdible[i]), as.numeric(data.explore$ppPrice[i]))
}


################################################################################
# Generating More Detailed Summary Data (two-chart version)
################################################################################
# this is the next iteration that aligns the two plots on top of each other for 
# details about n as well as the % differences
library(ggplot2)
library(scales)
lookup<-read.csv("survey_value_lookup6.csv", header = T)

library(grid)
library(gtable)
explore<-function(var1,var2) {
  # condense the data for ggplot
  condense.df<-tbl_df(cbind(data.explore[[var1]], data.explore[[var2]])) %>%
    group_by(V1,V2) %>%
    summarize(n=n()) %>%
    mutate(percent = n/sum(n)*100)
  colnames(condense.df)<-c(colnames(data.explore[var1]),colnames(data.explore[var2]),"n","percent")
  condense.df<-data.frame(condense.df)
  
  # perform replacement on condense.df so that when it's read into the plot, it has the right labels
  for(i in 1:dim(condense.df)[1]) {
    condense.df[i,1]<-paste(colnames(condense.df[1]),condense.df[i,1],sep="")
    condense.df[i,2]<-paste(colnames(condense.df[2]),condense.df[i,2],sep="")
  }
  condense.df[1]<-lookup$replacement[match(unlist(condense.df[1]),lookup$colval)]
  condense.df[2]<-lookup$replacement[match(unlist(condense.df[2]),lookup$colval)]
  
  condense.df2<-tbl_df(condense.df) %>%
    group_by(condense.df[,1]) %>%
    summarize(n=sum(n)) %>%
    mutate(percent=n/sum(n)*100)
  colnames(condense.df2)<-c(colnames(condense.df[1]),"n","percent")
  condense.df2<-data.frame(condense.df2)
  
  # plot
  p1<-ggplot(condense.df2,aes(x=condense.df2[[var1]],y=n)) +
    geom_bar(stat="identity",aes(fill = condense.df2[[var1]])) +
    ylab("Responses") + 
    ggtitle(paste(colnames(condense.df[1]),"cut by",
                  colnames(condense.df[2]), sep=" "))+
    theme(axis.text.x =element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank()) +
    geom_text(aes(label=n), vjust=1.6, color="white")+
    scale_fill_discrete(name=colnames(condense.df2[1])) +
    geom_text(aes(label=paste0(round(percent,0),"%")), vjust=-0.3) 
  
  
  p2<-ggplot(condense.df, aes(x=condense.df[[var1]], weight = n, fill=condense.df[[var2]], y=percent/100)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = paste0(round(percent,0),"%"),y = percent/100), position =position_stack(vjust = 0.5), color = "white") + 
    scale_y_continuous(labels=percent) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    ylab("Frequency") + 
    xlab(colnames(condense.df[1])) +
    scale_fill_discrete(name=colnames(condense.df[2])) 
  
  g1<-ggplotGrob(p1)
  g2<-ggplotGrob(p2)
  g<-gtable:::rbind_gtable(g1, g2, "first")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels] <- unit(c(1,1), "null")
  grid.newpage()
  grid.draw(g)
  
}

# same as explore, but accepts numeric references to columns in data.explore
explore.num<-function(num1,num2) {
  var1<-colnames(data.explore[num1])
  var2<-colnames(data.explore[num2])
  # condense the data for ggplot
  condense.df<-tbl_df(cbind(data.explore[[var1]], data.explore[[var2]])) %>%
    group_by(V1,V2) %>%
    summarize(n=n()) %>%
    mutate(percent = n/sum(n)*100)
  colnames(condense.df)<-c(colnames(data.explore[var1]),colnames(data.explore[var2]),"n","percent")
  condense.df<-data.frame(condense.df)
  
  # perform replacement on condense.df so that when it's read into the plot, it has the right labels
  for(i in 1:dim(condense.df)[1]) {
    condense.df[i,1]<-paste(colnames(condense.df[1]),condense.df[i,1],sep="")
    condense.df[i,2]<-paste(colnames(condense.df[2]),condense.df[i,2],sep="")
  }
  condense.df[1]<-lookup$replacement[match(unlist(condense.df[1]),lookup$colval)]
  condense.df[2]<-lookup$replacement[match(unlist(condense.df[2]),lookup$colval)]
  
  condense.df2<-tbl_df(condense.df) %>%
    group_by(condense.df[,1]) %>%
    summarize(n=sum(n)) %>%
    mutate(percent=n/sum(n)*100)
  colnames(condense.df2)<-c(colnames(condense.df[1]),"n","percent")
  condense.df2<-data.frame(condense.df2)
  
  # plot
  p1<-ggplot(condense.df2,aes(x=condense.df2[[var1]],y=n)) +
    geom_bar(stat="identity",aes(fill = condense.df2[[var1]])) +
    ylab("Responses") + 
    ggtitle(paste(colnames(condense.df[1]),"cut by",
                  colnames(condense.df[2]), sep=" "))+
    theme(axis.text.x =element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank()) +
    geom_text(aes(label=n), vjust=1.6, color="white")+
    scale_fill_discrete(name=colnames(condense.df2[1])) +
    geom_text(aes(label=paste0(round(percent,0),"%")), vjust=-0.3) 
  
  
  p2<-ggplot(condense.df, aes(x=condense.df[[var1]], weight = n, fill=condense.df[[var2]], y=percent/100)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = paste0(round(percent,0),"%"),y = percent/100), position =position_stack(vjust = 0.5), color = "white") + 
    scale_y_continuous(labels=percent) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    ylab("Frequency") + 
    xlab(colnames(condense.df[1])) +
    scale_fill_discrete(name=colnames(condense.df[2])) 
  
  g1<-ggplotGrob(p1)
  g2<-ggplotGrob(p2)
  g<-gtable:::rbind_gtable(g1, g2, "first")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels] <- unit(c(1,1), "null")
  grid.newpage()
  grid.draw(g)
}

# function which accepts a number for a column reference in data.explore and cuts
#   that variable by every other variable.  Next, the generated plots are converted
#   to a .pdf with 1 page per plot
var.explore<-function(x){
  name<-paste0(sprintf("%03d",x),"_",colnames(data.explore[x]))
  num.plots<-length(colnames(data.explore))
  my.plots<-vector(num.plots, mode='list')
  for(i in 1:length(colnames(data.explore))) {
    #print was required to make it work, but I think this is slowing it down
    explore.num(x,i)
    my.plots[[i]]<-recordPlot()
  }
  
  graphics.off()
  
  pdf(paste0(name,".pdf"),onefile=TRUE)
  for (my.plot in my.plots) {
    replayPlot(my.plot)
  }
  graphics.off()
}

# function that performs var.explore on every variable
full.explore<-function(){
  for(i in 1:dim(data.explore)[2]){
    var.explore(i)
  }
}
################################################################################
# Significance testing
################################################################################
# I want to end up with a table of p-values which shows the significance differences between groups
# columns should be the base factor, split factor, first comparison group, second comparison group, p-val, and difference in means (e.g. p-val of 0.04 and difference in means of 20pts is more interesting than 0.04 and 5pts)
# each base/split combination will have rows equal to all possible combinations; e.g. age and gender would have 8 combinations (2*4)
# ranking factors can have up to 14*14=196 rows

# cut data into groups to be tested
ageTest<-data.frame(factor(as.numeric(data.explore$age)))
genderTest<-data.frame(data.explore$gender)
ageGenderTest<-cbind(age=ageTest,gender=genderTest)
colnames(ageGenderTest)<-c("age","gender")
ageGender1<-count(ageGenderTest,age,gender) #this is a better version of group_by and summarize(n=n())
# let's look at age==1 and compare gender==1 and gender==2
# 450 responses are age==1, gender==1; 596 responses are age==1, gender==2
genderAge1<-ageGender1[,c(2,1,3)]
genderAge1$n<-as.numeric(genderAge1$n)


# need to organize a 2x2 matrix with columns for hits and misses and rows for the experimental and control conditions
test<-
  matrix(c(450,596,420,837),
             nrow=2,
             dimnames = list(age1 = c("Female", "Male"),
                             age2 = c("Female", "Male")))
?fisher.test
# https://en.wikipedia.org/wiki/Hypergeometric_distribution
# uses a hypergeometric distribution
# This test has a wide range of applications. For example, a marketing group 
# could use the test to understand their customer base by testing a set of known 
# customers for over-representation of various demographic subgroups (e.g., women, people under 30).

TeaTasting <-
  matrix(c(3, 1, 1, 3),
         nrow = 2,
         dimnames = list(Guess = c("Milk", "Tea"),
                         Truth = c("Milk", "Tea")))


# test with fisher.test(df)[1] to get p-val
fisher.test(test)[1]
fisher.test(TeaTasting)

# store test in table


################################################################################
# Prototyping Generation of Summary Data (Unused)
################################################################################
# wrapping in a function to ignore
unused<- function() {
# http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html
# this function takes any two variables within data.explore and plots them without labels
# now that I have a version working with labels, I think I can delete, but I'm just going to hide for now
explore.old<-function(var1,var2) {

  # reshape the data
  condense.df<-tbl_df(cbind(data.explore[[var1]], data.explore[[var2]])) %>%
    group_by(V1,V2) %>%
    summarize(n=n()) %>%
    mutate(percent = n/sum(n)*100)
  colnames(condense.df)<-c(colnames(data.explore[var1]),colnames(data.explore[var2]),"n","percent")
  condense.df<-data.frame(condense.df)
  condense.df[,1]<-factor(condense.df[,1])
  condense.df[,2]<-factor(condense.df[,2])
  # plot
  ggplot(condense.df,aes(x=condense.df[[var1]], y= n, fill=condense.df[[var2]])) +
    geom_col() +
    geom_text(aes(label = paste0(round(percent,0),"%")), 
              position = position_stack(vjust = 0.5)) +
    ylab("Frequency") + 
    xlab(colnames(condense.df[1])) +
    scale_fill_discrete(name=colnames(condense.df[2])) +
    ggtitle(paste("Histogram of",colnames(condense.df[1]),"cut by",
                  colnames(condense.df[2]), sep=" "))
}

# same as above, but it accepts numbers for column references; useful for doing a full compare
# I'm going to hide this early version too; not sure if best practice is to delete since I have versions
# My version control needs better documentation in general... perhaps a section at the top describing changes?
explore.num.old<-function(num1,num2) {
  var1<-colnames(data.explore[num1])
  var2<-colnames(data.explore[num2])
  condense.df<-tbl_df(cbind(data.explore[[var1]], data.explore[[var2]])) %>%
    group_by(V1,V2) %>%
    summarize(n=n()) %>%
    mutate(percent = n/sum(n)*100)
  colnames(condense.df)<-c(colnames(data.explore[var1]),colnames(data.explore[var2]),"n","percent")
  condense.df<-data.frame(condense.df)
  condense.df[,1]<-factor(condense.df[,1])
  condense.df[,2]<-factor(condense.df[,2])

  # plot
  ggplot(condense.df,aes(x=condense.df[[var1]], y= n, fill=condense.df[[var2]])) +
    geom_col() +
    geom_text(aes(label = paste0(round(percent,0),"%")), 
              position = position_stack(vjust = 0.5)) +
    ylab("Frequency") + 
    xlab(colnames(condense.df[1])) +
    scale_fill_discrete(name=colnames(condense.df[2])) +
    ggtitle(paste("Histogram of",colnames(condense.df[1]),"cut by",
                  colnames(condense.df[2]), sep=" "))
}

# http://stackoverflow.com/questions/13273611/how-to-append-a-plot-to-an-existing-pdf-file
}





################################################################################
# Normalizing intensity questions
################################################################################
# not sure why this didn't occur to me, but instead of re-bucketing my intensity questions, what I should do is just normalize them to fit into 0-1
int.col<-data.boot[,c(high.low.qs,never.always.qs,rank.qs)]
int.names<-colnames(int.col)
int.norm<-data.frame(0)

# loop through each column and divide it by the number of buckets
for(i in 1:length(colnames(int.col))) {
  int.col[i]<-int.col[i]/length(table(int.col[i]))
}
int.norm<-int.col
colnames(int.norm)<-int.names

# append to the rest of the data
'%nin%' <- Negate('%in%')
nint.col<-which(colnames(data.boot) %nin% c(high.low.qs,never.always.qs,rank.qs))
data.clean<-cbind(data.boot[,nint.col],int.norm)



################################################################################
# Sampling Women (Unused)
################################################################################
.f2<- function() {
# In order to make our data representative, we need to transform it to the known rate of cannabis use by age/gender
# We don't have a baseline to use strata other than age/gender since we have no baseline for variables such as HHI when we cut by cannabis use
# The results from my analysis show that the general breakdown of age aligns with Headset, but not gender
# Methodology: I have a table of the target sample size to reduce female respondents.  The plan is to sample our age/gender buckets to hit these targets
library(dplyr)
female<-data.clean %>% filter(gender==1)
male<-data.clean %>% filter(gender==2)
# break into age groups
f1<-subset(female, age==2)
f2<-subset(female, age==3)
f3<-subset(female, age==4)
f4<-subset(female, age==5)
# sample
set.seed(1337)
f1.sample<-sample_n(f1,118)
f2.sample<-sample_n(f2,147)
f3.sample<-sample_n(f3,67)
f4.sample<-sample_n(f4,38)
female.sample<-rbind(f1.sample,f2.sample,f3.sample,f4.sample)

data.sampled<-rbind(male,female.sample)
# find the sample with replacement package and bump up men instead of sampling women
}
################################################################################
# Clustering
################################################################################
# http://dpmartin42.github.io/blogposts/r/cluster-mixed-types
skip<-function(x) {

set.seed(1680) # for reproducibility
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot

# Cleaning to remove ID, state, and cannabis use from the clustering (may do state-specific clustering later)

data.clust<-data.clean[,-c(1,4,5)]
rownames(data.clust)<-NULL

# also small cleaning to adjust age to be 1-4 instead of 2-5 (1 isn't used because we didn't have the youngest age group in our analysis)
data.clust$age<-factor(as.numeric(data.clust$age))
# and converting columns 16:60 to numeric and back to factor to eliminate 0's to line up with lookup
data.clust[16:60]<-lapply(data.clust[16:60],function(x) factor(as.numeric(x)))


# calculate gower distances between factors and numeric data and create a matrix of distances
gower.dist<-daisy(data.clust,
                  metric = "gower",
                  type = list(logratio=3))
# summary(gower.dist)
gower.mat<-as.matrix(gower.dist)

# output most similar pair
#data.clust[which(gower.mat==min(gower.mat[gower.mat != min(gower.mat)]),
#                 arr.ind=TRUE)[1,],]

# output most dissimilar pair
#data.clust[which(gower.mat==max(gower.mat[gower.mat != max(gower.mat)]),
#                 arr.ind=TRUE)[1,],]

# select the number of clusters using silhouette width
#sil_width <- c(NA)

#for(i in 2:10){
  
#  pam_fit <- pam(gower.dist,
#                 diss = TRUE,
#                 k = i)
#  
#  sil_width[i] <- pam_fit$silinfo$avg.width
#  
#}

# Plot sihouette width (higher is better)

#plot(1:10, sil_width,
#     xlab = "Number of clusters",
#     ylab = "Silhouette Width")
#lines(1:10, sil_width)
# shows we need 3 clusters

# Data Model, testing for 3-8 clusters to see if I like the results
#pam.fit3<-pam(gower.dist, diss = TRUE, k=3)
#pam.fit4<-pam(gower.dist, diss = TRUE, k=4)
#pam.fit5<-pam(gower.dist, diss = TRUE, k=5)
#pam.fit6<-pam(gower.dist, diss = TRUE, k=6)
#pam.fit7<-pam(gower.dist, diss = TRUE, k=7)
#pam.fit8<-pam(gower.dist, diss = TRUE, k=8)
#pam.fit9<-pam(gower.dist, diss = TRUE, k=9)

#see each cluster's medoid with:
#medoids3<-data.clust[pam.fit3$medoids,]
#medoids4<-data.clust[pam.fit4$medoids,]
#medoids5<-data.clust[pam.fit5$medoids,]
#medoids6<-data.clust[pam.fit6$medoids,]
#medoids7<-data.clust[pam.fit7$medoids,]
#medoids8<-data.clust[pam.fit8$medoids,]
#medoids9<-data.clust[pam.fit9$medoids,]

#write to show others
#write.csv(medoids3, "3_cluster_medoids.csv")
#write.csv(medoids4, "4_cluster_medoids.csv")
#write.csv(medoids5, "5_cluster_medoids.csv")
#write.csv(medoids6, "6_cluster_medoids.csv")
#write.csv(medoids7, "7_cluster_medoids.csv")
#write.csv(medoids8, "8_cluster_medoids.csv")
#write.csv(medoids9, "9_cluster_medoids.csv")

#combined version

#comb.medoids<-rbind(medoids3,medoids4,medoids5,medoids6,medoids7,medoids8,medoids9)
#write.csv(comb.medoids,"combMedoids.csv")

## appending cluster # to data.clust
#data.clust$cluster3<-pam.fit3$clustering
#data.clust$cluster4<-pam.fit4$clustering
#data.clust$cluster5<-pam.fit5$clustering
#data.clust$cluster6<-pam.fit6$clustering
#data.clust$cluster7<-pam.fit7$clustering
#data.clust$cluster8<-pam.fit8$clustering
#data.clust$cluster9<-pam.fit9$clustering


# generating PAM results for full summaries
#pam.results<-data.clust %>%
#  mutate(cluster = pam.fit9$clustering) %>% #I selected k=9 here
#  group_by(cluster) %>%
#  do(the_summary = summary(.))

#summary stats for each cluster
#pam.results$the_summary

# replace values with labels for the factors
# convert every value in factor columns (1:60) to the colname and factor pasted together for the lookup

#comb.medoids[1:60]<-lapply(comb.medoids[1:60],as.character)
#for(i in 1:60){
#  for(j in 1:dim(comb.medoids)[1]){
#    comb.medoids[j,i]<-paste0(colnames(comb.medoids[i]),comb.medoids[j,i])
#  }
#}
# read in lookup
medoid.lookup<-read.csv("survey_value_lookupMedoids1.csv",header=T, colClasses=rep("character",4))

# perform replacement
for(i in 1:60){
  comb.medoids[i]<-medoid.lookup$replacement[match(unlist(comb.medoids[i]),medoid.lookup$colval)]
}
#write.csv(comb.medoids,"comb_medoids.csv")

# plotting
#tsne.obj <- Rtsne(gower.dist, is_distance = TRUE)

#tsne.data <- tsne.obj$Y %>%
#  data.frame() %>%
#  setNames(c("X", "Y")) %>%
#  mutate(cluster = factor(pam.fit$clustering), #need to specify k (e.g. pam.fit9)
#         name = data.clean$ID)

#ggplot(aes(x = X, y = Y), data = tsne.data) +
#  geom_point(aes(color = cluster))

}

################################################################################
# Visualizing Clusters
################################################################################
# after selecting 7 clusters, we can use the following instead of picking the code from the above that I need:
set.seed(1680) # for reproducibility
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
data.clust<-data.clean[,-c(1,4,5)]
rownames(data.clust)<-NULL
data.clust$age<-factor(as.numeric(data.clust$age))
data.clust[16:60]<-lapply(data.clust[16:60],function(x) factor(as.numeric(x)))
gower.dist<-daisy(data.clust,metric = "gower",type = list(logratio=3))
pam.fit7<-pam(gower.dist, diss = TRUE, k=7)
data.clust$cluster<-pam.fit7$clustering
data.results<-data.clust
data.results<-data.results[,c(113,1:112)] #put cluster first
lookup<-read.csv("survey_value_lookup5.csv", header = T)
##### visualization
# wait a minute, can't i just run var.explore on this with cluster set as the x-var?? brilliant..
library(gtable)
explore.clust<-function(var1,var2) {
  # condense the data for ggplot
  condense.df<-tbl_df(cbind(data.results[[var1]], data.results[[var2]])) %>%
    group_by(V1,V2) %>%
    summarize(n=n()) %>%
    mutate(percent = n/sum(n)*100)
  colnames(condense.df)<-c(colnames(data.results[var1]),colnames(data.results[var2]),"n","percent")
  condense.df<-data.frame(condense.df)
  
  # perform replacement on condense.df so that when it's read into the plot, it has the right labels
  for(i in 1:dim(condense.df)[1]) {
    #condense.df[i,1]<-paste(colnames(condense.df[1]),condense.df[i,1],sep="")
    condense.df[i,2]<-paste(colnames(condense.df[2]),condense.df[i,2],sep="")
  }
  #condense.df[1]<-lookup$replacement[match(unlist(condense.df[1]),lookup$colval)]
  condense.df[2]<-lookup$replacement[match(unlist(condense.df[2]),lookup$colval)]
  
  condense.df2<-tbl_df(condense.df) %>%
    group_by(condense.df[,1]) %>%
    summarize(n=sum(n)) %>%
    mutate(percent=n/sum(n)*100)
  colnames(condense.df2)<-c(colnames(condense.df[1]),"n","percent")
  condense.df2<-data.frame(condense.df2)
  condense.df2[,1]<-factor(condense.df2[,1])
  
  # plot
  p1<-ggplot(condense.df2,aes(x=condense.df2[[var1]],y=n)) +
    geom_bar(stat="identity",aes(fill = condense.df2[[var1]])) +
    ylab("Responses") + 
    ggtitle(paste(colnames(condense.df2[1]),"cut by",
                  colnames(condense.df2[2]), sep=" "))+
    theme(axis.text.x =element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank()) +
    geom_text(aes(label=n), vjust=1.6, color="white")+
    scale_fill_discrete(name=colnames(condense.df2[1])) +
    geom_text(aes(label=paste0(round(percent,0),"%")), vjust=-0.3) 
  
  
  p2<-ggplot(condense.df, aes(x=condense.df[[var1]], weight = n, fill=condense.df[[var2]], y=percent/100)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = paste0(round(percent,0),"%"),y = percent/100), position =position_stack(vjust = 0.5), color = "white") + 
    scale_y_continuous(labels=percent) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    ylab("Frequency") + 
    xlab(colnames(condense.df[1])) +
    scale_fill_discrete(name=colnames(condense.df[2])) 
  
  g1<-ggplotGrob(p1)
  g2<-ggplotGrob(p2)
  g<-gtable:::rbind_gtable(g1, g2, "first")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels] <- unit(c(1,1), "null")
  grid.newpage()
  grid.draw(g)
  
}

# same as explore, but accepts numeric references to columns in data.results

explore.num.clust<-function(num1,num2) {
  var1<-colnames(data.results[num1])
  var2<-colnames(data.results[num2])
  # condense the data for ggplot
  condense.df<-tbl_df(cbind(data.results[[var1]], data.results[[var2]])) %>%
    group_by(V1,V2) %>%
    summarize(n=n()) %>%
    mutate(percent = n/sum(n)*100)
  colnames(condense.df)<-c(colnames(data.results[var1]),colnames(data.results[var2]),"n","percent")
  condense.df<-data.frame(condense.df)
  
  # perform replacement on condense.df so that when it's read into the plot, it has the right labels
  for(i in 1:dim(condense.df)[1]) {
    condense.df[i,1]<-paste(colnames(condense.df[1]),condense.df[i,1],sep="")
    condense.df[i,2]<-paste(colnames(condense.df[2]),condense.df[i,2],sep="")
  }
  condense.df[1]<-lookup$replacement[match(unlist(condense.df[1]),lookup$colval)]
  condense.df[2]<-lookup$replacement[match(unlist(condense.df[2]),lookup$colval)]
  
  condense.df2<-tbl_df(condense.df) %>%
    group_by(condense.df[,1]) %>%
    summarize(n=sum(n)) %>%
    mutate(percent=n/sum(n)*100)
  colnames(condense.df2)<-c(colnames(condense.df[1]),"n","percent")
  condense.df2<-data.frame(condense.df2)
  condense.df2[,1]<-factor(condense.df2[,1])
  
  # plot
  p1<-ggplot(condense.df2,aes(x=condense.df2[[var1]],y=n)) +
    geom_bar(stat="identity",aes(fill = condense.df2[[var1]])) +
    ylab("Responses") + 
    ggtitle(paste(colnames(condense.df2[1]),"cut by",
                  colnames(condense.df2[2]), sep=" "))+
    theme(axis.text.x =element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank()) +
    geom_text(aes(label=n), vjust=1.6, color="white")+
    scale_fill_discrete(name=colnames(condense.df2[1])) +
    geom_text(aes(label=paste0(round(percent,0),"%")), vjust=-0.3) 
  
  
  p2<-ggplot(condense.df, aes(x=condense.df[[var1]], weight = n, fill=condense.df[[var2]], y=percent/100)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = paste0(round(percent,0),"%"),y = percent/100), position =position_stack(vjust = 0.5), color = "white") + 
    scale_y_continuous(labels=percent) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    ylab("Frequency") + 
    xlab(colnames(condense.df[1])) +
    scale_fill_discrete(name=colnames(condense.df[2])) 

  
  g1<-ggplotGrob(p1)
  g2<-ggplotGrob(p2)
  g<-gtable:::rbind_gtable(g1, g2, "first")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels] <- unit(c(1,1), "null")
  grid.newpage()
  grid.draw(g)
  
}


# function for boxplots
explore.num.clust.box<-function(num1,num2) {
  var1<-colnames(data.results[num1])
  var2<-colnames(data.results[num2])
 
  expand.df<-cbind(cluster=factor(data.results[,num1]),data.results[num2])
 
  p3<-ggplot(expand.df, aes(x=expand.df$cluster, y=expand.df[,2])) +
    geom_boxplot() +
    scale_x_discrete(name = colnames(expand.df[1])) +
    scale_y_continuous(name = colnames(expand.df[2])) +
    ggtitle(paste(colnames(expand.df[2]),"boxplots for each cluster",sep=" "))
  
  print(p3)
  
}

#function to print boxplots into a pdf
var.explore.clust.box<-function(x){
  name<-paste0(sprintf("%03d",x),"_",colnames(data.results[x]))
  num.plots<-length(colnames(data.results))
  my.plots<-vector(num.plots, mode='list')
  for(i in 62:113) { 
    print(explore.num.clust.box(x,i))
    my.plots[[i]]<-recordPlot()
  }
  
  graphics.off()
  
  pdf(paste0(name,".pdf"),onefile=TRUE)
  for (my.plot in my.plots) {
    replayPlot(my.plot)
  }
  graphics.off()
}




# function which accepts a number for a column reference in data.results and cuts
#   that variable by every other variable.  Next, the generated plots are converted
#   to a .pdf with 1 page per plot
var.explore.clust<-function(x){
  name<-paste0(sprintf("%03d",x),"_",colnames(data.results[x]))
  num.plots<-length(colnames(data.results))
  my.plots<-vector(num.plots, mode='list')
  for(i in 1:61) { # 62+ are numeric and need to be plotted separately
    #print was required to make it work, but I think this is slowing it down
    print(explore.num.clust(x,i))
    my.plots[[i]]<-recordPlot()
  }
  
  graphics.off()
  
  pdf(paste0(name,".pdf"),onefile=TRUE)
  for (my.plot in my.plots) {
    replayPlot(my.plot)
  }
  graphics.off()
}


# function to plot the numeric data in a boplot format

################################################################################
# Append cluster to data.explore for visualization
################################################################################
data.explore$cluster<-data.clust$cluster
data.explore<-data.explore[,c(116,115,1:114)]
