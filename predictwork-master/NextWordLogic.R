# prepare_data.R ####
# Coursera Data Science Capstone Project (https://www.coursera.org/course/dsscapstone)
# Prepare data for downstream analysis and models
# 2015-12-20

# Libraries and options ####
rm(list=ls(all=TRUE))
library(dplyr)
library(quanteda)
# library(wordcloud)
# library(RColorBrewer)
library(stringr)
library(tm)
library(readr)
library(caTools)
library(tidyr)
setwd ("F:/Shared/Drive/AnjaliS/Coursera/Course10Capstone/NextWordApp/NextWord")
# setwd ("F:/Shared/Drive/AnjaliS/Coursera/Course10Capstone")
source("./PredictWordAlgo.R")
# source("./NextWordApp/NextWord/PredictWordAlgo.R")


# Read and prepare data ####

# Read in data

dBlog <- readLines("Coursera-SwiftKey/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
dNews <- readLines("Coursera-SwiftKey/final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
dTwitter <- readLines("Coursera-SwiftKey/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)


saveRDS(dBlog,"dBlog.RData")
saveRDS(dNews,"dNews.RData")
saveRDS(dTwitter,"dTwitter.RData")
rm(dBlog,dNews,dTwitter)

dBlog <- readRDS("dBlog.RData"); dNews <- readRDS("dNews.RData"); dTwitter <- readRDS("dTwitter.RData")


combinedRaw = c(dBlog, dNews, dTwitter)
# combinedRaw<-gsub("(ftp|http)(s?)://.*\\b", "", combinedRaw)
combinedRaw<-gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", combinedRaw)
# combinedRaw<-gsub("[^A-Za-z0-9,-]"," ",combinedRaw)
# # combinedRaw<-str_replace_all(combinedRaw, "[[:punct:]]", "")
# # combinedRaw<-gsub("RT |via", "", combinedRaw)
# combinedRaw<-gsub("[@][a - zA - Z0 - 9_]{1,15}", "", combinedRaw)

# combinedRaw <- removePunctuation(combinedRaw)
# combinedRaw <- tolower(combinedRaw)
# combinedRaw <- stemDocument(combinedRaw)
# combinedRaw <- removeWords(combinedRaw, words=c('the', stopwords("english")))
# combinedRaw<-removeNumbers(combinedRaw)
# combinedRaw <- stripWhitespace(combinedRaw)
saveRDS(combinedRaw,"combinedRaw.RData")
# combinedRaw<-readRDS("combinedRaw.RData")



# Sample and combine data  
set.seed(1220)
n = 1/10
combined = sample(combinedRaw, length(combinedRaw) * n)
rm(combinedRaw)
# Split into train and validation sets
split = sample.split(combined, 0.8)
train = subset(combined, split == T)
valid = subset(combined, split == F)

rm(combined)

# Transfer to quanteda corpus format and segment into sentences (prediction.R)
train = fun.corpus(train)
saveRDS(train,"train.RData")
train<-readRDS("train.RData")
# Tokenize (prediction.R)

ngram1 <- dfm(train, ngrams = 1, concatenator = " ",remove_punct =TRUE,tolower = FALSE,remove = stopwords("english"),remove_twitter = TRUE)

# Store freqent n-grams in data frames
df <- as.data.frame(as.matrix(docfreq(ngram1)))
names(df)[1] <- "Frequency"
df_sort <- sort(rowSums(df), decreasing = TRUE)
df_unigrams <- data.frame(Term = names(df_sort), Frequency = df_sort)
df_unigrams<-df_unigrams[df_unigrams$Frequency>100,]
saveRDS(df_unigrams,"df_unigrams.RData")
rm(df, df_sort,df_unigrams,ngram1)


ngram2 <- dfm(train, ngrams = 2, concatenator = " ",remove_punct =TRUE,tolower = FALSE,remove = stopwords("english"),remove_twitter = TRUE)
df <- as.data.frame(as.matrix(docfreq(ngram2)))
names(df)[1] <- "Frequency"
df_sort <- sort(rowSums(df), decreasing = TRUE)
df_bigrams <- data.frame(Term = names(df_sort), Frequency = df_sort)
df_bigrams<-df_bigrams %>% separate(Term, c('word1', 'Term'), " ")
df_bigrams<-df_bigrams[df_bigrams$Frequency>10,]
saveRDS(df_bigrams,"df_bigrams.RData")
saveRDS(ngram2,"ngram2.RData")
rm(df, df_sort,df_bigrams,ngram2)

ngram3 <- dfm(train, ngrams = 3, concatenator = " ",remove_punct =TRUE,tolower = FALSE,remove = stopwords("english"),remove_twitter = TRUE)
df <- as.data.frame(as.matrix(docfreq(ngram3)))
names(df)[1] <- "Frequency"
df_sort <- sort(rowSums(df), decreasing = TRUE)
df_trigrams <- data.frame(Term = names(df_sort), Frequency = df_sort)
df_trigrams<-df_trigrams %>% separate(Term, c('word1', 'word2','Term'), " ")
df_trigrams<-df_trigrams[df_trigrams$Frequency>5,]
saveRDS(df_trigrams,"df_trigrams.RData")
saveRDS(ngram3,"ngram3.RData")
rm(df, df_sort,df_trigrams,ngram3)


ngram4 <- dfm(train, ngrams = 4, concatenator = " ",remove_punct =TRUE,tolower = FALSE,remove = stopwords("english"),remove_twitter = TRUE)
df <- as.data.frame(as.matrix(docfreq(ngram4)))
names(df)[1] <- "Frequency"
df_sort <- sort(rowSums(df), decreasing = TRUE)
df_fourgrams <- data.frame(Term = names(df_sort), Frequency = df_sort)
df_fourgrams<-df_fourgrams %>% separate(Term, c('word1', 'word2','word3','Term'), " ")
df_fourgrams<-df_fourgrams[df_fourgrams$Frequency>5,]
saveRDS(df_fourgrams,"df_fourgrams.RData")
saveRDS(ngram4,"ngram4.RData")
rm(df, df_sort,df_fourgrams,ngram4)

ngram5 <- dfm(train, ngrams = 5, concatenator = " ",remove_punct =TRUE,tolower = FALSE,remove = stopwords("english"),remove_twitter = TRUE)
df <- as.data.frame(as.matrix(docfreq(ngram5)))
names(df)[1] <- "Frequency"
df_sort <- sort(rowSums(df), decreasing = TRUE)
df_fivegrams <- data.frame(Term = names(df_sort), Frequency = df_sort)
df_fivegrams<-df_fivegrams %>% separate(Term, c('word1', 'word2','word3','word4','Term'), " ")
df_fivegrams<-df_fivegrams[df_fivegrams$Frequency>2,]
saveRDS(df_fivegrams,"df_fivegrams.RData")
saveRDS(ngram5,"ngram5.RData")
rm(df, df_sort,df_fivegrams,ngram5)
rm(train)

df_unigrams<-readRDS("df_unigrams.RData");df_bigrams <- readRDS("df_bigrams.RData"); df_trigrams <- readRDS("df_trigrams.RData"); df_fourgrams <- readRDS("df_fourgrams.RData");df_fivegrams <- readRDS("df_fivegrams.RData");
