# Libraries and options ####
# library(dplyr)
# library(quanteda)
# library(wordcloud)
# library(RColorBrewer)
library(stringr)
library(tm)
 df_bigrams <- readRDS(file="./df_bigrams.RData");
df_trigrams <- readRDS(file="./df_trigrams.RData");
df_fourgrams <- readRDS(file="./df_fourgrams.RData");
df_fivegrams <- readRDS(file="./df_fivegrams.RData");
 # Transfer to quanteda corpus format and segment into sentences
fun.corpus = function(x) {
  corpus(unlist(char_segment(x,pattern = ".",pattern_position = "after")))
}
# LoadData<-function(){
#   df_bigrams <- readRDS(file="./df_bigrams.RData"); 
#   df_trigrams <- readRDS(file="./df_trigrams.RData"); 
#   df_fourgrams <- readRDS(file="./df_fourgrams.RData");
#   df_fivegrams <- readRDS(file="./df_fivegrams.RData");
#   
# }
## Function predicting the next word
predictWord <- function(the_word) {
  word_add <- stripWhitespace(removeNumbers(removePunctuation(tolower(the_word),preserve_intra_word_dashes = TRUE)))
  
  the_word <- strsplit(word_add, " ")[[1]]
  
  n <- length(the_word)
  
  
  
  if (n == 0 ) { as.character(head("it",1))}
  else if (n == 1) {the_word <- as.character(tail(the_word,1)); functionBigram(the_word)}
  else if (n == 2) {the_word <- as.character(tail(the_word,2)); functionTrigram(the_word)}
  else if (n == 3) {the_word <- as.character(tail(the_word,3)); functionQuadgram(the_word)}
  else if (n >= 4) {the_word <- as.character(tail(the_word,4)); functionFivegram(the_word)}
}
 functionBigram <- function(the_word) {
  # testing print(the_word)
  if (identical(character(0),as.character(head(df_bigrams[df_bigrams$word1 == the_word[1], 2], 1)))) {
    
    # message<<-"If no word found the most used pronoun 'it' in English will be returned"
    as.character(head("it",1))
  }
  else {
    # message <<- "Trying to Predict the Word using Bigram Freqeuncy Matrix  "
    as.character(head(df_bigrams[df_bigrams$word1 == the_word[1],2], 1))
    
  }
}
 functionTrigram <- function(the_word) {
 
  if (identical(character(0),as.character(head(df_trigrams[df_trigrams$word1 == the_word[1]
                                                  & df_trigrams$word2 == the_word[2], 3], 1)))) {
    as.character(predictWord(the_word[2]))
    
  }
  else {
    # message<<- "Trying to Predict the Word using Trigram Fruequency Matrix "
    as.character(head(df_trigrams[df_trigrams$word1 == the_word[1]
                         & df_trigrams$word2 == the_word[2], 3], 1))
    
  }
}
 functionQuadgram <- function(the_word) {
  
  if (identical(character(0),as.character(head(df_fourgrams[df_fourgrams$word1 == the_word[1]
                                                  & df_fourgrams$word2 == the_word[2]
                                                  & df_fourgrams$word3 == the_word[3], 4], 1)))) {
    
    as.character(predictWord(paste(the_word[2],the_word[3],sep=" ")))
  }
  else {
    # message <<- "Trying to Predict the Word using Quadgram Frequency Matrix"
    as.character(head(df_fourgrams[df_fourgrams$word1 == the_word[1]
                         & df_fourgrams$word2 == the_word[2]
                         & df_fourgrams$word3 == the_word[3], 4], 1))
  
  }
}
functionFivegram <- function(the_word) {
  # testing print(the_word)
  if (identical(character(0),as.character(head(df_fivegrams[df_fivegrams$word1 == the_word[1]
                                                            & df_fivegrams$word2 == the_word[2]
                                                            & df_fivegrams$word3 == the_word[3]
                                                            & df_fivegrams$word4 == the_word[4], 5], 1)))) {
    
    as.character(predictWord(paste(the_word[2],the_word[3],the_word[4],sep=" ")))
  }
  else {
    # message <<- "Trying to Predict the Word using Quadgram Frequency Matrix"
    as.character(head(df_fivegrams[df_fivegrams$word1 == the_word[1]
                                   & df_fivegrams$word2 == the_word[2]
                                   & df_fivegrams$word3 == the_word[3]
                                   & df_fivegrams$word4 == the_word[4], 5], 1))
    
  }
}
 cleanText<-function(text){
  cleanText <- tolower(text)
  cleanText <- removePunctuation(cleanText)
  cleanText <- removeNumbers(cleanText)
  cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
  cleanText <- stripWhitespace(cleanText)
  return(cleanText)
}
