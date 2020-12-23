#---
#  title: "DSFinalCapstone ngram"
#author: "RS"
#date: "16 12 2020"
#output: R Function
#---
  
library(tidyverse)
library(stringr)

#  Import the training data`
(WD <- getwd())
if (!is.null(WD)) setwd(WD)
#setwd("C:/Users/Ronnz/Documents/datasciencecoursera/Capstone")
myBiWords <- readRDS(".myBigramWords.rds")
myTriWords  <- readRDS(".myTrigramWords.rds")
myQuadWords <- readRDS(".myQuadgramWords.rds")
myQuintWords <- readRDS(".myQuintgramWords.rds")


# Create the ngram matching functions
bigram <- function(input_words){
  num <- length(input_words)
  filter(myBiWords, 
         word1==input_words[num]) %>% 
      filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
  num <- length(input_words)
  filter(myTriWords, 
         word1==input_words[num-1], 
         word2==input_words[num])  %>% 
     filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
  num <- length(input_words)
  filter(myQuadWords, 
         word1==input_words[num-2], 
         word2==input_words[num-1], 
         word3==input_words[num])  %>% 
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> out
  ifelse(out=="character(0)", trigram(input_words), return(out))
}

quintgram <- function(input_words){
  num <- length(input_words)
  filter(myQuintWords, 
         word1==input_words[num-3], 
         word2==input_words[num-2], 
         word3==input_words[num-1], 
         word4==input_words[num])  %>% 
    filter(row_number() == 1L) %>%
    select(num_range("word", 5)) %>%
    as.character() -> out
  ifelse(out=="character(0)", quintgram(input_words), return(out))
}

# Matching function
ngrams <- function(input){
  # Create dataframe
  input <- data_frame(text = input)
  # Clean inpput
  replace_reg <- "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  # Find word counts, separate words, convert to lower case
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  # Call matching functions
  #out <- ifelse(input_count == 0, "Please input a phrase",
  #              ifelse(input_count == 3, quadgram(input_words),
  #                     ifelse(input_count == 2, trigram(input_words), bigram(input_words))))
  out <- ifelse(input_count == 0, "Please input a phrase",
           ifelse(input_count == 4, quintgram(input_words),
             ifelse(input_count == 3, quadgram(input_words),
               ifelse(input_count == 2, trigram(input_words), bigram(input_words)))))
  return(out)
}

