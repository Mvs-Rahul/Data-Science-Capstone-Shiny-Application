#
# server.R
#
# Capstone App created using Shiny and R Studio 
#
# 
# Mvs Rahul
# 
# 15th November,2022
#

# Load libraries
library(shiny)
library(ggplot2)
library(reshape2)
library(tm)
library(slam)
library(data.table)
library(RWeka)
library(dtplyr)

# Comment out the pre-processing of text when running in Shiny
preprocessing <- FALSE
if(preprocessing) 
{
  
  setwd( "C:/Users/user/OneDrive/Documents/King")
  # Load the raw blogs, news, and twitter text data from Coursera
  con <- file("en_US.blogs.txt",open="r" )
  Blogs <- readLines(con)
  close(con)
  con <- file("en_US.news.txt",open="r" )
  News <- readLines(con) 
  close(con)
  con <- file("en_US.twitter.txt",open="r" )
  Twitter <- readLines(con) 
  close(con)
  
  # Sample from the data and concatentate into one variable
  set.seed(123)
  sampleBlogs <- sample(Blogs, 10000, replace=FALSE)
  sampleNews <- sample(News, 10000, replace=FALSE)
  sampleTwitter <- sample(Twitter, 10000, replace=FALSE)
  sampleAll <- c(sampleBlogs,sampleNews,sampleTwitter)
  
  # Define function to process the corpus through transformation
  getCorpus <- function(v) 
  {
    corpus <- VCorpus(VectorSource(v))
    corpus <- tm_map(corpus, stripWhitespace)  # remove whitespace
    corpus <- tm_map(corpus, content_transformer(tolower))  # lowercase all
    corpus <- tm_map(corpus, removePunctuation) # remove punctuation
    corpus <- tm_map(corpus, removeNumbers) # remove number
    corpus 
  }
  
  # Apply function to process the sample
  aCorp <- getCorpus(sampleAll)
  
  # Define function to tokenize the corpus into N-grams
  UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
  QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
  
  # Create term document matrices of 2, 3, and 4 N-grams
  aTdm_2 <- TermDocumentMatrix(aCorp, control = list(tokenize = BigramTokenizer)) 
  aTdm_3 <- TermDocumentMatrix(aCorp, control = list(tokenize = TrigramTokenizer))
  aTdm_4 <- TermDocumentMatrix(aCorp, control = list(tokenize = QuadgramTokenizer))
  
  # Define function to convert the term document matrices into frequencies
  tdmToFreq <- function(tdm) 
  {
    freq <- sort(row_sums(tdm, na.rm=TRUE), decreasing=TRUE)
    word <- names(freq)
    data.table(word=word, freq=freq)
  }
  
  # Define function to process the N-grams to create two columns for the pre-term and current-term
  processGram <- function(dt) 
  {
    dt[, c("pre", "cur"):=list(unlist(strsplit(word, "[ ]+?[a-z]+$")), 
                               unlist(strsplit(word, "^([a-z]+[ ])+"))[2]), 
       by=word]
  }
  
  # Convert term document matrices into frequencies and process the N-grams
  aFreq_2 <- tdmToFreq(aTdm_2)
  processGram(aFreq_2)
  aFreq_3 <- tdmToFreq(aTdm_3)
  processGram(aFreq_3)
  aFreq_4 <- tdmToFreq(aTdm_4)
  processGram(aFreq_4)
  
  # Save the N-gram frequencies to disk
  save(aFreq_2, file = "aFreq_2.RData")
  save(aFreq_3, file = "aFreq_3.RData")
  save(aFreq_4, file = "aFreq_4.RData")
}


# Load more libraries
library(e1071)
library(dplyr)
library(magrittr)
library(stringr)

# Define my function to take raw text input and return the predicted next word
Papazian <- function(raw) 
{
  sentence <- tolower(raw) %>%
    removePunctuation %>%
    removeNumbers %>%
    stripWhitespace %>%
    str_trim %>%
    strsplit(split=" ") %>%
    unlist
  
  length(sentence)
  
  if(length(sentence)>=3)
  {
    gram <- paste(tail(sentence, 3), collapse=" ")
    myattempt4 <- aFreq_4[ which(pre==gram),]$cur[1]
    
    if(is.na(myattempt4))
    {
      gram <- paste(tail(sentence, 2), collapse=" ")
      myattempt3 <- aFreq_3[ which(pre==gram),]$cur[1]
      
      if(is.na(myattempt3))
      {
        gram <- paste(tail(sentence, 1), collapse=" ")
        myattempt2 <- aFreq_2[ which(pre==gram),]$cur[1]
        
        if(is.na(myattempt2))
        {
          return("sorry")
        }
        else
        {
          return(myattempt2)
        }
      }
      else
      {
        return(myattempt3)
      }
    }
    else
    {
      return(myattempt4)
    }
  }
  
  if(length(sentence)==2)
  {
    gram <- paste(tail(sentence, 2), collapse=" ")
    myattempt3 <- aFreq_3[ which(pre==gram),]$cur[1]
    
    if(is.na(myattempt3))
    {
      gram <- paste(tail(sentence, 1), collapse=" ")
      myattempt2 <- aFreq_2[ which(pre==gram),]$cur[1]
      
      if(is.na(myattempt2))
      { 
        return("sorry")
      }
      else
      {
        return(myattempt2)
      }
    }
    else
    {
      return(myattempt3)
    }
  }
  
  if(length(sentence)==1)
  {
    gram <- paste(tail(sentence, 1), collapse=" ")
    myattempt2 <- aFreq_2[ which(pre==gram),]$cur[1]
    
    if(is.na(myattempt2))
    { 
      return("sorry")
    }
    else
    {
      return(myattempt2)
    }
  }
  
  else
  {
    return("NIL")
  }
  
}


# Define the Shiny Server
shinyServer(function(input, output)
{
  
  observe({
    
    output$textOut <- renderText(
      { 
        paste("", Papazian(input$textIn))
      })
    
  })
  
})
