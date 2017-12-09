library(NLP)
library(tm)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(quanteda)

#source('E:\\RWorkingDirectory\\coursera_wordpred\\getNextWord.R') 


setwd("F:\\R\\capstone\\next_word\\")

load("bigram.RData") 
load("trigram.RData") 
load("quadgram.RData")

getNextWord <- function(phraseIn, wordsOut = 5, flag = TRUE) { 
  
  str<- "NA"
  lastWord <- NA 
  #print("firstin")
  in_token <- tokens(phraseIn)
  
  #print(in_token$text1)
  
  numWords <- length(in_token$text1)
  #print(numWords)
  phraseIn <- gsub(",", " ", phraseIn)
  phraseIn <- gsub("  ", " ", phraseIn)
  #print(numWords)
  
  # If user input is blank space or is filtered out, return common unigram 
  # if (numWords==0){ 
  #     lastWord <- as.data.frame(as.character(data1gram$word[1:wordsOut])) 
  #     return(lastWord) 
  #   } 
  
  if (numWords > 3) { 
    startp <- numWords-3
    newStr <- in_token$text1[startp+1:numWords]
    
    newStr1 <- newStr[1:3]
    phraseIn <- toString(newStr1)
    phraseIn <- gsub(",", " ", phraseIn)
    
    in_token <- tokens(phraseIn)
    #print(in_token)
    numWords <- length(in_token$text1)
    #print(numWords)
  }
  
  # searching 3 words in quadgram
  if (numWords == 3) { 
    
    #print("in quad")
    #print(phraseIn)
    phraseIn <- gsub("  ", " ", phraseIn)
    
    pos<-grep(phraseIn,quadgram[, 1],ignore.case = TRUE) 
    
    #print(pos)
    length(pos)
    if (length(pos) > 0) {
      
      str<- as.data.frame(quadgram[pos, 2])
      str$freq<-quadgram[pos, 3]
      
      #result<-cbind("text"=str,"freq"=1)
      result<-str
      #print("quad run")
      
    }
    else {  newStr1 <- in_token$text1[2:3]
    phraseIn <- toString(newStr1)
    phraseIn <- gsub(",", " ", phraseIn)
    phraseIn <- gsub("  ", " ", phraseIn)
    
    in_token <- tokens(phraseIn)
    #print(in_token)
    numWords <- length(in_token$text1)
    #print(numWords) 
    
    }
  }
  # searching 2 words in trigram
  if (numWords == 2) { 
    
    print("numWords=2")
    phraseIn <- gsub("  ", " ", phraseIn)
    print(phraseIn)
    pos<-grep(phraseIn,trigram[, 1],ignore.case = TRUE) 
    print(pos)
    
    length(pos)
    if (length(pos) > 0) {
      
      str<- as.data.frame(trigram[pos, 2])
      str$freq<-trigram[pos, 3]
      
      #result<-cbind("text"=str,"freq"=1)
      result<-str
      #print("tri run")
    }
    else { 
      
      newStr1 <- in_token$text1[2:2]
      
      #newStr1 <- newStr[1:3]
      phraseIn <- toString(newStr1)
      phraseIn <- gsub(",", " ", phraseIn)
      
      in_token <- tokens(phraseIn)
      #print(in_token)
      numWords <- length(in_token$text1)
      #print(numWords)  
    }
  }
  
  # searching 1 words in bigram
  if (numWords == 1) {
    #print("bi run")
    #print(phraseIn)
    pos<-grep(phraseIn,bigram[, 1],ignore.case = TRUE) 
    
    length(pos)
    if (length(pos) > 0) {
      
      str<- as.data.frame(bigram[pos, 2])
      str$freq<-bigram[pos, 3]
      #result<-cbind("text"=str,"freq"=1)
      result<-str
      
    } 
  }
  if (!exists("result")) {
    phraseIn_new<-substr(phraseIn,regexpr(" ", phraseIn)+1,nchar(phraseIn))
    if(phraseIn_new != phraseIn) {getNextWord(phraseIn_new)}
    else return("No Results")
  }
  else {
    result<-str
    colnames(result)<-c("nextword","freq")
    result<-head(result,5)
    
    return(result)
    
  }
}

getNextWord("i am happy")
