#install.packages("tm",dependencies = TRUE)

#install.packages("wordcloud", dependencies = TRUE)

library(NLP)
library(tm)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(quanteda)

#install.packages("quanteda",dependencies = TRUE)

setwd("F:\\R\\capstone\\next_word\\")

getwd()

x<-file("en_US.blogs.txt","r")

eBlg_s <- readLines(x, 3000)

length(eBlg_s)

close(x)

#vc <- Corpus(VectorSource(eBlg_s))

x<-file("en_US.news.txt","r")

eNews_s <- readLines(x, 3000)

length(eNews_s)

close(x)

x<-file("en_US.twitter.txt","r")

eTwit_s <- readLines(x, 3000)

length(eTwit_s)

close(x)

efeed_s <- c(eBlg_s,eNews_s,eTwit_s)

efeed_s <- removePunctuation(efeed_s)
efeed_s <- iconv(efeed_s, "latin1", "ASCII", sub="")


vc <- Corpus(VectorSource(efeed_s))

length(vc)

evc = tm_map(vc, tolower)
evc = tm_map(evc, removePunctuation)
evc = tm_map(evc, removeNumbers)
evc = tm_map(evc, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(evc)

dtm_matrix <- as.matrix(dtm)

#wordcount <- colSums(dtm_matrix)

#topten <- head(sort(wordcount, decreasing=TRUE), 10)

#topfif <- head(sort(wordcount, decreasing=TRUE), 50)



#dfplot <- as.data.frame(melt(topten))
#dfplot$word <- dimnames(dfplot)[[1]]
#dfplot$word <- factor(dfplot$word,
 #                     levels=dfplot$word[order(dfplot$value,
  #                                             decreasing=TRUE)])

#fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
#fig <- fig + xlab("Word in Corpus")
#fig <- fig + ylab("Count")
#print(fig)

#names(topten)

#wordcloud(evc, max.words = 20, random.order = FALSE, colors = brewer.pal(8,"Dark2"))

class(efeed_s)

unigram <- tokens(efeed_s,ngrams = 1L)
head(unigram)

unigram <- dfm((unigram),remove_punct = TRUE)
unigram <- dfm_remove(dfm(unigram_clean), stopwords("english"))
save(unigram, file="unigram.RData")

bigram <- tokens(efeed_s,ngrams = 2L, concatenator = " ")
bigram <- dfm((bigram),remove_punct = TRUE)
bigram <- dfm_remove(dfm(bigram), stopwords("english"))
bi_top <- topfeatures(bigram, n = 3000, decreasing = TRUE, scheme = c("count", "docfreq"), groups = NULL)
bigram <- as.data.frame(bi_top)

bigram$word <- dimnames(bigram)[[1]]
head(bigram$word)
temp <- strsplit(bigram$word, " ")
mat  <- matrix(unlist(temp), ncol=2, byrow=TRUE)
df   <- as.data.frame(mat)
df$bi_top   <-  bi_top
colnames(df) <- c("start", "end", "freq")
head(df)
bigram<-df
save(bigram, file="bigram.RData")


trigram <- tokens(efeed_s,ngrams = 3L, concatenator = " ")
trigram <- dfm((trigram),remove_punct = TRUE)
trigram <- dfm_remove(dfm(trigram), stopwords("english"))
tri_top <- topfeatures(trigram, n = 3000, decreasing = TRUE, scheme = c("count", "docfreq"), groups = NULL)
trigram <- as.data.frame(tri_top)

trigram$word <- dimnames(trigram)[[1]]
head(trigram$word)
temp <- strsplit(trigram$word, " ")
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)
df   <- as.data.frame(mat)

df$start <- paste(df$V1, df$V2, sep=" ")
df$tri_top <- tri_top

df   <- cbind(df$start,df$v3,tri_top)
colnames(df) <- c("word_1", "word_2","end","start", "freq")
head(df[,c(4,3,5)])
df1 <- (df[,c(4,3,5)])
colnames(df1)<-c("start","end","freq")
head(df1)
trigram <- df1
save(trigram, file="trigram.RData")

quadgram <- tokens(efeed_s,ngrams = 4L, concatenator = " ")
quadgram <- dfm((quadgram),remove_punct = TRUE)
quadgram <- dfm_remove(dfm(quadgram), stopwords("english"))
quad_top <- topfeatures(quadgram, n = 3000, decreasing = TRUE, scheme = c("count", "docfreq"), groups = NULL)
quadgram <- as.data.frame(quad_top)

quadgram$word <- dimnames(quadgram)[[1]]
head(quadgram$word)
temp <- strsplit(quadgram$word, " ")
mat  <- matrix(unlist(temp), ncol=4, byrow=TRUE)
df   <- as.data.frame(mat)

df$start <- paste(df$V1, df$V2, sep=" ")
df$start <- paste(df$start, df$V3, sep=" ")

df$quad_top <- quad_top

head(df)
colnames(df) <- c("word_1", "word_2","end","start", "freq")
head(df[,c(5,4,6)])
df1 <- (df[,c(5,4,6)])
colnames(df1)<-c("start","end","freq")
head(df1)
quadgram <- df1

save(quadgram, file="quadgram.RData")
