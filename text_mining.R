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

setwd("E:\\RWorkingDirectory\\coursera_wordpred\\")

getwd()

x<-file("en_US.blogs.txt","r")

eBlg <- readLines(x)
eBlg_s <- readLines(x, 5000)

length(eBlg)
length(eBlg_s)

dim(eBlg)

close(x)

x<-file("en_US.news.txt","r")

eNews <- readLines(x,5000)
eNews_s <- readLines(x, 5000)

length(eNews)

close(x)

x<-file("en_US.twitter.txt","r")

eTwit <- readLines(x,5000)
length(eTwit)

close(x)

ename <- c("Blog","News","Twit")

ecount <- c(length(eBlg),length(eNews),length(eTwit))

df = data.frame(ename, ecount)   

barplot(df$ecount, main="en US data",
        ylab="Number of records",xlab="Feed",names.arg =df$ename, col=c("brown","red","orange"), border="blue")


#----------- Sampling

x<-file("en_US.blogs.txt","r")

eBlg_s <- readLines(x, 3000)

length(eBlg_s)

close(x)

x<-file("en_US.news.txt","r")

eNews_s <- readLines(x, 3000)

length(eNews_s)

close(x)

x<-file("en_US.twitter.txt","r")

eTwit_s <- readLines(x, 3000)

length(eTwit_s)

close(x)

efeed_s <- c(eBlg_s,eNews_s,eTwit_s)

vc <- Corpus(VectorSource(efeed_s))

#twitc <- Corpus(VectorSource(eTwit_s))

length(vc)

evc = tm_map(vc, tolower)
evc = tm_map(evc, removePunctuation)
evc = tm_map(evc, removeNumbers)
evc = tm_map(evc, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(evc)

#dtm <- DocumentTermMatrix(twitc)
dtm_matrix <- as.matrix(dtm)

wordcount <- colSums(dtm_matrix)
#wordcount
topten <- head(sort(wordcount, decreasing=TRUE), 10)

topfif <- head(sort(wordcount, decreasing=TRUE), 50)



dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
print(fig)

names(topten)

wordcloud(evc, max.words = 20, random.order = FALSE, colors = brewer.pal(8,"Dark2"))

#df <- as.data.frame(dtm)

#unnest_tokens(df, text, token = "ngrams", n = 2)


efeed_s <- removePunctuation(efeed_s)

efeed_s <- iconv(efeed_s, "latin1", "ASCII", sub="")

class(efeed_s)

unigram <- tokens(efeed_s,ngrams = 1L)


bigram <- tokens(efeed_s,ngrams = 2L, concatenator = " ")
trigram <- tokens(efeed_s,ngrams = 3L, concatenator = " ")
quadgram <- tokens(efeed_s,ngrams = 4L, concatenator = " ")

unigram_clean <- dfm((unigram),remove_punct = TRUE)
unigram_clean <- dfm_remove(dfm(unigram_clean), stopwords("english"))

save(unigram_clean, file="unigram_clean.RData")

bigram_clean <- dfm((bigram),remove_punct = TRUE)
bigram_clean <- dfm_remove(dfm(bigram_clean), stopwords("english"))

save(bigram_clean, file="bigram_clean.RData")

trigram_clean <- dfm((trigram),remove_punct = TRUE)
trigram_clean <- dfm_remove(dfm(trigram_clean), stopwords("english"))

save(trigram_clean, file="trigram_clean.RData")

quadgram_clean <- dfm((quadgram),remove_punct = TRUE)
quadgram_clean <- dfm_remove(dfm(quadgram_clean), stopwords("english"))

save(quadgram_clean, file="quadgram_clean.RData")

uni_top <- topfeatures(unigram_clean, n = 10, decreasing = TRUE, scheme = c("count", "docfreq"), groups = NULL)

dfplot <- as.data.frame(uni_top)

dfplot$word <- dimnames(dfplot)[[1]]

dfplot$uni_top

dfplot$word <- factor(dfplot$word,levels=dfplot$word[order(dfplot$uni_top, decreasing=TRUE)])

fig1 <- ggplot(dfplot, aes(x=word, y=uni_top)) + geom_bar(stat="identity")
fig1 <- fig1 + xlab("Unigram in Corpus")
fig1 <- fig1 + ylab("Count")
print(fig1)

bi_top <- topfeatures(bigram_clean, n = 10, decreasing = TRUE, scheme = c("count", "docfreq"), groups = NULL)

dfplot2 <- as.data.frame(bi_top)

dfplot2$word <- dimnames(dfplot2)[[1]]

dfplot2$word <- factor(dfplot2$word,levels=dfplot2$word[order(dfplot2$bi_top, decreasing=TRUE)])

fig2 <- ggplot(dfplot2, aes(x=word, y=bi_top)) + geom_bar(stat="identity")
fig2 <- fig2 + xlab("Bi-gram in Corpus")
fig2 <- fig2 + ylab("Count")
print(fig2)

tri_top <- topfeatures(trigram_clean, n = 10, decreasing = TRUE, scheme = c("count", "docfreq"), groups = NULL)

dfplot3 <- as.data.frame(tri_top)

dfplot3$word <- dimnames(dfplot3)[[1]]

dfplot3$word <- factor(dfplot3$word,levels=dfplot3$word[order(dfplot3$tri_top, decreasing=TRUE)])

fig3 <- ggplot(dfplot3, aes(x=word, y=tri_top)) + geom_bar(stat="identity")
fig3 <- fig3 + xlab("Tri-gram in Corpus")
fig3 <- fig3 + ylab("Count")
print(fig3)

quad_top <- topfeatures(quadgram_clean, n = 10, decreasing = TRUE, scheme = c("count", "docfreq"), groups = NULL)

dfplot4 <- as.data.frame(quad_top)

dfplot4$word <- dimnames(dfplot4)[[1]]

dfplot4$word <- factor(dfplot4$word,levels=dfplot4$word[order(dfplot4$quad_top, decreasing=TRUE)])

fig4 <- ggplot(dfplot4, aes(x=word, y=quad_top)) + geom_bar(stat="identity")
fig4 <- fig4 + xlab("Quad-gram in Corpus")
fig4 <- fig4 + ylab("Count")
print(fig4)

allQuadgrams <- dimnames(quadgram_clean)$features
allTrigrams <- dimnames(trigram_clean)$features
allBigrams <- dimnames(bigram_clean)$features
allUnigrams <- dimnames(unigram_clean)$features

quadgrams <- do.call(rbind, strsplit(allQuadgrams, split = " "))
quadgrams <- cbind(apply(quadgrams[, 1:3], 1, function(x) paste(x, collapse = " ")), quadgrams[, 4])
quadgrams <- cbind(quadgrams[, 1],quadgrams[, 2])

save(quadgrams, file="quadgrams.RData")

trigrams <- do.call(rbind, strsplit(allTrigrams, split = " "))
trigrams <- cbind(apply(trigrams[, 1:2], 1, function(x) paste(x, collapse = " ")),trigrams[, 3])
trigrams <- cbind(trigrams[, 1],trigrams[, 2])

save(trigrams, file="trigrams.RData")

bigrams <- do.call(rbind, strsplit(allBigrams, split = " "))
#bigrams <- cbind(apply(bigrams[, 1], 1, function(x) paste(x, collapse = " ")),bigrams[, 2])
bigrams <- cbind(bigrams[, 1],bigrams[, 2])

save(bigrams, file="bigrams.RData")

