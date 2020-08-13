rm(list = ls())
getwd()
setwd("C:/Users/lanjing/Desktop")

#read a txt file 
file <-'JingLan - resume 2019 Sep .txt'
sba <- scan(file,character(0),sep='\n')
summary(sba)
str(sba)

#part 1, word cloud 
install.packages('tm')
library(tm)
words.vec <- VectorSource(sba)
words.corpus <-Corpus(words.vec)
words.corpus
#coerce our text file vector (sba) into a custom Class
#provided by the tm package and called a corpus, storing the result in a new
#data object called words.corpus.

words.corpus <- tm_map(words.corpus,content_transformer(tolower))
words.corpus <- tm_map(words.corpus,removePunctuation)
words.corpus <- tm_map(words.corpus,removeWords,stopwords('english'))
#use of the tm_map() function,first making all of the letters lowercase, then removing
#the punctuation, then removing numbers, and finally taking out the so-called
#stop words.

tdm <- TermDocumentMatrix(words.corpus)
tdm
#use term-document matrix to conduct a kind of statistical analysis of the corpus


install.packages("wordcloud")
library(wordcloud)

m <-as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing = TRUE)
head(wordCounts)
cloudFrame <- data.frame(word =names(wordCounts),freq=wordCounts)
wordcloud(cloudFrame$word,cloudFrame$freq, min.freq = 2,
          max.words = 80, rot.per = 0.5,colors = 'light blue')
#install and library() the wordcloud package.pass to the wordcloud() function the term list and
#frequency list and visualize the result.

#part2, sentiment analysis
pos <-'positive-words.txt'
neg <-'negative-words.txt'
#define the name of the word files

p <-scan(pos,character(0),sep='\n')
n <-scan(neg,character(0),sep='\n')
#separate each word

totalWords <- sum(wordCounts)
#calculate the total number of words
words <- names(wordCounts)
#store all the words  in a vector

matched <- match(words,p,nomatch = 0)
#use match function to see how many words match in the positive words.
mCounts <-wordCounts[which(matched !=0)]
length((mCounts))
mWords <- names(mCounts)
nPos <- sum(mCounts)
nPos
#there were 13 positive words

matched <- match(words,n,nomatch = 0)
nCounts <-wordCounts[which(matched !=0)]
length((nCounts))
nWords <- names(nCounts)
nNeg <- sum(nCounts)
nNeg
#there were 3 negative words

totalWords <- length(words)
ratioPos <-nPos/totalWords
ratioPos
#5.3% positive words

ratioNeg <- nNeg/totalWords
ratioNeg
#1.2% negative words
