#####Twitter data extraction
install.packages("twitteR")
install.packages("stringr")
install.packages("ggplot2",dependencies = T)
install.packages("dplyr",dependencies = T)
install.packages("MASS")
install.packages("tm",dependencies = T)
install.packages("syuzhet")
install.packages("e1071")
library(e1071)
library(caret)
if (!require(RJSONIO)) {install.packages("RJSONIO")}
library(syuzhet)
library(RJSONIO)
library("tm")
library(MASS)
library(dplyr)
library(ggplot2)
library(twitteR)
install.packages("ROAuth")
library(ROAuth)
library(stringr)
library(wordcloud)
library(twitteR)
library(tm)
library(topicmodels)
library(sentiment)
install.packages("sentiment",dependencies = T)
library(ggplot2)
library(wordcloud)
library(data.table)
library(devtools)
install.packages("rjson")
library(rjson)
install_github('sentiment140', 'okugami79')
library(sentiment)
install.packages("Rcpp")
library(Rcpp)
install.packages("plyr")
#install.packages("sentimentr")
#library(sentimentr)

#Step 1: Establishing the connection with Twitter App:->
###Setting working directory
setwd("D:/AMRIT/PGCBAMD/courses/Text Mining/Text_Mining_Assignment")
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
consumer_key = "generate your consumer key"
consumer_secret = "generate your consumer_secret"
access_token = "Put your token"
access_secret = "put your access_secret"
#########################

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

result = searchTwitter("#donaldtrump",since='2017-01-01',n=1000,resultType='recent')
searchString <- "#donaldtrump"
number= 3000
tweetsDF = twListToDF(result)
#head(tweetsDF,10)
#View(tweetsDF)
#######   Step 2     ###############
##Preparing posistive and negative words dictionary

positive_words=scan('positive_words.txt',what='character',comment.char=';')
negative_words=scan('negative_words.txt',what='character',comment.char=';')
#positive[10:30]
#negative[5:50]

########  Step 3: Pre processing  ##############

##Preparing new data frame for cleaned tweets
# Converting and  encode it to native
twitter_tweets <- tweetsDF
twitter_tweets$text <- enc2native(twitter_tweets$text)

#View(twitter_tweets$text)
##Removing the URLS FROM THE TWEETS

# Extracting URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA
-F][0-9a-fA-F]))+"
twitter_tweets$contentURL <- str_extract(twitter_tweets$text, url_pattern)


##### Cleaning the content of text
twitter_tweets$text <- gsub("^[[:space:]]*","",twitter_tweets$text) # Removed leading whitespaces
twitter_tweets$text <- gsub("[[:space:]]*$","",twitter_tweets$text) # Removed trailing whitespaces
twitter_tweets$text <- gsub(" +"," ",twitter_tweets$text) # Removed extra whitespaces
twitter_tweets$text <- gsub("'", "", twitter_tweets$text) # Replaced apostrophes 
twitter_tweets$text <- iconv(twitter_tweets$text, "latin1", "ASCII", sub="") # Removed emojis/dodgy unicode
twitter_tweets$text <- gsub("<(.*)>", "", twitter_tweets$text) # Removed pesky Unicodes like <U+A>
twitter_tweets$text <- gsub("\\ \\. ", "", twitter_tweets$text) # Replaced orphaned fullstops with space
twitter_tweets$text <- gsub("  ", " ", twitter_tweets$text) # Replaced double space with single space
twitter_tweets$text <- gsub("%%", "\'", twitter_tweets$text) 
twitter_tweets$text <- gsub("https(.*)*$", "", twitter_tweets$text) # removed tweet URL
twitter_tweets$text <- gsub("\\n", "-", twitter_tweets$text) # replaced line breaks with -
twitter_tweets$text <- gsub("--", "", twitter_tweets$text) # removed double - from double line breaks
twitter_tweets$text <- gsub("&amp;", "&", twitter_tweets$text) # fixed ampersand &
head(twitter_tweets$text,10)
twitter_tweets$text=gsub("^\\s+|\\s+$", "", twitter_tweets$text)
head(twitter_tweets$text,10)
twitter_tweets$text=gsub('@\\w+','',twitter_tweets$text)
twitter_tweets$text=gsub('http\\w+','',twitter_tweets$text)
head(twitter_tweets$text,10)
twitter_tweets$text=gsub("^\\s+|\\s+$", "", twitter_tweets$text)
head(twitter_tweets$text,10)
twitter_tweets$text=gsub('[[:punct:]]','',twitter_tweets$text)
twitter_tweets$text=gsub('[[:cntrl:]]','',twitter_tweets$text)
head(twitter_tweets$text,10)

# removing all strange characters
twitter_tweets$text <- gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', " ", twitter_tweets$text)
head(twitter_tweets$text,10)

twitter_tweets$text=gsub('^c\\(|\\)$', '', twitter_tweets$text)
head(twitter_tweets$text,10)
twitter_tweets$text=gsub('[[:digit:]]','',twitter_tweets$text) ##removing the digits
twitter_tweets$text=gsub(',','',twitter_tweets$text)
head(twitter_tweets$text,10)

# Removing trailing whitespaces
for (i in 1:nrow(twitter_tweets)) {
  if (twitter_tweets$truncated[i] == TRUE) {
    twitter_tweets$text[i] <- gsub("[[:space:]]*$","",twitter_tweets$text[i])
  }
}
View(twitter_tweets)
# Removing unused columns
Tweet_data <- twitter_tweets %>% select("text", "contentURL", "favorited", "favoriteCount",
                            "created", "truncated", "id", "screenName",
                            "retweetCount", "isRetweet", "retweeted")
#head(Tweet_data)
##for Text Analysis
Tweet_corpus <- twitter_tweets %>% select("text")
head(Tweet_corpus,10)

###########    STEP 4:    #####################

################Sentiment Analysis
#####Calculating the Score
for (j in 1:nrow(Tweet_corpus)) {
  theTweet <- tolower(Tweet_corpus$text[j])
  tweetWords <- str_split(theTweet, "\\s+")
  words <- unlist(tweetWords)
  posMatches <- match(words, positive_words)
  negMatches <- match(words, negative_words)
  posMatches <- !is.na(posMatches)
  negMatches <- !is.na(negMatches)
  score <- sum(posMatches) - sum(negMatches)
  Tweet_corpus$sentimentScore[j] <- score
}


####GRAPH of the Sentiment score of the tweets

plotData <- Tweet_corpus[c("text", "sentimentScore")]
xLabel <- paste("Sentiment Score.  Mean sentiment: ",
                round(mean(Tweet_corpus$sentimentScore), 2), sep = "")
yLabel <- paste("Number of Tweets (", nrow(Tweet_corpus),")", sep = "")
graphTitle <- paste("Twitter Sentiment Analysis of ", searchString, sep = "")

qplot(factor(sentimentScore), data=plotData,
      geom="bar",
      fill=factor(sentimentScore),
      xlab = xLabel,
      ylab = yLabel,
      main = graphTitle) +
  theme(legend.position="none")

########### STEP 5:- Analyzing twitter feeds

##Step 5.1
###Here i am creating the tweet corpus and adding a new column having sentiment class
## column name is class_sentiment.This new column will help me in getting sentiment
##class as per sentiment score.
Tweet_corpus$class_sentiment <- ifelse( Tweet_corpus$sentimentScore >0, 'pos', 'neg')
head(Tweet_corpus,5)

##Step 5.2
###preparing the corpus for positive and negative counts
Twt_corpus<- Tweet_corpus
Twt_corpus <- str_split(Tweet_corpus, "\\s+")
Twt_corpus <- unlist(Twt_corpus)

##########################################
head(Tweet_corpus,5)
head(Twt_corpus,5)


######## STEP 5.3     #############
##counting +ve and -ve words in the tweets
##below is the Function to calculate +ve tweet counts
#here i am comparing  the twitter text feeds with the word dictionaries and retrieve 
#out the matching words. To do this, i have first defined a function to count 
#the number of positive and negative words that are matching with my dictionary

##function pos_score for counting the positive matching words
pos_score=function(tweet) {
  pos.match=match(tweet,positive_words)
  pos.match=!is.na(pos.match)
  pos.score=sum(pos.match)
  return(pos.score)
}
positive_score=lapply(Twt_corpus,function(x) pos_score(x))
#length(positive_score)  
#length(Twt_corpus)  
head(positive_score,10)

##to count the total number of positive words present in the tweets
p_count=0
for (i in 1:length(positive_score)) {
  p_count=p_count+positive_score[[i]]
}
p_count   ##491  positive tweets as per the prepared word dictionary

###Negative score
##neg_score for counting the negative matching words
neg_score=function(tweet) {
  neg.match=match(tweet,negative_words)
  neg.match=!is.na(neg.match)
  neg.match=sum(neg.match)
  return(neg.match)
}
negative_score=lapply(Twt_corpus,function(x) neg_score(x))
length(negative_score)   ##69384
#head(Twt_corpus,40)

####to count the total number of negative words present in the tweets as per our dictionary
n_count=0
for (i in 1:length(negative_score)) {
  n_count=n_count+negative_score[[i]]
}
n_count   ##726  ##negative tweets as per the prepared word dictionary


#########  STEP 5.4 ####################
##The below code retrieves the positive and negative matching words as per dictionary

###Finding the Positive Match:-> between twitter tweets and word dictionary

poswords=function(tweets){
  pmatch=match(t,positive_words)
  posw=positive_words[pmatch]
  posw=posw[!is.na(posw)]
  return(posw)
}

words=NULL
pdatamart=data.frame(words)

for (t in Twt_corpus) {
  pdatamart=c(poswords(t),pdatamart)
}
head(pdatamart,10)

###Negative Match
negwords=function(tweets){
  nmatch=match(t,negative_words)
  negw=negative_words[nmatch]
  negw=negw[!is.na(negw)]
  return(negw)
}

words=NULL
ndatamart=data.frame(words)

for (t in Twt_corpus) {
  ndatamart=c(negwords(t),ndatamart)
}
head(ndatamart,30)
##Finding high frequency negative and positive words
###UNLIST TO VECTORS
pwords = unlist(pdatamart)
nwords = unlist(ndatamart)
#head(pwords)
#pwords
###plotting high frequency words
##Converting Vector to dataframe AND
##MAKING contingency table of the counts at each combination of factor levels..
dpwords=data.frame(table(pwords))
dnwords=data.frame(table(nwords))
head(dpwords,5)
head(dnwords,5)
##Using dplyr package, need to first mutate the words as character variables and then 
#filter for frequency >15 repetitions for positive and negative words.


#########  STEP 6 ################
##POSITIVE WORDS with freq > 15
dpwords=dpwords%>%
  mutate(pwords=as.character(pwords))%>%
  filter(Freq>15)
##NEGATIVE words with freq > 15
dnwords=dnwords%>%
  mutate(nwords=as.character(nwords))%>%
  filter(Freq>15)
############  Plotting  ################
##plotting the major positive words and their frequency by using ggplot2 package
####### plot for positive words
ggplot(dpwords,aes(pwords,Freq))+geom_bar(stat="identity",fill="lightblue")+theme_bw()+
  geom_text(aes(pwords,Freq,label=Freq),size=4)+
  labs(x="Major Positive Words", y="Frequency of Occurence",title=paste("Major Positive Words and Occurence in \n '",searchString,"' twitter feeds, n =",number))+
  geom_text(aes(1,5,label=paste("Total Positive Words :",p_count)),size=4,hjust=0)+theme(axis.text.x=element_text(angle=45))

######  plot for negative words
ggplot(dnwords,aes(nwords,Freq))+geom_bar(stat="identity",fill="lightblue")+theme_bw()+
  geom_text(aes(nwords,Freq,label=Freq),size=4)+
  labs(x="Major Negative Words", y="Frequency of Occurence",title=paste("Major Negative Words and Occurence in \n '",searchString,"' twitter feeds, n =",number))+
  geom_text(aes(1,5,label=paste("Total Negative Words :",n_count)),size=4,hjust=0)+theme(axis.text.x=element_text(angle=45))


###############################################################################
################### Step 7    #################
#####Twitter Analysis to calculate sentiments using sentiment package  ###########
##converting the Twt_corpus into a word corpus using the function VectorSource
###Using tm package
###creating the corpus using vectorsource:->

tweets_data=Corpus(VectorSource(Twt_corpus))

#tweets_data
inspect(tweets_data)
##head(tweets_data)
#removing un required words from the corpus
####Removing common words and creating wordcloud
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
inspect(tweets_data)
tweets_data <- tm_map(tweets_data, toSpace, "[])(;:#%$^*\\~{}[&+=@/`|<>_]+")
tweets_data <- tm_map(tweets_data, toSpace, "'")
tweets_data<-tm_map(tweets_data, removeNumbers)
inspect(tweets_data)

#tweets stopwords("english"))
tweets_data = tm_map(tweets_data, removeWords, c("the", "and", stopwords("SMART")))
inspect(tweets_data)
myStopwords <- c("can", "say","one","way","use","deci","RT","JR","en","El","insisti","Ame","the",
                 "also","however","tell","will",
                 "much","need","take","tend","even","una",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end","le","de",
                 "first","two","help","often","may",
                 "might","see","something","thing","point",
                 "post","look","right","now","think","us","un","la",
                 "inc","pza","four","one","three","mln","saying","gave","pct","per","since","nearly",
                 "six","says")
#removing custom stopwords
tweets_data <- tm_map(tweets_data, removeWords, myStopwords)
inspect(tweets_data)
##keeping a copy of the corpus
tweet_corpusCopy<-tweets_data #keep this for later
tweet_corpusCopy_v<-tm_map(tweet_corpusCopy, PlainTextDocument)

##creating a Word Cloud of tweets using the wordcloud package
wordcloud(tweets_data,scale=c(10,0.5),random.order = TRUE,rot.per = 0.20,use.r.layout = FALSE,colors = brewer.pal(6,"Dark2"),max.words = 300)

######## STEP 8 #######

##Analyzing and plotting high frequency words
##converting the word corpus into a Term Document Matrixx using the function TermDocumentMatrix:->

tweet_dtm<-TermDocumentMatrix(tweets_data, control = list(wordLengths = c(1, Inf)))
term_freq<-rowSums(as.matrix(tweet_dtm))
term_freq<-subset(term_freq, term_freq>=30)
tdm_df<-data.frame(term=names(term_freq), freq=term_freq)
##plotting the most frequent words. Frequent words are defined 
##above as those that appear over 30 times

ggplot(data=tdm_df)+geom_bar(aes(x=term, y=freq), stat="identity")+xlab("Terms")+ylab("Count")+coord_flip()+theme(axis.text=element_text(size=10))

######WordClouds
# words appearing at least 6 times.
tdm_matrix<-as.matrix(tweet_dtm)
word_freq<-sort(rowSums(tdm_matrix), decreasing=T)
pal<-brewer.pal(8, "Dark2")
wordcloud(words=names(word_freq), freq=word_freq, min.freq=6, random.order = F, colors=pal)

###########  STEP 9:  ##########
##Topic modeling
###What does #donal dtrump tweet about on a higher level
## topic modeling to discover commonly  words 
##and grouping them into predetermined buckets i have used k=5.


dtm_df<-as.DocumentTermMatrix(tweet_dtm)
rowTotals <- apply(dtm_df , 1, sum) 
dtm_corrected   <- dtm_df[rowTotals> 0, ] 
lda<-LDA(dtm_corrected, control=list(seed=123) ,k=5) 
term<-terms(lda, 5)
(term<-apply(term, MARGIN=2, paste, collapse=", "))


##### STEP 10  ##########
####Sentiment Analysis using Sentiment package
#What is the overall attitude of @donaldtrump 
#Here i am using sentiment package. It classifies every tweet as 
#either “negative”, “neutral”, or “positive” 
#based on the amount of positive/negative words.

sentiment_twitter_df<-data.frame(text = sapply(tweet_corpusCopy_v, paste, collapse = " "), stringsAsFactors = FALSE)
sentiment_analysis_package<-sentiment(sentiment_twitter_df$text)
table(sentiment_analysis_package$polarity)


#########   STEP 11   #########
#######Sentiment Classification using NAIVE BAYES
##The prediction accuracy of a classification model is given by the 
#proportion of the total number of correct predictions.

dtm_naiveBayes <- DocumentTermMatrix(tweets_data)
inspect(dtm_naiveBayes[1:50, 1:20])

##partioning the data
df.train <-Tweet_corpus[1:700,]
df.test <- Tweet_corpus[701:1000,]

dtm.train <- dtm_naiveBayes[1:700,]
dtm.test <- dtm_naiveBayes[701:1000,]

corpus.clean.train <- tweets_data[1:700]
corpus.clean.test <- tweets_data[701:1000]

##Feature Selection
dtm.train
dtm.test
dim(dtm.train)

#i am  reducing the number of features by ignoring words 
#which appear in less than 1 tweets.
#to use only the frequent words using the ‘dictionary’ option.
one_freq <- findFreqTerms(dtm.train, 1)
length((one_freq))

# Use only 1 most frequent words (one_freq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = one_freq))
dtm.train.nb
dim(dtm.train.nb)
##

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = one_freq))

dim(dtm.train.nb)


##The Naive Bayes algorithm
##The Naive Bayes text classification algorithm is essentially an application of Bayes theorem

##using variation of the multinomial Naive Bayes algorithm..

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("neg", "pos"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)


#####Training the Naive Bayes Model
#To train the model i am using the naiveBayes function 
#from the ‘e1071’ package. Since Naive Bayes evaluates products of 
#probabilities,  
## i am using  Laplace 1 smoothing

# Train the classifier
classifier <- naiveBayes(trainNB, df.train$class_sentiment, laplace = 1)
classifier

# Use the NB classifier to built to make predictions on the test set.
pred <- predict(classifier, newdata=testNB)
head(pred)
pred
# Creating a truth table by tabulating the predicted class labels with the actual class labels 
table("Predictions"= pred,  "Actual" = df.test$class_sentiment )

length(df.test)
head(df.test$class_sentiment)

confusion_matrix <- confusionMatrix(pred, df.test$class)
