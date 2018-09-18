install.packages("caret",dependencies = T)
install.packages("bindrcpp",dependencies = T)
install.packages("tidyr",dependencies = T)
install.packages("ddalpha")
#install.packages("quanteda")
library(tibble)
library(RcppArmadillo)
#library(Rcpp)
library(quanteda)
library(RColorBrewer)
library(ggplot2)
library(ddalpha)
library(tidyr)
library(bindrcpp)
library(caret)


####### Assignment1-Classification of hamspam data using quanteda package ##############
##Loading the dataset
##Step 1
setwd("D:/AMRIT/PGCBAMD/courses/Text Mining/Text_Mining_Assignment")
sms_data<-read.csv("smsspam.csv",header=TRUE, sep=",", quote='\"\"', stringsAsFactors=FALSE)
#head(sms_data)

names(sms_data) <- c("class_mail", "comments")
sms_data[1,]
table(sms_data$class_mail)

#checking the distribution of type of messages
theme_set(theme_bw())
ggplot(aes(x=class_mail),data=sms_data) +
  geom_bar(fill="green",width=0.35)

##To make it consistent
set.seed(1234)
sms_data<-sms_data[sample(nrow(sms_data)),]  ##use sample function in Quanteda
head(sms_data)
##Constructing a Corpus from the character vector
#Adding Label to the corpus created ,so that we can associate SMS messages 
#with their respective ham/spam label
##STEP 2
corpus_comments<-corpus(sms_data$comments)
docvars(corpus_comments) <- sms_data$class_mail
head(corpus_comments)

###STEP 3: Pre Processing
##Pre-Processing the corpus dfm()
main_corpus <- dfm(corpus_comments, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE,remove_symbols = TRUE,remove_numbers = TRUE,stem = FALSE,remove=stopwords("smart"))


########### STEP 4:- Filtering and weighting ########
##document frequency per term
doc_freq <-docfreq(main_corpus) 
##selecting terms where docfreq >=2
main_corpus <- main_corpus[,doc_freq >=2]
##Weighting the features using tf-idf
main_corpus <- dfm_weight(main_corpus,"tfidf")
main_corpus <- dfm_tfidf(main_corpus)
head(main_corpus)  
##Document-feature matrix of: 6 documents, 8,048 features (99.9% sparse).
# keep only words occurring >= 10 times and in >= 2 documents
main_corpus <- dfm(main_corpus, verbose = FALSE)
sparsity(main_corpus)
sparsity(dfm_trim(main_corpus, min_termfreq = 10, min_docfreq = 2))
sparsity(main_corpus)

##"Compresses" or groups a dfm or fcm whose dimension names are the same, 
#for either documents or features

###Compress a dfm by combining identical elements
dfm_compress(main_corpus, margin = c("both", "documents", "features"))
##To see the features
main_corpus_trim = dfm_trim(main_corpus,min_count=10,min_docfreq = 5)
##generating top 50 features having minm count of 10 and 
#appearing in 5 of the documents
topfeatures(main_corpus_trim,n=50)

####  Step 5: Creating a cluster for the top 50 words
numwords=50

wordfm <- dfm_sort(dfm_weight(main_corpus_trim,"tfidf"))
head(wordfm)

wordfm = t(wordfm)[1:numwords]  ##keeping top 50 words
wordistmat = dist(wordfm)
##Creating a cluster for the to 50 words
wordcluster =hclust(wordistmat)
plot(wordcluster,xlab="",main="TFIDF Frequency weighing for first 50 words")

rect.hclust(wordcluster, k=2)

################## Step 6: Generating wordcloud:
###Generating the spam wordcloud
spam.corpus <- corpus_subset(corpus_comments, docvar1 == "spam") 
spam.corpus <- dfm(spam.corpus, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE,stem = FALSE,remove=stopwords("smart"))

title("Spam Wordcloud", col.main = "grey14")

spam.col <- brewer.pal(10, "BrBG")  
spam.cloud <- textplot_wordcloud(spam.corpus, min.freq = 16, color = spam.col)
##
#quanteda’s corpus() command to construct a corpus 
##UNIGRAMS IN SPAM and its FREQUENCIES:-
unigrams = dfm(spam.corpus, ngrams = 1, verbose = FALSE, concatenator = " ")
uni.freq <- colSums(unigrams)
uni.freq <- sort(uni.freq, decreasing=TRUE) 
nf.1 <- data.frame(word=names(uni.freq), freq1=uni.freq)

##Below to show Unigrams and their respective frequencies
plotUni <- ggplot(nf.1[1:20,], 
                  aes(x=reorder(word, freq1), 
                      y=freq1)) +
  geom_bar(stat = "identity", fill="#E69F00") +  
  coord_flip() +
  theme(legend.title=element_blank()) +
  xlab("Unigram") + 
  ylab("Frequency") +
  labs(title = "Top Unigrams by Frequency of SPAM")
print(plotUni)

###Generating the Ham wordcloud
ham.corpus <- corpus_subset(corpus_comments, docvar1 == "ham") 
ham.corpus <- dfm(ham.corpus, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE,stem = FALSE,remove=stopwords("smart"))

title("Ham Wordcloud", col.main = "grey14")

ham.col <- brewer.pal(10, "BrBG")  
ham.cloud <- textplot_wordcloud(ham.corpus, min.freq = 16, color = ham.col)

##UNIGRAMS in HAM Wordcloud and its frequencies
unigrams = dfm(ham.corpus, ngrams = 1, verbose = FALSE, concatenator = " ")
uni.freq <- colSums(unigrams)
uni.freq <- sort(uni.freq, decreasing=TRUE) 
nf.1 <- data.frame(word=names(uni.freq), freq1=uni.freq)

##Below to show Unigrams :-
plotUni <- ggplot(nf.1[1:20,], 
                  aes(x=reorder(word, freq1), 
                      y=freq1)) +
  geom_bar(stat = "identity", fill="#E69F00") +  
  coord_flip() +
  theme(legend.title=element_blank()) +
  xlab("Unigram") + 
  ylab("Frequency") +
  labs(title = "Top Unigrams by Frequency of HAM")
print(plotUni)

########  STEP 7: NAÏVE BAYES CLASSIFICATION  ############################
################# NAIVE Bayes classification  #############################
corpus_comments_dfm <- dfm(corpus_comments, tolower = TRUE)  
corpus_comments_dfm <- dfm_trim(corpus_comments_dfm, min_count = 5, min_docfreq = 3)  
corpus_comments_dfm <- dfm_weight(corpus_comments_dfm, type ="tfidf") 

sms_data.train <- sms_data[1:3902,]  
sms_data.test <- sms_data[3903:nrow(sms_data),]

corpus_comments_dfm.train <- corpus_comments_dfm[1:3902,]  
corpus_comments_dfm.test <- corpus_comments_dfm[3903:nrow(sms_data),]  
sms.classifier <- textmodel_nb(corpus_comments_dfm.train, sms_data.train$class_mail,smooth = 1,prior ="docfreq",distribution = "multinomial") 
sms.classifier

sms.predictions <- predict(sms.classifier, newdata = corpus_comments_dfm.test)  
sms.predictions
class_table <- table(sms.predictions$nb.predicted, sms_data.test$class_mail)
class_table
confusionMatrix(class_table, mode = "everything")