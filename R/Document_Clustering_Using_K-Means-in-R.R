##Document Clustering
install.packages("tm",dependencies = T)
install.packages("wordcloud",dependencies = T)
install.packages("Rgraphviz",dependencies = T)
install.packages("RColorBrewer",dependencies = T)
install.packages("topicmodels",dependencies = T)
install.packages("plyr",dependencies = T)
install.packages("ggplot2",dependencies = T)
install.packages("RTextTools",dependencies = T)
install.packages("e1071",dependencies = T)
install.packages("RGraphics",dependencies = T)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(Rgraphviz)
source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(Rgraphviz)
install.packages("graph",dependencies = T)
install.packages("prodlim",dependencies = T)
library(prodlim)
library(graph)
help(Rgraphviz)
library(wordcloud)
library(topicmodels)
library(plyr)
library(ggplot2)
library(RTextTools)
library(e1071)
library(dendextend) #coloured dendrogram
install.packages("dendextend",dependencies = T)
install.packages("robustbase",dependencies = T)
install.packages("kernlab")
library(kernlab)
library(dendextend)
library(robustbase)
###Step 2 :Loading Data
##D:\AMRIT\PGCBAMD\courses\Text Mining\Text_Mining_Assignment
setwd("D:/AMRIT/PGCBAMD/courses/Text Mining/Text_Mining_Assignment/")

reuters_data <- read.table('reuters_article.txt', header=FALSE, sep='\t')

###########
#D:\AMRIT\PGCBAMD\courses\Text Mining\t1\t1
##D:\AMRIT\PGCBAMD\courses\Text Mining\Text_Mining_Assignment\tma
#setwd("D:/AMRIT/PGCBAMD/courses/Text Mining/t1/")
#D:\AMRIT\PGCBAMD\courses\Text Mining
#options(header=FALSE,stringsAsFactors = FALSE,fileEncoding='UTF-8')
##options(header=FALSE,sep='\t',fileEncoding='UTF-8')

##/* Below is the code for doing clustering for different physical documents and then merging in one (Corpus(DirSource("tma")))  */
###not using it as i have created a single document
#reuters_data<-Corpus(DirSource("tma"))
head(reuters_data)
unique(reuters_data$V1)
##head(reuters_data)
#inspect(reuters_data)

##Creating the Vector Source
source <- VectorSource(reuters_data$V2)
#source <- VectorSource(reuters_data)
corpus <- Corpus(source)
head(corpus)
##Tagging words
tag_words<-unique(reuters_data$V1)
tag_words

##Step 2:
##Cleaning and Pre Processing the data
##all to lower case
corpus <- tm_map(corpus, content_transformer(tolower))
inspect(corpus)
###
#remove potentiallyy problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
corpus <- tm_map(corpus, toSpace, "-")

corpus <- tm_map(corpus, toSpace, ":")

corpus <- tm_map(corpus, toSpace, "‘")
corpus <- tm_map(corpus, toSpace, "•")
corpus <- tm_map(corpus, toSpace, "•    ")

corpus <- tm_map(corpus, toSpace, " -")
corpus <- tm_map(corpus, toSpace, "“")
corpus <- tm_map(corpus, toSpace, "”")
###
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
myStopwords <- c("can", "say","one","way","use",
                                  "also","however","tell","will",
                                "much","need","take","tend","even",
                                "like","particular","rather","said",
                              "get","well","make","ask","come","end",
                              "first","two","help","often","may",
                             "might","see","something","thing","point",
                            "post","look","right","now","think","ve ",
                          "re","vs","inc","pza","four","one","three","mln","saying","gave","pct","s","t","per","since","nearly",
                 "sec","tre","looking","six","U","says")
#remove custom stopwords
corpus <- tm_map(corpus, removeWords, myStopwords)
corpus <- tm_map(corpus,stemDocument,language="english")
#inspect(corpus)

##Step 3:-
##Creating DocumentTermMatrix
####CLUSTERING  ###########
##create a document-term matrix:
mat <- DocumentTermMatrix(corpus)

mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)

##creating wordcloud
wordcloud(colnames(mat4), mat4[3, ], max.words = 70)
##creating wordcloud:
#wordcloud(colnames(mat4), mat4[3, ], max.words = 100)

###Step 4:Normalization
##Normalizing the scores
##normalize the Tf-Idf scores by euclidean distance
##Function below normalizes using Euclidean function

#norm_eucl <- function(m)
 # m/apply(m,1,function(x) sum(x^2)^.5)
#mat_norm <- norm_eucl(mat4)
#dim(mat_norm) 

mat_norm <-dist(mat4, method = "euclidean", diag = FALSE, upper = FALSE)
#print(mat_norm)
##Normalization using COSINE
#mat_norm <- dist(mat4, method="cosine")

freq <- colSums(as.matrix(mat_norm))
hist(freq, breaks=100)

###STEP 5
###K-MEANS Finding the correct number of clusters
#kmeans – determine the optimum number of clusters (elbow method)
#Checking for “elbow” in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:100
for (i in 2:100) wss[i] <- sum(kmeans(mat_norm,centers=i,nstart=25)$withinss)
plot(2:100, wss[2:100], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
##The plot clearly shows that there is no k for which the summed WSS 
#flattens out (no distinct “elbow”).  

###Step 6: K-MEANS Clustering
#k means algorithm, 3 clusters, 100 starting configurations
k_mean <- kmeans(mat_norm, 3, nstart=100)
k_mean$cluster[1:101] ##where each document has been placed(i.e in which cluster)
count(k_mean$cluster)


##Step 7:Validation of the Model
##Checking the Model
###Model Performance
result <- data.frame('actual'=reuters_data$V1, 'predicted'=k_mean$cluster)
result <- result[order(result[,1]),]
result$counter <- 1
result.agg <- aggregate(counter~actual+predicted, data=result, FUN='sum')

result.agg

ggplot(data=result.agg, aes(x=actual, y=predicted, size=counter)) + geom_point()

##Step 8 Performing Hierarchial Clustering
####HIERACHIAL Clustering
##hierarchical clustering using Ward’s method
groups <- hclust(mat_norm,method="ward.D")
#plot dendogram, using hang to ensure that labels fall below tree
plot(groups, hang=-1)
rect.hclust(groups, k=3)

dend <- groups

dend <- color_branches(dend, k = 3)
dend <- color_labels(dend, k = 3)

#represent the different  clusters with different colours
plot(dend, main = 'Cluster Dendrogram', ylab = 'Height')

##STEP 9: TOPIC Modelling
##########Topic Modeling  #############
k <- 7
lda <- LDA(mat, k)
terms(lda)

x <- topics(lda)
desc_topics_freq <- data.frame('response'=names(x), 'topic'=x, row.names=NULL)
count(desc_topics_freq, vars='topic')

##step 10 :- HIERACHIAL CLUSTERING USING PCA
##########HIERACHIAL CLUSTERING USING PCA:####################

#reduce dimensions using PCA
install.packages("irlba",dependencies = T)
library(irlba)

pca1 <- prcomp(mat4,scale=TRUE)
summary(pca1)
#View(pca1$rotation)

#retain first 100 components based on the percenatge of variance explained
p <- as.matrix(pca1$rotation[,1:100])
q <- as.matrix(mat4)
final <- as.data.frame(q%*%p)
final
#for clustering, either heirarichal or k means clustering can be used

#heirarichal clustering after performing PCA
d <- dist(final, method="euclidean")
fit <- hclust(d,method="ward.D")
plot(fit, hang=-1)
fit
table(groups)
mydf <- mat4
mydf <- cbind(mat4,groups)
mydf
