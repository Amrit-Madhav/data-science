
#Bank Data to predict the clients which subscribed to the campaign held.

## PART 1  ##Setting working directory and importing libraries
library(rpart)
install.packages("caret")
library(caret)
install.packages("caTools")
library(caTools)
install.packages("Amelia")
library(Amelia)
install.packages("randomForest")
library(randomForest)
install.packages("ROCR")
library("ROCR")
library(rpart)
library(rpart.plot)

library(caret)
install.packages("C50")
library(c50)

setwd("D:/AMRIT/PGCBAMD/courses/Data_mining")
bank_sales_data <- read.csv("Bank_Customer_Data.csv", header =  TRUE)
######Check column variables
str(bank_sales_data)
## To check basic statistical information of each variable
summary(bank_sales_data) 
####  Checking for Missing Data

is.na(bank_sales_data) ## Displays True for a missing value
print(dim(bank_sales_data))  ##45211 rows and 20 columns

## PART 2 To see missing data in a graphical way  #####

missmap(bank_sales_data,main="Data Missing - Bank Campaign", col=c("red","grey"),legend=FALSE)
## Red colour stripes is visible for columns x,x.1,x.2


##Removing Removing  Columns having NA (variables)

#Apply function = 2, for columns only; default is 1, for rows or lists

na_colsum <- apply(bank_sales_data, 2, function(x){sum(is.na(x))})
bank_sales_data <- bank_sales_data[, which(na_colsum == 0)]

## Checking whether the missing value column data has got removed,
#to get a cleaner dataset.

missmap(bank_sales_data,main="Missing Data - Bank Subscription", col=c("red","grey"),legend=FALSE)

print(dim(bank_sales_data))   ##45211 rows and 17 columns


##PART 3  To remove Outlier #################

# Checking difference between mean and median in summary if its more there then it will  be a outlier
boxplot(bank_sales_data$age, main="Age Box plot",
        yaxt="n", xlab="Age", horizontal=TRUE,
        col=terrain.colors(2))
# Histogram plot
hist(bank_sales_data$age,col=terrain.colors(10))

#Filtering the outliers from AGE parameter where age<=60
bank_sales_data <- subset(bank_sales_data,age<=60)
hist(bank_sales_data$age,col=terrain.colors(10))

###Selecting Subset of the columns numbers 1,2,3,4,7,8,9,12,14,15,17

bank_sales_data <-bank_sales_data[, c(1:4,7:9,12,14,15,17)]
str(bank_sales_data)


####PART 4 Data Tranformations#########
##Segregating age into different category (1 to 20, 20 to 40, 40 to 60, 60 to 100)
bank_sales_data$age <- cut(bank_sales_data$age, c(1,20,40,60,100)) 

##Segregating marital status into divorced,single,married else NULL
bank_sales_data$is_divorced <- ifelse( bank_sales_data$marital == "divorced", 1, 0)
bank_sales_data$is_single <- ifelse( bank_sales_data$marital == "single", 1, 0)
bank_sales_data$is_married <- ifelse( bank_sales_data$marital == "married", 1, 0)
bank_sales_data$marital <- NULL
str(bank_sales_data)

###  PART 5  ########

#Rows selection for training data set
set.seed(200)
#####Training and testing split############

#Rows selection for training data set in the ratio of 70:30 here prob=0.7
train_data_set <- createDataPartition(y=bank_sales_data$y ,p=0.7,list=FALSE)
training_data_set <- bank_sales_data[train_data_set,]
testing_data_set <- bank_sales_data[-train_data_set,]

### ###  PART 6 ######
############ Random Forest##############

##Creating Model for Random Forest

model <- randomForest(y ~ ., data=training_data_set)
#model <- randomForest(y ~ ., data=training_data_set,ntree=10,proximity=TRUE,importance=TRUE)
model

#importance of each predictor
importance(model)
###plot
plot(model)
##Variable Importance Plot
varImpPlot(model,
           sort = T,
           main="Variable Importance",
           n.var=5)

#####  PART 7  ########
############ Testing Random forest ############

predicted <- predict(model, testing_data_set,type="class")
table(predicted)
## Below showing the actual subscribed as per data and predicted subscribed by the trained model
tb <- confusionMatrix(predicted, testing_data_set$y)
tb

#### PART 8 Decision Tree  #####################

##Checking the Accuracy using Decision Tree
##Going to compare Random Forest Accuracy of Prediction and Decision Tree Accuracy

#################Decision Tree#################

dt_model<- rpart(y ~ ., data = training_data_set,method = "class")

plot(dt_model)

summary(dt_model)

####Implementing the model on to the Testing data set  #################
predictions <- predict(dt_model, testing_data_set, type = "class")
# Lets try for individual Values
predict(dt_model, testing_data_set[,-10])

table(predictions)
# confusion matrix
confusion.matrix <- prop.table(table(predictions, testing_data_set$y))
confusion.matrix
confusionMatrix(predictions,testing_data_set$y)


## Part 9 SVM

# SVM model using R
install.packages("e1071")
library(e1071)

classifier = svm(formula = y ~ ., data = training_data_set,type = 'C-classification',kernel = 'linear')
classifier
#predicting the test set results
head(testing_data_set)
y_pred_test_data = predict(classifier,newdata = testing_data_set[-10] )
y_pred_test_data
