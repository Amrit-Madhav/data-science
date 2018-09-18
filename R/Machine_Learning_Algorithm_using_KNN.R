#  AUTHOR $ TA17012 AND  $ TA17020  AMRIT MADHAV  BHANU PRATAP SINGH 
install.packages("gvlma")
install.packages("lmtest")
install.packages("tseries")
install.packages("stats")
install.packages("varhandle")
install.packages("GGally")
install.packages("mctest")
install.packages('caTools')
install.packages("class")
install.packages("DT")
install.packages("dplyr", dependencies = TRUE)
install.packages('caTools')
install.packages("gmodels")
install.packages("kernlab")
install.packages('e1071')
library(e1071)
library(kernlab)
library(gmodels)
library(class)
library(DT)
library(dplyr)
library(caTools)
library(GGally)
library(stats)
library(lmtest)
library(tseries)
library(gvlma)
library(varhandle)
library(mctest)

##*********************************
##***Step 1:- 
##Setting working Directory
setwd("D:/AMRIT/PGCBAMD/courses/ML/ML_Assignment")

#Importing the dataset
set.seed(12)
HCC_data <- read.csv("HCC_Survival.csv",header=TRUE)
head(HCC_data)
str(HCC_data)
#################### STEP 2 ::->  DATA IMPUTATION  ##############################################################
######  STEP 2.1 : -> #######################################

##Data Imputation
##Imputing '?' with 'NA'
HCC_data$Symptoms[HCC_data$Symptoms == '?'] <- 'NA'
HCC_data$HBsAg[HCC_data$HBsAg == '?'] <- 'NA'
HCC_data$HBeAg[HCC_data$HBeAg == '?'] <- 'NA'
HCC_data$HBcAb[HCC_data$HBcAb == '?'] <- 'NA'

HCC_data$HBVAb[HCC_data$HBVAb == '?'] <- 'NA'
HCC_data$Endemic.Countries[HCC_data$Endemic.Countries == '?'] <- 'NA'
HCC_data$Smoking[HCC_data$Smoking == '?'] <- 'NA'
HCC_data$Diabetes[HCC_data$Diabetes == '?'] <- 'NA'

HCC_data$Obesity[HCC_data$Obesity == '?'] <- 'NA'
HCC_data$Hemochromatosis[HCC_data$Hemochromatosis == '?'] <- 'NA'
HCC_data$AHT[HCC_data$AHT == '?'] <- 'NA'
HCC_data$CRI[HCC_data$CRI == '?'] <- 'NA'

HCC_data$HIV[HCC_data$HIV == '?'] <- 'NA'
HCC_data$NASH[HCC_data$NASH == '?'] <- 'NA'
HCC_data$Esophageal.varices[HCC_data$Esophageal.varices == '?'] <- 'NA'
HCC_data$Splenomegaly[HCC_data$Splenomegaly == '?'] <- 'NA'

HCC_data$Portal.hypertension[HCC_data$Portal.hypertension == '?'] <- 'NA'
HCC_data$Portal.vein.thrombosis[HCC_data$Portal.vein.thrombosis == '?'] <- 'NA'
HCC_data$Liver.metastasis[HCC_data$Liver.metastasis == '?'] <- 'NA'
HCC_data$Radiological.hallmark[HCC_data$Radiological.hallmark == '?'] <- 'NA'

HCC_data$Portal.hypertension[HCC_data$Portal.hypertension == '?'] <- 'NA'
HCC_data$Portal.vein.thrombosis[HCC_data$Portal.vein.thrombosis == '?'] <- 'NA'
HCC_data$Liver.metastasis[HCC_data$Liver.metastasis == '?'] <- 'NA'
HCC_data$Radiological.hallmark[HCC_data$Radiological.hallmark == '?'] <- 'NA'

HCC_data$Grams...day[HCC_data$Grams...day == '?'] <- 'NA'
HCC_data$Packs...year[HCC_data$Packs...year == '?'] <- 'NA'
HCC_data$Encefalopathy[HCC_data$Encefalopathy == '?'] <- 'NA'
HCC_data$Ascites[HCC_data$Ascites == '?'] <- 'NA'

HCC_data$IR[HCC_data$IR == '?'] <- 'NA'
HCC_data$AFP[HCC_data$AFP == '?'] <- 'NA'
HCC_data$Hemaglobin[HCC_data$Hemaglobin == '?'] <- 'NA'
HCC_data$MCV[HCC_data$MCV == '?'] <- 'NA'

HCC_data$Leukocytes[HCC_data$Leukocytes == '?'] <- 'NA'
HCC_data$Platelets[HCC_data$Platelets == '?'] <- 'NA'
HCC_data$Albumi[HCC_data$Albumi == '?'] <- 'NA'
HCC_data$Total.Bil[HCC_data$Total.Bil == '?'] <- 'NA'

HCC_data$ALT[HCC_data$ALT == '?'] <- 'NA'
HCC_data$AST[HCC_data$AST == '?'] <- 'NA'
HCC_data$GGT[HCC_data$GGT == '?'] <- 'NA'
HCC_data$ALP[HCC_data$ALP == '?'] <- 'NA'

HCC_data$TP[HCC_data$TP == '?'] <- 'NA'
HCC_data$Creatinine[HCC_data$Creatinine == '?'] <- 'NA'
HCC_data$Number.of.nodules[HCC_data$Number.of.nodules == '?'] <- 'NA'
HCC_data$Major.Dimension[HCC_data$Major.Dimension == '?'] <- 'NA'

HCC_data$Dir..Bil[HCC_data$Dir..Bil == '?'] <- 'NA'
HCC_data$Iron[HCC_data$Iron == '?'] <- 'NA'
HCC_data$Sat[HCC_data$Sat == '?'] <- 'NA'
HCC_data$Ferritin[HCC_data$Ferritin == '?'] <- 'NA'
################  STEP 2.2 :-> ###############
##Unfactoring the variables for data imputation

HCC_data$Symptoms <- unfactor(HCC_data$Symptoms)
HCC_data$HBsAg <- unfactor(HCC_data$HBsAg)
HCC_data$HBeAg <- unfactor(HCC_data$HBeAg)
HCC_data$HBcAb <- unfactor(HCC_data$HBcAb)

HCC_data$HBVAb <- unfactor(HCC_data$HBVAb)
HCC_data$Endemic.Countries <- unfactor(HCC_data$Endemic.Countries)
HCC_data$Smoking <- unfactor(HCC_data$Smoking)
HCC_data$Diabetes <- unfactor(HCC_data$Diabetes)


HCC_data$Obesity <- unfactor(HCC_data$Obesity)
HCC_data$Hemochromatosis <- unfactor(HCC_data$Hemochromatosis)
HCC_data$AHT <- unfactor(HCC_data$AHT)
HCC_data$CRI <- unfactor(HCC_data$CRI)

HCC_data$HIV <- unfactor(HCC_data$HIV)
HCC_data$NASH <- unfactor(HCC_data$NASH)
HCC_data$Esophageal.varices <- unfactor(HCC_data$Esophageal.varices)
HCC_data$Splenomegaly <- unfactor(HCC_data$Splenomegaly)

HCC_data$Portal.hypertension <- unfactor(HCC_data$Portal.hypertension)
HCC_data$Portal.vein.thrombosis <- unfactor(HCC_data$Portal.vein.thrombosis)
HCC_data$Liver.metastasis <- unfactor(HCC_data$Liver.metastasis)
HCC_data$Radiological.hallmark <- unfactor(HCC_data$Radiological.hallmark)

HCC_data$Grams...day <- unfactor(HCC_data$Grams...day)
HCC_data$Packs...year <- unfactor(HCC_data$Packs...year)
HCC_data$Encefalopathy <- unfactor(HCC_data$Encefalopathy)
HCC_data$Ascites <- unfactor(HCC_data$Ascites)

HCC_data$IR <- unfactor(HCC_data$IR)
HCC_data$AFP <- unfactor(HCC_data$AFP)
HCC_data$Hemaglobin <- unfactor(HCC_data$Hemaglobin)
HCC_data$MCV <- unfactor(HCC_data$MCV)

HCC_data$Leukocytes <- unfactor(HCC_data$Leukocytes)
HCC_data$Platelets <- unfactor(HCC_data$Platelets)
HCC_data$Albumi <- unfactor(HCC_data$Albumi)
HCC_data$Total.Bil <- unfactor(HCC_data$Total.Bil)

HCC_data$ALT <- unfactor(HCC_data$ALT)
HCC_data$AST <- unfactor(HCC_data$AST)
HCC_data$GGT <- unfactor(HCC_data$GGT)
HCC_data$ALP <- unfactor(HCC_data$ALP)

HCC_data$TP <- unfactor(HCC_data$TP)
HCC_data$Creatinine <- unfactor(HCC_data$Creatinine)
HCC_data$Number.of.nodules <- unfactor(HCC_data$Number.of.nodules)
HCC_data$Major.Dimension <- unfactor(HCC_data$Major.Dimension)

HCC_data$Dir..Bil <- unfactor(HCC_data$Dir..Bil)
HCC_data$Iron <- unfactor(HCC_data$Iron)
HCC_data$Sat <- unfactor(HCC_data$Sat)
HCC_data$Ferritin <- unfactor(HCC_data$Ferritin)

#View(HCC_data)
str(HCC_data)
############### Step 2.3:->  To check the missing values
#TO SEE MISSING VALUES
##MISSING SUMMARY

missSummary <- function(HCC_data){
  VarName <- names(HCC_data)
  Observations <- sapply(HCC_data,function(x) length(x))
  missCount <-sapply(HCC_data,function(x) sum(is.na(x)))
  missPercent <-sapply(HCC_data,
                       function(x) paste(100*sum(is.na(x))/length(x),"%",sep="")
  )
  out.DF <- cbind(VarName,
                  Observations,
                  missCount,
                  missPercent)
  row.names(out.DF) <- NULL
  out.DF  
}
missings <- missSummary(HCC_data)
##summary display
missings
##Making a copy of the dataframe
HCC_data_copy <- HCC_data

str(HCC_data)

############################### Step 2.4 :- > #######################
###############Imputing data with Median and Mean
HCC_data$Symptoms[which(is.na(HCC_data$Symptoms))] <- median(HCC_data$Symptoms,na.rm=TRUE)
HCC_data$HBsAg[which(is.na(HCC_data$HBsAg))] <- median(HCC_data$HBsAg,na.rm=TRUE)
HCC_data$HBeAg[which(is.na(HCC_data$HBeAg))] <- median(HCC_data$HBeAg,na.rm=TRUE)
HCC_data$HBcAb[which(is.na(HCC_data$HBcAb))] <- median(HCC_data$HBcAb,na.rm=TRUE)

HCC_data$HBVAb[which(is.na(HCC_data$HBVAb))] <- median(HCC_data$HBVAb,na.rm=TRUE)
HCC_data$Endemic.Countries[which(is.na(HCC_data$Endemic.Countries))] <- median(HCC_data$Endemic.Countries,na.rm=TRUE)
HCC_data$Smoking[which(is.na(HCC_data$Smoking))] <- median(HCC_data$Smoking,na.rm=TRUE)
HCC_data$Diabetes[which(is.na(HCC_data$Diabetes))] <- median(HCC_data$Diabetes,na.rm=TRUE)

HCC_data$Obesity[which(is.na(HCC_data$Obesity))] <- median(HCC_data$Obesity,na.rm=TRUE)
HCC_data$Hemochromatosis[which(is.na(HCC_data$Hemochromatosis))] <- median(HCC_data$Hemochromatosis,na.rm=TRUE)
HCC_data$AHT[which(is.na(HCC_data$AHT))] <- median(HCC_data$AHT,na.rm=TRUE)
HCC_data$CRI[which(is.na(HCC_data$CRI))] <- median(HCC_data$CRI,na.rm=TRUE)

HCC_data$HIV[which(is.na(HCC_data$HIV))] <- median(HCC_data$HIV,na.rm=TRUE)
HCC_data$NASH[which(is.na(HCC_data$NASH))] <- median(HCC_data$NASH,na.rm=TRUE)
HCC_data$Esophageal.varices[which(is.na(HCC_data$Esophageal.varices))] <- median(HCC_data$Esophageal.varices,na.rm=TRUE)
HCC_data$Splenomegaly[which(is.na(HCC_data$Splenomegaly))] <- median(HCC_data$Splenomegaly,na.rm=TRUE)

HCC_data$Portal.hypertension[which(is.na(HCC_data$Portal.hypertension))] <- mean(HCC_data$Portal.hypertension,na.rm=TRUE)
HCC_data$Portal.vein.thrombosis[which(is.na(HCC_data$Portal.vein.thrombosis))] <- mean(HCC_data$Portal.vein.thrombosis,na.rm=TRUE)
HCC_data$Liver.metastasis[which(is.na(HCC_data$Liver.metastasis))] <- mean(HCC_data$Liver.metastasis,na.rm=TRUE)
HCC_data$Radiological.hallmark[which(is.na(HCC_data$Radiological.hallmark))] <- mean(HCC_data$Radiological.hallmark,na.rm=TRUE)


HCC_data$Grams...day[which(is.na(HCC_data$Grams...day))] <- mean(HCC_data$Grams...day,na.rm=TRUE)
HCC_data$Packs...year[which(is.na(HCC_data$Packs...year))] <- mean(HCC_data$Packs...year,na.rm=TRUE)
HCC_data$Encefalopathy[which(is.na(HCC_data$Encefalopathy))] <- mean(HCC_data$Encefalopathy,na.rm=TRUE)
HCC_data$Ascites[which(is.na(HCC_data$Ascites))] <- mean(HCC_data$Ascites,na.rm=TRUE)

HCC_data$IR[which(is.na(HCC_data$IR))] <- mean(HCC_data$IR,na.rm=TRUE)
HCC_data$AFP[which(is.na(HCC_data$AFP))] <- mean(HCC_data$AFP,na.rm=TRUE)
HCC_data$Hemaglobin[which(is.na(HCC_data$Hemaglobin))] <- mean(HCC_data$Hemaglobin,na.rm=TRUE)
HCC_data$MCV[which(is.na(HCC_data$MCV))] <- mean(HCC_data$MCV,na.rm=TRUE)

HCC_data$Leukocytes[which(is.na(HCC_data$Leukocytes))] <- mean(HCC_data$Leukocytes,na.rm=TRUE)
HCC_data$Platelets[which(is.na(HCC_data$Platelets))] <- mean(HCC_data$Platelets,na.rm=TRUE)
HCC_data$Albumi[which(is.na(HCC_data$Albumi))] <- mean(HCC_data$Albumi,na.rm=TRUE)
HCC_data$Total.Bil[which(is.na(HCC_data$Total.Bil))] <- mean(HCC_data$Total.Bil,na.rm=TRUE)

HCC_data$ALT[which(is.na(HCC_data$ALT))] <- mean(HCC_data$ALT,na.rm=TRUE)
HCC_data$AST[which(is.na(HCC_data$AST))] <- mean(HCC_data$AST,na.rm=TRUE)
HCC_data$GGT[which(is.na(HCC_data$GGT))] <- mean(HCC_data$GGT,na.rm=TRUE)
HCC_data$ALP[which(is.na(HCC_data$ALP))] <- mean(HCC_data$ALP,na.rm=TRUE)

HCC_data$TP[which(is.na(HCC_data$TP))] <- mean(HCC_data$TP,na.rm=TRUE)
HCC_data$Creatinine[which(is.na(HCC_data$Creatinine))] <- mean(HCC_data$Creatinine,na.rm=TRUE)
HCC_data$Number.of.nodules[which(is.na(HCC_data$Number.of.nodules))] <- mean(HCC_data$Number.of.nodules,na.rm=TRUE)
HCC_data$Major.Dimension[which(is.na(HCC_data$Major.Dimension))] <- mean(HCC_data$Major.Dimension,na.rm=TRUE)
HCC_data$ALP[which(is.na(HCC_data$ALP))] <- mean(HCC_data$ALP,na.rm=TRUE)

HCC_data$Dir..Bil[which(is.na(HCC_data$Dir..Bil))] <- mean(HCC_data$Dir..Bil,na.rm=TRUE)
HCC_data$Iron[which(is.na(HCC_data$Iron))] <- mean(HCC_data$Iron,na.rm=TRUE)
HCC_data$Sat[which(is.na(HCC_data$Sat))] <- mean(HCC_data$Sat,na.rm=TRUE)
HCC_data$Ferritin[which(is.na(HCC_data$Ferritin))] <- mean(HCC_data$Ferritin,na.rm=TRUE)

#######################  Step 2.5 :- >       #############################
#########classifying " Class..1.Year.Survival. " into factors
a<-length(HCC_data[,50])
a
for (i in c(1:a))
{
  if(HCC_data[i,50] == 1) { HCC_data[i,50]<-'lives'}
  else (HCC_data[i,50]<-'dies')
}

levels(HCC_data$Class..1.Year.Survival.)
#converting into FACTORS
HCC_data$Class..1.Year.Survival. <- factor(HCC_data$Class..1.Year.Survival.)
levels(HCC_data$Class..1.Year.Survival.)
#head(HCC_data)
is.factor(HCC_data$Class..1.Year.Survival.)
str(HCC_data)
##Back up of the imputed data frame
HCC_data_copy_imputed <- HCC_data
HCC_data_copy_imputed_1 <- HCC_data

##to view the data
#View(HCC_data)

########################  Step 2.6  : ->     #################### 
########Data Pre-processing (including Data Imputation, Normalization / Standardization)
# rescaling features using min-max normalization and Z score
#This process transforms a feature such that all of its values fall in a range between 0
#and 1. 
#The formula for normalizing a feature is as below:
#the formula subtracts the minimum of feature X from each value and
#divides by the range of X.

###Checking for 'NA'
table(is.na(HCC_data))
################ Step 2.7 #################
######  Feature Scaling using Normalization
##below is the Normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

### Implementing the Normalization function using lapply over the whole dataframe
HCC_data[-50] <- as.data.frame(lapply(HCC_data[-50], normalize))
##taking copy
HCC_data_2<-HCC_data
HCC_data_3<-HCC_data
HCC_data_3<-HCC_data

## percentage form
round(prop.table(table(HCC_data$Class..1.Year.Survival.)) * 100, digits = 1)

head(HCC_data)
str(HCC_data)

##Splitting of the data (training and test set)
### Starting - STEP 3: -> Splitting of the data (training and test set)###################################################

split = sample.split(HCC_data$Class..1.Year.Survival., SplitRatio = 0.75)
my_training_data = subset(HCC_data, split == TRUE)
my_test_data = subset(HCC_data, split == FALSE)
table(is.na(my_training_data))
table(is.na(my_test_data))



###################   Selection of the machine learning techniques Model Development #############################
#############  Step 4: - >  ###################################
##Model : KNN 
y_pred_data = knn(train = my_training_data[,-50],test = my_test_data[,-50],cl = my_training_data[,50],k = 13)

y_pred_data


#Here The knn() function returns a factor vector of predicted labels for each of the
#examples in the test dataset, which has been assigned to y_pred_data.
#The next step of the process in KNN is to evaluate how well the predicted classes in the y_pred_data
# vector match up with the known values in the my_test_data[,50] dataframe
#CrossTable()

###### Step 5 : - > Performance Measurement  ################################## 
##creating confusion matrix
CrossTable(x = my_test_data[,50], y = y_pred_data,prop.chisq=FALSE)
##Measure of Performance:
#The sensitivity of a model (also called the true positive rate) measures the
#proportion of positive examples that were correctly classified.
#sensitivity=TP/TP+FN
sensitivity <- (22)/(22+5)
sensitivity  ##0.8148148

#The specificity of a model (also called the true negative rate) measures the
#proportion of negative examples that were correctly classified.
#Specificity = TN/TN+FP
Specificity <- (4)/(4+11)
Specificity   ##0.2666667

#We can use the confusion matrix to obtain the accuracy and error rate. Since
#the accuracy is (TP + TN) / (TP + TN + FP + FN)
Accuracy <-(5+22)/(5+22+11+4)
Accuracy    ## 64 % is the accuracy

##Improving model performance
#First, we need
# an alternative method for rescaling our numeric features. Second, we
#will try several different values for k.
##increasing the performance of the Model:

#############  Step : -> 5.1  ##############################
## Performing the Z scale standardization for the better performance

#using dataframe from above HCC_data_copy_imputed_1
#HCC_data[-50] = scale(HCC_data[-50])
HCC_data_copy_imputed_1[-50] = scale(HCC_data_copy_imputed_1[-50])

#####data split

split = sample.split(HCC_data_copy_imputed_1$Class..1.Year.Survival., SplitRatio = 0.75)
my_training_data = subset(HCC_data_copy_imputed_1, split == TRUE)
my_test_data = subset(HCC_data_copy_imputed_1, split == FALSE)
################ Step 5.2 ###########
##creating the Model  and checking the Model performance 

y_pred_data = knn(train = my_training_data[,-50],test = my_test_data[,-50],cl = my_training_data[,50],k = 13)

y_pred_data

###### Performance Measurement
##confusion matrix

CrossTable(x = my_test_data[,50], y = y_pred_data,prop.chisq=FALSE)
##Measure of Perormance:
#sensitivity=TP/TP+FN
sensitivity <- (24)/(24+5)
sensitivity  ##82 %
#Specificity = TN/TN+FP
Specificity <- (2)/(2+11)
Specificity   # 15 %
# the accuracy and error rate. Since
#the accuracy is (TP + TN) / (TP + TN + FP + FN)
Accuracy <-(24+5)/(5+11+2+24)
Accuracy    ## 69 % is the accuracy
###############   STEP 6 : -> ################
####Now we are going to check the Accuracy of the prediction using SVM Model
#################SVM  ################
#SVMs use a boundary called a hyperplane to partition data into
#groups of similar class values
#In two dimensions, the task of the SVM algorithm is to identify a line that separates
#the two classes
#SVM uses Maximum Margin Hyperplane
#(MMH) that creates the greatest separation between the two classes.

###### $$$ Pre requisite execute step 1 to step 2.7   $$$$
HCC_data[-50] = scale(HCC_data_copy_imputed[-50])

######Data Split into Training, Testing and Validation datasets
split = sample.split(HCC_data$Class..1.Year.Survival., SplitRatio = 0.75)
my_training_data = subset(HCC_data, split == TRUE)
my_test_data = subset(HCC_data, split == FALSE)

######Model Development

HCC_classifier <- ksvm(Class..1.Year.Survival. ~ ., data = my_training_data,kernel = "vanilladot")
HCC_classifier


#Now Going  to examine the performance on the testing dataset to know whether
#it generalizes well to unseen data

#The predict() function is uses survival classification model to make
#predictions on the testing dataset:

HCC_predictions <- predict(HCC_classifier, my_test_data)

head(HCC_predictions)


#To examine how well our classifier performed, we need to compare the predicted
# to the true Survival in the testing dataset. We'll use the table() function for this

#############  Step 6.1 : - ###############################
conf_matrix <-table(HCC_predictions, my_test_data$Class..1.Year.Survival.)
conf_matrix


HCC_agree <- HCC_predictions == my_test_data$Class..1.Year.Survival.
table(HCC_agree)
prop.table(table(HCC_agree))


##########     Step 7: ->   ############################################
###IMPROVE THE ACCURACY using kernel as Radial


HCC_classifier = svm(formula = Class..1.Year.Survival. ~ .,
                 data = my_training_data,
                 type = 'C-classification',
                 kernel = 'radial')

#####predictions on the testing dataset:

HCC_predictions <- predict(HCC_classifier, my_test_data)

head(HCC_predictions)

#########  Step 8: -> #####################
##Performance Measurement
##Confusion Matrix
table(HCC_predictions, my_test_data$Class..1.Year.Survival.)

HCC_agree <- HCC_predictions == my_test_data$Class..1.Year.Survival.
table(HCC_agree)
##True prediction in % form

prop.table(table(HCC_agree))

###Now here we have increased the Accuracy  by multiple folds till 78%

########################################################################################################################

