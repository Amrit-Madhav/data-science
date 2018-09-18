##PART 1##Setting working directory
library(rpart)
install.packages("caret")
library(caret)
install.packages("caTools")
library(caTools)
install.packages("Amelia")
library(Amelia)
install.packages("ROCR")
install.packages("DT")
install.packages("dplyr", dependencies = TRUE)
install.packages('caTools')
install.packages("gmodels")
install.packages("kernlab")
install.packages('e1071')
install.packages("neuralnet")
library(GGally)
library(stats)
library(lmtest)
library(tseries)
library(gvlma)
library(varhandle)
library(mctest)
install.packages("tseries")
install.packages('e1071')
library(e1071)
library(neuralnet)
library(nnet)
library(caTools)
library(e1071)
library(kernlab)
library(gmodels)
library(class)
library(DT)
library(dplyr)
library("ROCR")
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
library(mtcars)
install.packages("mtcars")
library(MASS)
setwd("D:/AMRIT/PGCBAMD/courses/ML/assignment2")

##STEP 1: Loading the data 

telecom_cust_data1 <- read.csv("Telecom Customer Data.csv", header =  TRUE)
##removing customer_id
telecom_cust_data <- telecom_cust_data1[-1]
######Check column variables
str(telecom_cust_data)
## To check basic statistical information of each variable
summary(telecom_cust_data) 
####  Checking for Missing Data

is.na(telecom_cust_data) ## Displays True for a missing value
print(dim(telecom_cust_data))  ##7043 rows and 21 columns
#####To check missing value #########
#View(telecom_cust_data)
############### Step 2:->  To check the missing values
#TO SEE MISSING VALUES
##MISSING SUMMARY

missSummary <- function(telecom_cust_data){
  VarName <- names(telecom_cust_data)
  Observations <- sapply(telecom_cust_data,function(x) length(x))
  missCount <-sapply(telecom_cust_data,function(x) sum(is.na(x)))
  missPercent <-sapply(telecom_cust_data,
                       function(x) paste(100*sum(is.na(x))/length(x),"%",sep="")
  )
  out.DF <- cbind(VarName,
                  Observations,
                  missCount,
                  missPercent)
  row.names(out.DF) <- NULL
  out.DF  
}
missings <- missSummary(telecom_cust_data)
##summary display
missings
##Making a copy of the dataframe
telecom_cust_data_copy <- telecom_cust_data

##to see data in a graphical way
##To see missing data in a graphical way

missmap(telecom_cust_data,main="Data Missing - Telecom Sales", col=c("red","grey"),legend=FALSE)

## Red colour stripes is visible for columns Total Charges

###Checking for 'NA'
table(is.na(telecom_cust_data))

print(dim(telecom_cust_data))   ##7403 rows and 21 columns

##Step 3: -
##Unfactoring the variables for data imputation
telecom_cust_data$gender <- unfactor(telecom_cust_data$gender)
telecom_cust_data$Partner <- unfactor(telecom_cust_data$Partner)
telecom_cust_data$Dependents <- unfactor(telecom_cust_data$Dependents)
telecom_cust_data$PhoneService <- unfactor(telecom_cust_data$PhoneService)
telecom_cust_data$MultipleLines <- unfactor(telecom_cust_data$MultipleLines)
telecom_cust_data$InternetService <- unfactor(telecom_cust_data$InternetService)
telecom_cust_data$OnlineSecurity <- unfactor(telecom_cust_data$OnlineSecurity)
telecom_cust_data$OnlineBackup <- unfactor(telecom_cust_data$OnlineBackup)
telecom_cust_data$DeviceProtection <- unfactor(telecom_cust_data$DeviceProtection)
telecom_cust_data$TechSupport <- unfactor(telecom_cust_data$TechSupport)
telecom_cust_data$StreamingTV <- unfactor(telecom_cust_data$StreamingTV)
telecom_cust_data$StreamingMovies <- unfactor(telecom_cust_data$StreamingMovies)
telecom_cust_data$Contract <- unfactor(telecom_cust_data$Contract)
telecom_cust_data$PaperlessBilling <- unfactor(telecom_cust_data$PaperlessBilling)
telecom_cust_data$PaymentMethod <- unfactor(telecom_cust_data$PaymentMethod)
telecom_cust_data$Churn <- unfactor(telecom_cust_data$Churn)
str(telecom_cust_data)

############# Data Imputation

telecom_cust_data$MultipleLines[telecom_cust_data$MultipleLines=="No phone service"]<-"No"
telecom_cust_data$OnlineBackup[telecom_cust_data$OnlineSecurity=="No internet service"]<-"No"
telecom_cust_data$OnlineBackup[telecom_cust_data$OnlineBackup=="No internet service"]<-"No"
telecom_cust_data$DeviceProtection[telecom_cust_data$DeviceProtection=="No internet service"]<-"No"
telecom_cust_data$TechSupport[telecom_cust_data$TechSupport=="No internet service"]<-"No"

#for (i in c(1:a))
#{
 # if(telecom_cust_data$gender == 'Male') 
  #  { telecom_cust_data$gender = 1}
  #else (telecom_cust_data$gender = 0)
#}
##Segregating variables status into Binary form having  Yes=1 No= 0:-

telecom_cust_data$gender<-ifelse(telecom_cust_data$gender=="Male",1,0)

telecom_cust_data$Partner<-ifelse(telecom_cust_data$Partner=="Yes",1,0)
telecom_cust_data$Dependents<-ifelse(telecom_cust_data$Dependents=="Yes",1,0)
telecom_cust_data$PhoneService<-ifelse(telecom_cust_data$PhoneService=="Yes",1,0)
telecom_cust_data$MultipleLines<-ifelse(telecom_cust_data$MultipleLines=="Yes",1,0)
telecom_cust_data$OnlineSecurity<-ifelse(telecom_cust_data$OnlineSecurity=="Yes",1,0)
telecom_cust_data$OnlineBackup<-ifelse(telecom_cust_data$OnlineBackup=="Yes",1,0)
telecom_cust_data$DeviceProtection<-ifelse(telecom_cust_data$DeviceProtection=="Yes",1,0)
telecom_cust_data$TechSupport<-ifelse(telecom_cust_data$TechSupport=="Yes",1,0)
telecom_cust_data$StreamingMovies<-ifelse(telecom_cust_data$StreamingMovies=="Yes",1,0)
telecom_cust_data$StreamingTV<-ifelse(telecom_cust_data$StreamingTV=="Yes",1,0)
telecom_cust_data$PaperlessBilling<-ifelse(telecom_cust_data$PaperlessBilling=="Yes",1,0)
#######churning classification :
telecom_cust_data$Churn<-ifelse(telecom_cust_data$Churn =='Yes',1,0)  ##left = 1 not left = 0
######## Imputing the data with Numbers

telecom_cust_data$PaymentMethod <-ifelse(telecom_cust_data$PaymentMethod=="Electronic check",1,ifelse(telecom_cust_data$PaymentMethod=="Mailed check",2,0))
telecom_cust_data$InternetService<-ifelse(telecom_cust_data$InternetService=="DSL",1,ifelse(telecom_cust_data$InternetService =="Fiber optic",2,0))
telecom_cust_data$Contract<-ifelse(telecom_cust_data$Contract=="Month-to-month",1,ifelse(telecom_cust_data$Contract =="One year",2,0))


#######################
##Imputing data with mean for variables TotalCharges and MonthlyCharges

telecom_cust_data$TotalCharges[which(is.na(telecom_cust_data$TotalCharges))] <- mean(telecom_cust_data$TotalCharges,na.rm=TRUE)

telecom_cust_data$MonthlyCharges[which(is.na(telecom_cust_data$MonthlyCharges))] <- mean(telecom_cust_data$MonthlyCharges,na.rm=TRUE)

head(telecom_cust_data)

#### Step 4 :- 
set.seed(200)
#####Training and testing split############

#Rows selection for training data set in the ratio of 70:30 here prob=0.7
# Splitting the dataset into the Training set and Test set

set.seed(123)
split = sample.split(telecom_cust_data$Churn, SplitRatio = 0.7)
my_training_data = subset(telecom_cust_data, split == TRUE)
my_test_data = subset(telecom_cust_data, split == FALSE)

#########Step 5 Artificial Neural Network  Model 1
###ANN

#nn = neuralnet(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges
 #              ,data=my_training_data,hidden=10,err.fct="ce",linear.output=FALSE)


concrete_model <- neuralnet(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+MonthlyCharges+TotalCharges,data = my_training_data)

concrete_model
plot(concrete_model)
concrete_model$net.result   ### overall result i.e output for each replication
concrete_model$weights      ### weights of each neuron at each layers
concrete_model$result.matrix
concrete_model$net.result[[1]] ##here number between 0 and 1 which are outcomes of the logistics function
nn1 = ifelse(concrete_model$net.result[[1]]>0.5,1,0)
nn1               ## COMAPARING WITH 50% CHANCES OF THE Occurnce as per the logistics function
                   ## Outcomes with the probability that we observed

misclassificationError = mean(my_training_data$Churn != nn1)  
misclassificationError   ## how predicted value is different from the real
                         ## 19.2% of the error
##missclassification ie how namny case which were different from real outputs
OutputvsPred = cbind(my_training_data$Churn,nn1)
OutputvsPred             #to see actual outcome and predicted value side by side

                          # in 795 observation in reality case was  customer remained but model predicted that the customer left

##Using Backpropagation Algorithm and Palying
#with Learning rate and Entropy

nn.bp = neuralnet(formula = Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+MonthlyCharges+TotalCharges,
                  data = my_training_data,hidden=2,learningrate = 0.01,
                  algorithm = "backprop",err.fct = "ce",linear.output = FALSE)

### STEP 6 : Checking the output classfier
#One way we can check the output of the classifier is the confusion matrix.
#Test the resulting output

temp_test <- subset(my_test_data, select = c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure", "PhoneService", "MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV","StreamingMovies","Contract","PaperlessBilling","MonthlyCharges","TotalCharges"))

head(temp_test)
nn.results <- compute(concrete_model, temp_test)

#Accuracy
results <- data.frame(actual = my_test_data$Churn, prediction = nn.results$net.result)
results

roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

conf_matrix<-table(actual,prediction)
conf_matrix
library(caret)
##Measure of Perormance:
#sensitivity=TP/TP+FN
sensitivity(conf_matrix)
specificity(conf_matrix)
#We can use the confusion matrix to obtain the accuracy and error rate. Since
#the accuracy is (TP + TN) / (TP + TN + FP + FN)
Accuracy <-(1865+371)/(1865+371+377+205)
Accuracy    ## 69 % is the accuracy

####


######STEP 6  : - SVM Model Development
######Model Development


telecom_classifier <- ksvm(Churn ~ ., data = my_training_data,kernel = "vanilladot")
telecom_classifier


#Now Going  to examine the performance on the testing dataset to know whether
#it generalizes well to unseen data

#The predict() function is uses Customer retention classification model to make
#predictions on the testing dataset:

telecom_predictions <- predict(telecom_classifier, my_test_data)

head(telecom_predictions)


#To examine how well our classifier performed, i need to compare the predicted
# to the true Survival in the testing dataset. i will use the table() function for this

#############  Step 6.1: - ###############################
conf_matrix <-table(telecom_predictions, my_test_data$Churn)
conf_matrix


telecom_agree <- telecom_predictions == my_test_data$Churn
table(telecom_agree)
prop.table(table(telecom_agree))


##########     Step 7: ->   ############################################
###IMPROVE THE ACCURACY using kernel as Radial

telecom_classifier = svm(formula = Churn ~ .,
                     data = my_training_data,
                     type = 'C-classification',
                     kernel = 'radial')




#####predictions on the testing dataset:

telecom_predictions <- predict(telecom_classifier, my_test_data)

head(telecom_predictions)

#########  Step 8: -> #####################
##Performance Measurement
##Confusion Matrix
conf_matrix1<-table(telecom_predictions, my_test_data$Churn)
conf_matrix1
specificity(conf_matrix1)
sensitivity(conf_matrix1)
Accuracy = (1899+333)/(1899+333+171+415)
Accuracy

telecom_agree <- telecom_predictions == my_test_data$Churn
table(telecom_agree)
##True prediction in % form

prop.table(table(telecom_agree))

###Now here we have the Accuracy till 79%

########################################################################################################################

##############step 8
##sensitivity analysis for both the models
##Checking the Sensitivity of the SVM model
telecom_classifier = svm(formula = Churn ~ gender+Dependents+tenure+PhoneService+InternetService+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+MonthlyCharges+TotalCharges,
                         data = my_training_data,
                         type = 'C-classification',
                         kernel = 'radial')

Accuracy = (1414+241)/(1414+241+320+138)
Accuracy

##Checking sensitivity of the AMM Model:-

concrete_model <- neuralnet(Churn ~ gender+SeniorCitizen+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+MonthlyCharges+TotalCharges,data = my_training_data)