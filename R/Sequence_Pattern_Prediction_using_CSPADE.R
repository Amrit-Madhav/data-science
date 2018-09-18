##############  Pattern sequence Mining  ################
##Installing Packages
install.packages("arulesSequences")
install.packages("plyr", dependencies = TRUE)
install.packages("arulesSequences", dependencies = TRUE)
install.packages("colorspace", dependencies = TRUE)
install.packages("tibble", dependencies = TRUE)
library(arulesViz)
library(plyr)
library(colorspace)
library(arulesSequences)
library(tibble)
##Setting working directory
setwd("D:/AMRIT/PGCBAMD/courses/Data_mining")
#Creating the data frame
sales_data <- read.csv("assodata_1.csv", header =  TRUE)

##Time and Customer
sales_data["CUSTOMER"]<-sales_data["CUSTOMER"]+1
sales_data["TIME"]<-sales_data["TIME"]+1

head(sales_data)
glimpse(sales_data)
names(sales_data)
####Create the transaction data with temporal information:

sales_data$sequence <- as.numeric(sales_data$CUSTOMER)
sales_data <- sales_data[order(sales_data$sequence, sales_data$TIME),]
tran_data<-data.frame(item=sales_data$PRODUCT)

#I have used (as) function used to transform the list data into a transaction dataset.
#After that added eventID and sequenceID as temporal information.

sales_data.tran<-as(tran_data,"transactions")
transactionInfo(sales_data.tran)$sequenceID <- sales_data$sequence
transactionInfo(sales_data.tran)$eventID<-sales_data$TIME
sales_data.tran
##inspect function to inspect the transactions:

inspect(head(sales_data.tran))
##obtain summary information about the transactions with temporal information

summary(sales_data.tran)

#####################
# Read transaction data in basket format example as per zaki inbuild file:-shows the demo sequence from a dummy file already available
zaki=read_baskets(con = system.file("misc", "zaki.txt", package = "arulesSequences"), info = c("sequenceID","eventID","SIZE"))
as(zaki, "data.frame")
######################

##  Mining frequent sequential patterns with cSPADE
frequent_pattern <-cspade(sales_data.tran,parameter = list(support = 0.045, maxgap = 5), control = list(verbose = TRUE))
inspect(frequent_pattern)
summary(frequent_pattern)
pattern<-as(frequent_pattern, "data.frame")
##Below is the filteration in order to look the rule 112, in order to understand the sequence pattern in the transactions of the customer
pattern[112,]
##with label name
sequences_df <- cbind(frequent_pattern = labels(frequent_pattern), support = frequent_pattern@quality)
sequences_df[112,]
as(frequent_pattern, "data.frame")

##########################################