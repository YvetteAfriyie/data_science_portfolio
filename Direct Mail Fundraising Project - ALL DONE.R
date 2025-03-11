#Install and load required packages into R
#install.packages("dplyr")
#install.packages("DataExplorer")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("Boruta")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("class")
#install.packages("neuralnet")
#install.packages("glmnet")

library(glmnet)
library(dplyr)
library(DataExplorer)
library(ggplot2)
library(Boruta)
library(randomForest)
library(caret)
library(class)
library(neuralnet)
library(corrplot)


#Obtain data into R
fund <- read.csv("Fundraising.csv", header = TRUE)

#Data Description
head(fund)
t(t(names(fund)))
str(fund)
dim(fund)
summary(fund)

#Count missing variables and duplicated values
colSums(is.na(fund))
is.null(fund)
missingness <- complete.cases(fund)
unique(missingness)
(sum(duplicated(fund))) 

#Add zipconvert_1 to dataset
fund$zipconvert_1 <- as.integer(ifelse(fund$zipconvert_2 + fund$zipconvert_3 + fund$zipconvert_4 + fund$zipconvert_5 == 1,0,1))

#Combine 5 zipconverts into one variable
fund$zipconvert <- case_when(fund$zipconvert_1 == 1 ~ "zc_1" , 
                             fund$zipconvert_2 == 1~ "zc_2",
                             fund$zipconvert_3 == 1 ~ "zc_3",
                             fund$zipconvert_4 == 1 ~ "zc_4",
                             fund$zipconvert_5 == 1 ~ "zc_5"
                              )


##To find the various zipcodes in the zipconvert variable
unique(fund$zipconvert)


#Average dollar / mean donation amount of donations ~ $13 DOLLARS
mean(fund$TARGET_D[fund$TARGET_B==1])

#Net profit of mail was sent to everyone
Netprofitresponders <- mean(fund$TARGET_D[fund$TARGET_B==1] - 0.68)*0.051
Netprofitnonresponders <- mean(fund$TARGET_D[fund$TARGET_B==0] - 0.68)*(1-0.051)
Netprofitresponders
Netprofitnonresponders
Total = Netprofitresponders + Netprofitnonresponders
Total

#Explore, Clean and preprocess data
#Remove unwanted variables from dataset ( All initial zipcodes , Row_ IDs and Target_D)
fund <- fund[,-c(1,2,3,4,5,6,24,25)]
dim(fund)
head(fund)
str(fund)
t(t(names(fund)))
summary(fund)


#Change binary integer values into factors
fund$TARGET_B <- as.factor(fund$TARGET_B)
fund$gender.dummy <- as.factor(fund$gender.dummy)
fund$homeowner.dummy <- as.factor(fund$homeowner.dummy)
fund$zipconvert <- as.factor(fund$zipconvert)

###############Find correlations, also All predictors have been converted into numerical values to calculate correlation and variance values.
fund_num <- fund
fund_num <- as.data.frame(sapply(fund_num, as.numeric))
str(fund_num)
corrplot(round(cor(fund_num),1), method = "number")
corrplot(cor(fund_num))

#Convert 0 and 1 in variables to have a better meaning for visualizations 
fund$gender.dummy <- as.factor(ifelse(fund$gender.dummy =="1", 'Female','Male'))
fund$TARGET_B <- as.factor(ifelse(fund$TARGET_B =="1", 'Donor','Non- Donor'))
fund$homeowner.dummy <- as.factor(ifelse(fund$homeowner.dummy =="1", 'homeownwer','non-homeownwer'))






#######################univariate analysis of Attributes#################################################################################################


#check table and check classes distribution in Target Variable
table(fund$TARGET_B)
prop.table(table(fund$TARGET_B))
ggplot(fund, aes(x = TARGET_B, fill = (TARGET_B))) + 
  geom_bar() +
  scale_fill_manual( values = c ("cyan3","plum2")) +
  theme(panel.background = element_blank())+
  labs(title = "Barplot of Dependent Variable(TARGET_B)", x = "DONORSHIP", y = "Frequency")


#Gender 
prop.table(table(fund$gender.dummy))
table(fund$gender.dummy)
ggplot(fund, aes(x = gender.dummy, fill = (gender.dummy))) + 
  geom_bar() +
  geom_text(aes(label=..count..), stat ="count", vjust =1.5,colour = "black")+
  scale_fill_manual(values =c("hotpink","lightblue")) +
  theme(panel.background = element_blank())+
  labs(title = "Barplot of Gender of Members", x = "Gender", y = "Frequency")

#Homeowner 
table(fund$homeowner.dummy)
ggplot(fund, aes(x = homeowner.dummy, fill = (homeowner.dummy))) + 
  geom_bar() +
  geom_text(aes(label=..count..), stat ="count", vjust =1.5,colour = "black")+
  scale_fill_manual(values =c("aquamarine3","lightblue")) +
  theme(panel.background = element_blank())+
  labs(title = "Barplot of Homeownership", x = "Homeownership", y = "Frequency")


#Income 
count_Income <- table(fund$INCOME)
count_Income
fund$INCOME <- as.factor(fund$INCOME)
ggplot(fund, aes(x = INCOME,fill = (INCOME))) + 
  geom_bar() +
  scale_fill_hue(c = 60) +
  geom_text(aes(label=..count..), position = position_dodge(width=0.9), vjust =-0.25,stat ="count",colour = "black")+
  theme(panel.background = element_blank())+
  labs(title = "Barplot of INCOME of Members", x = "INCOME", y = "Frequency")


#Wealth
fund$WEALTH <- as.factor(fund$WEALTH)
ggplot(fund, aes(x = WEALTH, fill = (WEALTH))) + 
  geom_bar() +
  scale_fill_hue(c = 60) +
  geom_text(aes(label=..count..), position = position_dodge(width=0.9), vjust =-0.25,stat ="count",colour = "black")+
  theme(panel.background = element_blank())+
    labs(title = "Barplot of Wealth of Members", x = "Wealth", y = "Frequency")


# Number of Children
fund$NUMCHLD <- as.factor(fund$NUMCHLD)
ggplot(fund, aes(x = NUMCHLD, fill = (NUMCHLD))) + 
  geom_bar(BIM = 1) +
  geom_text(aes(label=..count..), position = position_dodge(width=0.9), vjust =-0.25,stat ="count",colour = "black")+
  scale_fill_hue(c = 100)+
  theme(panel.background = element_blank())+
  labs(title = "Barplot of Number of Children of Members", x = "Number of Children", y = "Frequency")

#zipcodes 
ggplot(fund, aes(x = zipconvert, fill = (zipconvert))) + 
  geom_bar() +
  geom_text(aes(label=..count..), position = position_dodge(width=0.9), vjust =-0.25,stat ="count",colour = "black")+
  scale_fill_hue(c = 100)+
  theme(panel.background = element_blank())+
  labs(title = "Barplot of Various zipcodes", x = "zipcodes", y = "Frequency")


# Histogram of HV
ggplot(fund, aes(HV)) +
  geom_density(color="darkblue", fill="lightblue")+
  theme(panel.background = element_blank())+
  ggtitle("Distribution of Average Home Value")


ggplot(fund, aes(x = HV,color = "chartreuse" , fill = HV)) + 
  geom_histogram(binwidth = 50) +
  labs(title = "Histogram of HV", x = "Home Value", y = "Frequency")

#Histogram of ICmed
ggplot(fund, aes(Icmed)) +
  geom_density(color="red", fill="lightblue")+
  theme(panel.background = element_blank())+
  ggtitle("Distribution of Median Household Income Value")

ggplot(fund, aes(x = Icmed, fill = "chartreuse")) + 
  geom_histogram(binwidth =10) +
  theme(panel.background = element_blank())+
  labs(title = "Histogram of Median income in Neighborhood", x = "Median Household Income", y = "Frequency")

#Histogram of ICavg
ggplot(fund, aes(Icavg)) +
  theme(panel.background = element_blank())+
  geom_density(color="darkblue", fill="lightblue")+
  labs(title = "density plot 0f Average income in Neighborhood", x = "Average income in Neighborhood", y = "Frequency")

ggplot(fund, aes(x = Icavg, fill = "chartreuse")) + 
  geom_histogram(binwidth =10) +
  theme(panel.background = element_blank())+
  labs(title = "Histogram of Average income in Neighborhood", x = "Average Household Income", y = "Frequency")

#Histogram of IC15
ggplot(fund, aes(IC15)) +
  geom_density(color="darkblue", fill="lightblue")+
  theme(panel.background = element_blank())+
  ggtitle("Distribution of % earning <15k") 


ggplot(fund, aes(x = IC15, fill = "chartreuse")) + 
  geom_histogram(binwidth =0.5) +
  theme(panel.background = element_blank())+
  labs(title = "Histogram of% earning <15k", x = "% earning less than 15k", y = "Frequency")
  
#Histogram of NUMPROM
ggplot(fund, aes(NUMPROM)) +
  geom_density( fill="lightblue")+
  theme(panel.background = element_blank())+
  labs(title = "density plot of number of promotions", x = "Number of promotions", y = "Frequency")


ggplot(fund, aes(x = NUMPROM, fill = "chartreuse")) + 
  geom_histogram(binwidth =0.5) +
  theme(panel.background = element_blank())+
  theme(panel.background = element_blank())+
  labs(title = "Histogram of% earning <15k", x = "% earning less than 15k", y = "Frequency")



#Histogram of TIMELAG
ggplot(fund, aes(TIMELAG)) +
  theme(panel.background = element_blank())+
  geom_density(color="darkblue", fill="lightblue")+
  theme(panel.background = element_blank())+
  labs(title = "Density plot of Number of months between first and second gift", x = "Number of months between first and second gift", y = "Frequency")

ggplot(fund, aes(x = TIMELAG, )) + 
  geom_histogram(binwidth = 2 , fill = "red") +
  theme(panel.background = element_blank())+
  labs(title = "Histogram of TimeLag", x = "No of months btwn 1 and 2 donation", y = "Frequency")

str(fund)
table(fund$RAMNTALL)

#Histogram of RAMNTALL - Histogram is not a good way to view this variable
ggplot(fund, aes(RAMNTALL)) +
  geom_density(color="darkblue", fill="lightblue")+
  theme(panel.background = element_blank())+
  labs(title = "Histogram of RAMNTALL", x = "Dollar amount of lifetime gift to date", y = "Frequency")
#MAXRAMNT

ggplot(fund, aes(MAXRAMNT )) +
  geom_density(color="darkblue", fill="lightblue")+
  theme(panel.background = element_blank())+
  labs(title = "Density Plot of MAXRAMNT", x = "LARGEST amount of  gift to date", y = "Frequency")


#totalmonths

ggplot(fund, aes(totalmonths )) +
  geom_density(color="darkblue", fill="lightblue")+
  theme(panel.background = element_blank())+
  labs(title = "Density Plot of total months", x = "Number of Months from the last donation to July 1998( the last time the case was updated", y = "Frequency")



table(boxplot.stats(fund$MAXRAMNT)$out)

unique(fund$MAXRAMNT)
table(fund$MAXRAMNT)




#plot histograms 
plot_histogram(fund, theme_config = list("strip.background" = element_rect(fill = "lightblue"),"aspect.ratio" = 1/3), ggtheme = theme_bw())
 
####################MULTIVARIATE ANALYSIS###################################################################################################################

# Create a table of frequencies for the WEALTH and INCOME columns
table_data <- table(fund$WEALTH, fund$INCOME)
table_data2 <- table(fund$zipconvert, fund$WEALTH)

# Create a heatmap of the table using ggplot
ggplot(data = as.data.frame.table(table_data), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Heatmap of Wealth and Income", x = "Wealth Level", y = "Income Level", fill = "Frequency")

#Used scale_fill_gradient to create a gradient color scale for the fill colors, with white indicating low frequencies and blue indicating high frequencies.
ggplot(data = as.data.frame.table(table_data2), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Heatmap of Wealth and NUMCHLD", x = "ZIPCODE", y = "WEALTH", fill = "Frequency")


ggplot2::ggplot(fund, aes( x = zipconvert, y =  TARGET_B, fill = TARGET_B )) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 40) 
 
fund$zipconvert <- as.factor(fund$zipconvert)

fund$TARGET_B <- as.factor(fund$TARGET_B)
ggplot(fund, aes(x = zipconvert, fill = TARGET_B)) + 
  geom_bar() +
  scale_fill_manual(values = c("coral", "blue")) +
  theme(panel.background = element_blank())+
  labs(title = "TARGET against zipcode", x = "Zipcode", y = "Frequency")

  

ggplot2::ggplot(fund,mapping = aes( x =homeowner.dummy, y = TARGET_B , fill = TARGET_B )) + 
  geom_bar(stat = "identity") +
  scale_fill_manual( values = c("coral","grey"))+
  theme(panel.background = element_blank())+
  labs(title = "Homeownership vs being a donor", x = "Homeownership", y = "Frequency")


ggplot2::ggplot(fund,mapping = aes( x = NUMCHLD  , y = TARGET_B , fill = TARGET_B)) + 
  geom_bar(stat = "identity") +
  theme(panel.background = element_blank())+
  scale_fill_manual( values = c("blue","grey"))+
  labs(title = "Number of Children vs being a donor", x = "Number of Children", y = "Frequency")




ggplot2::ggplot(fund, aes(x = WEALTH, y = TARGET_B, fill = TARGET_B)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual( values = c("seagreen","grey"))+
  theme(panel.background = element_blank())+
  labs(title = "Barplot of Wealth vs being a donor", x = "Wealth", y = "Frequency")


#_______________________________________PREDICTOR IDENTIFICATION____________________________________________
#fit a basic randomforest model to select the important variables based on the variable importance feature.

set.seed(12345)                       
rf <- randomForest(TARGET_B~., data=fund , ntree = 500,
                   mtry = 4, nodesize = 5, importance = TRUE)    #train
rfImp<-varImpPlot(rf , sort = TRUE, type= 1 , main = 'Variable Importance')                      #compute the variable importance
                     

set.seed(12345)
fundboruta <- Boruta(TARGET_B ~., data = fund , doTrace = 0)
print(fundboruta)
plot(fundboruta, xlab="", xaxt="n")
lz<-lapply(1:ncol(fundboruta$ImpHistory), function(i)
  fundboruta$ImpHistory[is.finite(fundboruta$ImpHistory[, i]), i])
names(lz)<-colnames(fundboruta$ImpHistory)
lb<-sort(sapply(lz, median))
axis(side=1, las=2, labels=names(lb), at=1:ncol(fundboruta$ImpHistory), cex.axis=0.5, font = 4)

#Predictor Selection with RFE
library(caret)
library(randomForest)
set.seed(12345)
control.fund<-rfeControl(functions = rfFuncs, method = "cv", number=10)
rf.train<-rfe(fund[,-17], fund[, 17], sizes=c(10, 20, 30, 40), rfeControl=control.fund)
rf.train


#_____________________________Selected Variables_____________________________________
fund <- fund[,-c(1,3,4,5,6,7,8,9,10,11,15,18)]
t(t(names(fund)))

#_________________________________Change factor variable into integer to prepare for normalization________________________
fund$TARGET_B <- as.integer(fund$TARGET_B)


#_________________________________Normalize the original data__________________________________________

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

fund_norm<-as.data.frame(lapply(fund, normalize))
str(fund_norm)

#________________________________Partition the normalized  data__________________________________________________
sub<-sample(nrow(fund_norm), floor(nrow(fund_norm)*0.6))
fund_train <- fund_norm[sub, ]
fund_test <- fund_norm[-sub, ]
prop.table(table(fund_train$TARGET_B))

##_____________________________Neural Networks_________________________________________________________
# Fit neural network models
fund_model.nn <- neuralnet(TARGET_B ~ ., data = fund_train, hidden = 4, linear.output = FALSE)
plot(fund_model.nn)

# Make predictions and evaluate model performance
fund_pred <- predict(fund_model.nn, fund_test[,-6])

# Compute the confusion matrix and print the results
cm_knn_all <- confusionMatrix(as.factor(ifelse(fund_pred> 0.5, "1", "0")), as.factor(fund_test$TARGET_B), positive = "1")
print(cm_knn_all)

#____________________________________KNN_____________________________________
fund_knn_train <- fund_train
fund_knn_test <- fund_test

# Convert the target variable to a factor
fund_knn_train$TARGET_B <- as.factor(fund_knn_train$TARGET_B)
fund_knn_test$TARGET_B <- as.factor(fund_knn_test$TARGET_B)

# Define the trainControl function
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Train the k-NN model using the train function from the caret package
set.seed(123) # for reproducibility
fund_knn_model <- train(TARGET_B ~ ., data = fund_knn_train, method = "knn", trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 10)

# Print the model
print(fund_knn_model)

# Use the predict function to make predictions on the test dataset
fund_knn_pred <- predict(fund_knn_model, newdata = fund_knn_test[,-6])

# Compute the confusion matrix and print the results
cm_knn_all <- confusionMatrix(fund_knn_pred, as.factor(fund_knn_test$TARGET_B), positive = "1")
print(cm_knn_all)


#________________________Logistic regression _____________________________________________

# Convert the target variable to a factor
fund_train$TARGET_B <- as.factor(fund_train$TARGET_B)
fund_test$TARGET_B <- as.factor(fund_test$TARGET_B)

# Create a matrix of predictors
X_train <- model.matrix(TARGET_B ~ ., data = fund_train)[,-1]
X_test <- model.matrix(TARGET_B ~ ., data = fund_test)[,-1]

# Define the trainControl function
ctrl <- trainControl(method = "cv", number = 10)

# Train the logistic regression model using the train function from the caret package
set.seed(123) # for reproducibility
fund_glmnet_model <- train(TARGET_B ~ ., data = fund_train, method = "glmnet", trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 10)

# Print the model
print(fund_glmnet_model)

# Use the predict function to make predictions on the test dataset

fund_glmnet_pred <- predict(fund_glmnet_model, newdata = fund_test[,-6])

# Compute the confusion matrix and print the results
cm_glmnet_all <- confusionMatrix(fund_glmnet_pred, as.factor(fund_test$TARGET_B), positive = "1")
print(cm_glmnet_all)

#____________________________CLASSIFICATION Trees_____________________________________

set.seed(12345)
train_index <- sample(seq_len(nrow(fund)), size = 0.6*nrow(fund))
fund_train.tree<- fund[train_index, ]
fund_test.tree<- fund[-train_index, ]
prop.table(table(fund_train.tree$TARGET_B))
prop.table(table(fund_test.tree$TARGET_B))

fund_train.tree$TARGET_B <- as.factor(ifelse(fund_train.tree$TARGET_B==1 ,'non donor' , "donor"))
fund_test.tree$TARGET_B <- as.factor(ifelse(fund_test.tree$TARGET_B==1 ,'non donor' , "donor"))
str(fund_train.tree)

library("rpart") 
library("caret")

set.seed(1234)

fund_tree_model<-rpart(TARGET_B~., data=fund_train.tree, cp=0.01) 
fund_tree_model
library(rpart.plot)
rpart.plot(fund_tree_model, type = 4,extra = 1,clip.right.labs = F)
fund_tree_pred<- predict(fund_tree_model, fund_test.tree[,-6],type = 'class')
confusionMatrix(table(fund_tree_pred, fund_test.tree$TARGET_B), positive = "donor")


#____________________________Make prediction________________________________
future.fund <- read.csv("FutureFundraising.csv", header = TRUE)
future.fund.pred <- predict(fund_tree_model, future.fund , type = "class")

#Write future into a new csv
write.csv(future.fund.pred, "futurefund.csv")
newfund<- read.csv("futurefund.csv")
table(new$x)

ggplot(newfund, aes(x = x, fill = (x))) + 
  geom_bar() +
  geom_text(aes(label=..count..), stat ="count", vjust =1.5,colour = "black")+
  scale_fill_manual(values =c("lightblue","salmon")) +
  theme(panel.background = element_blank())+
  labs(title = "Barplot of New Potential Members", x = "TARGET", y = "Frequency")

#New netprofit
 13*1599 

# New cost to send to donors only 
 0.68 *1599
 
 #Net profit for organization
 20787 -1087.32
 