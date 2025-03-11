#Packages used for Project
install.packages("tidyverse")
install.packages("pastecs")
install.packages("ggplot2")
install.packages("Amelia")
install.packages("gridExtra")
install.packages("VIM")
install.packages("VIM")
install.packages("imbalance")
install.packages("ROSE")
install.packages("corrplot")
install.packages("Boruta")



library(Boruta)
library(corrplot)
library(ROSE)
library(imbalance)
library(VIM)
library(pastecs)
library(ggplot2)
library(Amelia)
library(gridExtra)
library(tidyverse)
library(randomForest)


#________________________Load homeequity.dfset into R_________________________________#########
homeequity.df <- read.csv("hmeq.csv" , header = TRUE)

#__________________Overview homeequity.df___________________________________________
str(homeequity.df)
t(t(names(homeequity.df)))
dim(homeequity.df)
head(homeequity.df)
tail(homeequity.df)
stat.desc(homeequity.df)

#_______________________Handling_Missing_Data________________________________________________
#Find Missing Values

sum(is.na(homeequity.df)) # returns the total number of missing values in the homeequity.dfset
missmap(homeequity.df)  # returns the number of missing values in each columns of the homeequity.dfset
aggr_plot <- aggr(homeequity.df, col=c('lightgreen','gray'), sortVars=TRUE, numbers=TRUE, labels=names(homeequity.df), cex.axis=.5, gap=2, ylab=c("Histogram of missing homeequity.df","Pattern"))

# create data frame from colSums output
missing_values <- colSums(is.na(homeequity.df))
missing_df <- data.frame(variable = names(missing_values), missing = missing_values)

View(missing_df)

# add variable names column
missing_df <- tibble::rownames_to_column(missing_df, "index")
missing_df <- missing_df[,-c(1,2,5,6)]


# create barplot of Mising values
ggplot(missing_df, aes(x = variable, y = missing , fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Missing Values in Home Equity Dataset",
       x = "Variable", y = "Missing Values") +
  theme(panel.background = element_blank())+
  scale_fill_hue(c = 60)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#________________________remove missing values_______________________________________________
homeequity.df <- na.omit(homeequity.df)
missmap(homeequity.df, col= c('white','cyan')  , main ='missmap of homeequity.df')


#________________________Typographical Errors in REASON and JOB_________________________________________________________
## Variables : JOB & REASON
table(homeequity.df$REASON)
table(homeequity.df$JOB)

# View Reason Visualization without correcting error
ggplot(homeequity.df, aes(x = REASON,fill = REASON)) +
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Frequency of values in REASON", x = "Classes", y = "Frequency") +
  theme(panel.background = element_blank())+
  scale_fill_hue(c = 60)

#View JOB Visualization without correcting error
ggplot(homeequity.df, aes(x = JOB,fill = JOB)) +
  geom_bar()+
  labs(title = "Frequency of values in JOB", x = "Classes ", y = "Frequency") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme(panel.background = element_blank())+
  scale_fill_hue(c = 60)

#### Replace empty spaces in JOB with other
homeequity.df$JOB[which(homeequity.df$JOB=="")] = 'Other'
homeequity.df$JOB = factor(homeequity.df$JOB, labels=c('Mgr','Office','Other','ProfExe','Sales','Self'))

###Check to see if error has been corrected
table(homeequity.df$JOB)

##Replace empty spaces in REASON with other
homeequity.df$REASON[which(homeequity.df$REASON=="")] = "other"
homeequity.df$REASON= factor(homeequity.df$REASON)

#Check to see if error has been corrected
table(homeequity.df$REASON)

# View Reason Visualization after correcting error
ggplot(homeequity.df, aes(x = REASON,fill = REASON)) +
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Frequency of values in REASON", x = "Classes", y = "Frequency") +
  theme(panel.background = element_blank())+
  scale_fill_hue(c = 60)

#View JOB Visualization after correcting error
ggplot(homeequity.df, aes(x = JOB,fill = JOB)) +
  geom_bar()+
  labs(title = "Frequency of values in JOB", x = "Classes ", y = "Frequency") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme(panel.background = element_blank())+
  scale_fill_hue(c = 60)

#____________________________Data Transformation___________________________________________
#Merge classes that have smaller values
homeequity.df$NINQ[homeequity.df$NINQ > 3]<-3
table(homeequity.df$NINQ)

#Merge classes that have smaller values
homeequity.df$DEROG[homeequity.df$DEROG > 2]<-2
table(homeequity.df$DEROG)

#Merge classes that have smaller values
homeequity.df$DELINQ[homeequity.df$DELINQ > 2]<-2
table(homeequity.df$DELINQ)


#Convert 'categorical integer variables to factor
homeequity.df$BAD <- as.factor(homeequity.df$BAD)
homeequity.df$JOB <- as.factor(homeequity.df$JOB)
homeequity.df$REASON <- as.factor(homeequity.df$REASON)

# Convert integer variables to numeric variables
homeequity.df$LOAN <- as.numeric(homeequity.df$LOAN)
homeequity.df$NINQ <- as.numeric(homeequity.df$NINQ)
homeequity.df$CLNO <- as.numeric(homeequity.df$CLNO)
homeequity.df$DEROG <- as.numeric(homeequity.df$DEROG)
homeequity.df$DELINQ <- as.numeric(homeequity.df$DELINQ)


#__________________________________________Plot of Numeric Attributes_______________________________________________________________


#LOAN

B.L = homeequity.df %>%
  ggplot(aes(x=BAD, y=LOAN, fill=BAD)) + geom_boxplot()

#MORTDUE

B.M = homeequity.df %>%
  ggplot(aes(x=BAD, y=MORTDUE, fill=BAD)) + geom_boxplot()

#VALUE

B.V = homeequity.df %>%
  ggplot(aes(x=BAD, y=VALUE, fill=BAD)) + geom_boxplot()
#YOJ 

B.Y = homeequity.df %>%
  ggplot(aes(x=BAD, y=YOJ, fill=BAD)) + geom_boxplot()

#CLAGE

B.CLAGE = homeequity.df %>%
  ggplot(aes(x=BAD, y=CLAGE, fill=BAD)) + geom_boxplot()

#CLNO 

B.CLNO = homeequity.df %>%
  ggplot(aes(x=BAD, y=CLNO, fill=BAD)) + geom_boxplot()

#DEBTINC

B.D = homeequity.df %>%
  ggplot(aes(x=BAD, y=DEBTINC, fill=BAD)) + geom_boxplot()

grid.arrange(B.L, B.M, B.V, B.Y, B.CLAGE, B.CLNO, B.D, ncol=2)

#___________________Exploratory analysis_______________________________________________
#visual exploration: boxplots

ggplot(homeequity.df, aes(y = NINQ)) +
  geom_boxplot(fill = "lightblue") +
  ylab("NINQ") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of NINQ")


ggplot(homeequity.df, aes(y = LOAN)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  ylab("LOAN") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of LOAN with Outliers in Red")

ggplot(homeequity.df, aes(y = MORTDUE)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  ylab("MORTDUE") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of MORDUE with Outliers in Red")

ggplot(homeequity.df, aes(y = VALUE)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  ylab("VALUE") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of VALUE with Outliers in Red")

ggplot(homeequity.df, aes(y = YOJ)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  ylab("YOJ") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of YOJ with Outliers in Red")

ggplot(homeequity.df, aes(y = DEROG)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  ylab("DEROG") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of DEROG with Outliers in Red")

ggplot(homeequity.df, aes(y = DELINQ)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  ylab("DELINQ") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of DELINQ with Outliers in Red")

ggplot(homeequity.df, aes(y = CLAGE)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  ylab("CLAGE") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of CLAGE with Outliers in Red")

ggplot(homeequity.df, aes(y = NINQ)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  ylab("NINQ") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of NINQ with Outliers in Red")


ggplot(homeequity.df, aes(y = CLNO)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  ylab("CLNO") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of CLNO with Outliers in Red")

ggplot(homeequity.df, aes(y = DEBTINC)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  ylab("DEBTINC") +
  theme(axis.text.y = element_text(size = 12, color = "red"),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Boxplot of DEBTINC with Outliers in Red")

#______________________TARGET VARIABLE VISUALIZATION____________________________________________
homeequity.df$BAD <- factor(ifelse(homeequity.df$BAD ==1,"FAIL TO PAY", "PAID"))
ggplot(homeequity.df, aes(x = BAD,fill = BAD)) +
  geom_bar()+
  labs(title = "Frequency of values in BAD IN OMITTED homeequity.dfSET", x = "Classes ", y = "Frequency") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme(panel.background = element_blank())+
  scale_fill_hue(c = 60)

homeequity.df$NINQ <- as.integer(homeequity.df$NINQ)

#_______________________________CATEGORICAL VARIABLE VISUALIZATION____________________________________________________________________________
#JOB
ggplot(homeequity.df , mapping = aes( x = JOB, y= BAD,fill = BAD)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "slateblue"))+
  labs(title = "JOB against Target Variable", x = "JOB ", y = "Frequency") +
  theme(panel.background = element_blank())

#REASON
ggplot(homeequity.df , mapping = aes( x = REASON, y = BAD, fill = BAD)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "slateblue"))+
  labs(title = "REASON against Target Variable", x = "REASON", y = "Frequency") +
  theme(panel.background = element_blank())

#NINQ
homeequity.df$NINQ <- as.factor(homeequity.df$NINQ)
ggplot(homeequity.df , mapping = aes( x = NINQ, y = BAD, fill = BAD)) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 100)+
  labs(title = "NINQ against Target Variable", x = "NINQ ", y = "Frequency") +
  theme(panel.background = element_blank())

#DELINQ
homeequity.df$DELINQ <- as.factor(homeequity.df$DELINQ)
ggplot(homeequity.df , mapping = aes( x = DELINQ, y = BAD, fill = BAD)) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 100)+
  labs(title = "DELINQ against Target Variable", x = "DELINQ ", y = "Frequency") +
  theme(panel.background = element_blank())

#DEROG
homeequity.df$DEROG <- as.factor(homeequity.df$DEROG)
ggplot(homeequity.df , mapping = aes( x = DEROG, y = BAD, fill = BAD)) + 
  geom_bar(stat = "identity") +
  scale_fill_hue(c = 100)+
  labs(title = "DEROG against Target Variable", x = "DEROG ", y = "Frequency") +
  theme(panel.background = element_blank())



#_____________________________VARIABLE SELECTION______________________________________________________

corrplot(cor(homeequity.df))

# INPUT NUMERICAL VERSION OF BAD,JOB AND REASON
homeequity.df$BAD.1 <- as.numeric(homeequity.df$BAD)
homeequity.df$JOB.1 <- as.numeric(homeequity.df$JOB)
homeequity.df$REASON.1 <- as.numeric(homeequity.df$REASON)

# Convert integer variables to numeric variables
homeequity.df$LOAN <- as.numeric(homeequity.df$LOAN)
homeequity.df$NINQ <- as.numeric(homeequity.df$NINQ)
homeequity.df$CLNO <- as.numeric(homeequity.df$CLNO)
homeequity.df$DEROG <- as.numeric(homeequity.df$DEROG)
homeequity.df$DELINQ <- as.numeric(homeequity.df$DELINQ)

#Correlation Plot
corrplot(cor(homeequity.df[,-c(1,5,6)]),method='number')
corrplot(cor(homeequity.df[,-c(1,5,6)]))

#______________________BORUTA METHOD___________________________________________

set.seed(12345)

homeboruta <- Boruta(BAD.1 ~., data = homeequity.df[,-c(1,5,6)], doTrace = 0)
print(homeboruta)
homeboruta$ImpHistory
plot(homeboruta, xlab="", xaxt="n")
lz<-lapply(1:ncol(homeboruta$ImpHistory), function(i)
  homeboruta$ImpHistory[is.finite(homeboruta$ImpHistory[, i]), i])
names(lz)<-colnames(homeboruta$ImpHistory)
lb<-sort(sapply(lz, median))
axis(side=1, las=2, labels=names(lb), at=1:ncol(homeboruta$ImpHistory), cex.axis=0.5, font = 4)
final.boruta<-TentativeRoughFix(homeboruta)
print(final.boruta)
final.boruta$finalDecision
getConfirmedFormula(final.boruta)

#___________________________Random Forest Method___________________________________

set.seed(12345) 
home <- homeequity.df[,-c(14,15,16)]
rf <- randomForest(BAD~., data= home, ntree = 500,
                   mtry = 4, nodesize = 5, importance = TRUE)    #train
rfImp<-varImpPlot(rf , type= 1)          

#_______________________________PARTITIONS_____________________________________________________

homeequity.df$BAD <- as.factor(homeequity.df$BAD)
homeequity.df$REASON <- as.factor(homeequity.df$REASON)
homeequity.df$JOB <- as.factor(homeequity.df$JOB)
t(t(names(homeequity.df)))

homeequity.df <- homeequity.df[,-c(14,15,16)]
str(homeequity.df)
#_________________________Partitioning into training (60%) and validation (40%)_______________________________________
set.seed(12345)
train_index <- sample(nrow(homeequity.df), round(nrow(homeequity.df)*0.6))
train <- homeequity.df[train_index,]
test <- homeequity.df[-train_index,]

unique(homeequity.df$BAD)


#____________________________BALANCE THE DATA_________________________________________________________________
table(train$BAD)
prop.table(table(train$BAD))
table(test$BAD)
prop.table(table(test$BAD))
library(ROSE)
train_balanced <- ROSE(BAD ~ ., data = train, seed= 1)$data
barplot(table(train_balanced$BAD))


#####################LOGISTIC REGRESSION CLASSIFICATIONS##################################
train.logreg <- train
train.rose.logreg <- train_balanced
test.logreg <- test

logit.reg <- glm(BAD ~ ., data = train.logreg, family = "binomial")
summary(logit.reg)
logit.reg.pred <- predict(logit.reg ,test.logreg[,-1], response= "class")

library(caret)
confusionMatrix(factor(ifelse(logit.reg.pred >0.5 ,'FAIL TO PAY', "PAID"), levels = c( 'FAIL TO PAY', "PAID")), 
                factor(test.logreg$BAD, levels = c('FAIL TO PAY', "PAID")))

#Blanced Data logistic regression
train.logreg <- train
train.rose.logreg <- train_balanced
test.logreg <- test

#Logistic regression with balanced data
logit.reg2 <- glm(BAD ~ ., data = train.rose.logreg, family = "binomial")
summary(logit.reg2)



logit.reg.pred2 <- predict(logit.reg2 ,test.logreg[,-1], response= "class")


confusionMatrix(factor(ifelse(logit.reg.pred2 >0.5 ,'FAIL TO PAY', "PAID"), levels = c( 'FAIL TO PAY', "PAID")), 
                factor(test.logreg$BAD, levels = c('FAIL TO PAY', "PAID")))


####################################Classification tree#############################################################################################################
train.decision <- train
train.rose.decision <- train_balanced
test.decision <- test

train.decision$BAD <- as.factor(train.decision$BAD)
test.decision$BAD <- as.factor(test.decision$BAD)
train.rose.decision $BAD <- as.factor(train.rose.decision$BAD) 

library("rpart")
library("caret")
set.seed(1234)
home_tree_model<-rpart(BAD~., data=train.decision , cp = 0.01) 
home_tree_model
library(rpart.plot)
rpart.plot(home_tree_model, type = 4,extra = 1,clip.right.labs = F)

home_tree_pred<- predict(home_tree_model, test.decision[,-1],type = "class")
confusionMatrix(table(home_tree_pred,test.decision$BAD) ,positive = "FAIL TO PAY")


#Classification Tree with Balanced Data
set.seed(1234)
home_tree_model2<-rpart(BAD~., data=train.rose.decision, cp=0.01) 
home_tree_model2

rpart.plot(home_tree_model2, type = 4,extra = 1,clip.right.labs = F)


home_tree_pred2 <-  predict(home_tree_model2, test.decision[,-1],type = 'class')

confusionMatrix(table(home_tree_pred2,test.decision$BAD) ,positive = "FAIL TO PAY")


##################################RANDOM FOREST CLASSIFIER #########################################

# Load necessary packages
library(caret)

train.rf<- train
train.rose.rf<- train_balanced
test.rf<- test

# Convert the target variable to a factor
train.rf$BAD <- as.factor(train.rf$BAD)
test.rf$BAD <- as.factor(test.rf$BAD)
train.rose.rf$BAD <- as.factor(train.rose.rf$BAD)

# Train the random forest model with default settings
set.seed(1234)
rf_model <- train(BAD ~ ., data = train.rf, method = "rf")

# Make predictions on the test set
rf_pred <- predict(rf_model, test.rf)

# Evaluate the model's performance
confusionMatrix(rf_pred, test.rf$BAD, positive = "FAIL TO PAY")

# Train the random forest model with balanced data
set.seed(1234)
rf_model2 <- train(BAD ~ ., data = train.rose.rf, method = "rf")

# Make predictions on the test set
rf_pred2 <- predict(rf_model2, test.rf[,-1])

# Evaluate the model's performance
confusionMatrix(rf_pred2, test.rf$BAD, positive = "FAIL TO PAY")
