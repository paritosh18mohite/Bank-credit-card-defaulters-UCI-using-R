
library(dplyr)			# For data manipulation
library(ggplot2)		# For plotting 
library(caret)			# for data-preprocessing & for data-partitioning
library(dplyr)
library(magrittr)
library(forcats)  # For feature reordering in ggplot
library(grid)
library(gridExtra)
library(forcats)  # For feature reordering in ggplot
library(data.table)
library(caTools)




UCI_Credit_Card=read.csv("C:/Users/kupekarraj/Downloads/UCI_Credit_Card.csv")
#View(UCI_Credit_Card)
head(UCI_Credit_Card ,5)
#checking for Null value
sum(is.na(UCI_Credit_Card))		# There is none
dim(d_credit)
#Drop the ID Column
UCI_Credit_Card$ID <- NULL
#converting 1 and 2 to male and female
UCI_Credit_Card$SEX <- as.factor(UCI_Credit_Card$SEX)
levels(UCI_Credit_Card$SEX) <- c("Male","Female")
#num to factor as in this case the unknown and other are merged together
UCI_Credit_Card$EDUCATION <- as.factor(UCI_Credit_Card$EDUCATION)
levels(UCI_Credit_Card$EDUCATION) <- c("Others", "Graduate School", "University", "High school", "Others", "Others", "Others")
#here the divorced and unknown are considered as others
UCI_Credit_Card$MARRIAGE <- as.factor(UCI_Credit_Card$MARRIAGE)
levels(UCI_Credit_Card$MARRIAGE) <- c("Unknown" , "Married" , "Single" ,"Others")

UCI_Credit_Card$default.payment.next.month <- as.factor(UCI_Credit_Card$default.payment.next.month)

levels(UCI_Credit_Card$default.payment.next.month) <- c("No" , "Yes")

UCI_Credit_Card$agegroup<-cut(UCI_Credit_Card$AGE, c(20,30,40,50,60,70,80),labels = c("20s","30s","40s","50s","60s","70s") )

UCI_Credit_Card$PAY_0 <- as.factor(UCI_Credit_Card$PAY_0)
UCI_Credit_Card$PAY_2 <- as.factor(UCI_Credit_Card$PAY_2)
UCI_Credit_Card$PAY_3 <- as.factor(UCI_Credit_Card$PAY_3)
UCI_Credit_Card$PAY_4 <- as.factor(UCI_Credit_Card$PAY_4)
UCI_Credit_Card$PAY_5 <- as.factor(UCI_Credit_Card$PAY_5)
UCI_Credit_Card$PAY_6 <- as.factor(UCI_Credit_Card$PAY_6)
dim(UCI_Credit_Card)


# Plot Defaulter Status with Age, Education, Marital Status and Gender
b1 <- ggplot(data=UCI_Credit_Card, aes(x=EDUCATION,fill=default.payment.next.month)) + geom_bar(position='dodge') +
  labs(title = "credit defaulter Status by Education", x ="Education",fill = "Default Status") +
  scale_fill_manual(values=c("#009E73", "red")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

b2 <- ggplot(data=UCI_Credit_Card, aes(x=MARRIAGE,fill=default.payment.next.month)) + geom_bar(position='dodge') +
  labs(title = "credit defaulter Status by Marital Status", x ="Marital Status",fill = "Default Status") +
  scale_fill_manual(values=c("#009E73", "red")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

b3 <- ggplot(data=UCI_Credit_Card, aes(x=agegroup,fill=default.payment.next.month)) + geom_bar(position='dodge') +
  labs(title = "credit defaulter Status by Age Group", x ="Age Group",fill = "Default Status") +
  scale_fill_manual(values=c("#009E73", "red")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

b4 <- ggplot(data=UCI_Credit_Card, aes(x=SEX,fill=default.payment.next.month)) + geom_bar(position='dodge') +
  labs(title = "credit defaulter Status by Gender", x ="Gender",fill = "Default Status") +
  scale_fill_manual(values=c("#009E73", "red")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

grid.arrange(b1,b2,b3,b4,ncol=2)
rm(b1,b2,b3,b4)







as.numeric(UCI_Credit_Card$default.payment.next.month)



d_credit=read.csv("C:/Users/kupekarraj/Downloads/UCI_Credit_Card.csv")
#View(UCI_Credit_Card)
head(d_credit ,5)
#checking for Null value
sum(is.na(d_credit))		# There is none
dim(d_credit)
#Drop the ID Column
d_credit$ID <- NULL
#sampling data with 80% of the dataset
s=sample(nrow(d_credit),.8*nrow(d_credit))
s

#splitting the dataset into training and testing
training_credit=d_credit[s,]
testing_credit=d_credit[-s,]

#creadting model 1
colnames(d_credit)
library(glmnet)
logit_model1=glm(default.payment.next.month~.,family="binomial",data=training_credit)
logit_model1    
#keeping default payment next month as dependent variable and considering a full model........
summary(logit_model1)
library(car)
varImp(logit_model1)
vif(logit_model1)

#predict model 1
p_logit1=predict(logit_model1,testing_credit,type="response")
p_logit1


#confusion matrix model 1
predict_logit1=ifelse(p_logit1>.6,1,0)
predict_logit1

table(predict_logit1,testing_credit$default.payment.next.month)
nrow(testing_credit)
#accuracy score for model 1 i.e  for full model 
ac_score_logit1=(4775/6000)*100
ac_score_logit1

#tabulating the data to inspect
ac_predict_logit1=data.frame(cbind("Actuals"=testing_credit$default.payment.next.month,"Predicted"=predict_logit1))
ac_predict_logit1
head(ac_predict_logit1)



#creating model  2 removing non significant variables
logit_model2=glm(default.payment.next.month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+BILL_AMT1+PAY_AMT1+PAY_AMT2+PAY_AMT4,family="binomial",data=training_credit)
logit_model2
summary(logit_model2)

#predict model 2
p_logit2=predict(logit_model2,testing_credit,type="response")
p_logit2
table(predict_logit1,testing_credit$default.payment.next.month)
#confusion matrix model 2
predict_logit2=ifelse(p_logit2>.5,1,0)
predict_logit2
nrow(testing_credit)
#accuracy score
ac_score_logit2=(4871/6000)*100
ac_score_logit2


#likelyhood ratio test using lmtest
library(lmtest)
lrtest(logit_model1,logit_model2) #p<0.5 i.e it rejects null hypothesis, which says that unconstrained model is good

#Pseudo R^2
library(pscl)
pR2(logit_model1)
pR2(logit_model2)
#McFadden R2 value are more than 1 indicating both the models have predictive power



#variable importance
library(caret)
varImp(logit_model1)
varImp(logit_model2)

#ROC Curve
library(pROC)
roc_data1=roc(default.payment.next.month~logit_model1$fitted.values,plot=TRUE,data=training_credit,legacy.axes=TRUE,percent=TRUE, xlab="False Positive %", ylab="True Positive %", main="for logit_model 1")
#.............#area under the curve for model 1 is 72%
nrow(roc_data1)
roc_data2=roc(default.payment.next.month~logit_model2$fitted.values,plot=TRUE,data=training_credit,legacy.axes=TRUE,percent=TRUE, xlab="False Positive %", ylab="True Positive %", main="for logit_model 2")
#............#area under the curve for model 1 is 72%

#checking threshold
roc_df1=data.frame(cbind("true +ve"=roc_data1$sensitivities, "false +ve"=roc_data1$specificities, thresholds=roc_data1$thresholds))
head(roc_df1)
nrow(roc_df1)
roc_df2=data.frame(cbind("true +ve"=roc_data2$sensitivities, "false +ve"=roc_data2$specificities, thresholds=roc_data2$thresholds))
head(roc_df2)
nrow(roc_df2)

library(ISLR)
library(caret)
library(rpart)
library(rattle)
library(dtree)

dtree_mod1=rpart(default.payment.next.month~.,training_credit,method = "class",control = rpart.control
                 (minsplit = 1,minbucket = 1,cp=0.001))
summary(dtree_mod1)
varImp(dtree_mod1)
pred_tree=predict(dtree_mod1,testing_credit[,-25],type="class")
table(testing_credit$default.payment.next.month,pred_tree)
acc=table(testing_credit$default.payment.next.month,pred_tree)
sum(diag(acc))/sum(acc)
library(rpart.plot)
library(rpart)
prp(dtree_mod1)
### cv
library(e1071)
library(rpart)

numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
cpGrid
treecv<-train(default.payment.next.month~., data = training_credit, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
p_cv=predict(treecv,testing_credit[,-25])
p_cv
table(testing_credit$default.payment.next.month,p_cv)
table(testing_credit$default.payment.next.month,pred_tree)
acc_cv=table(testing_credit$default.payment.next.month,p_cv)
sum(diag(acc_cv))/sum(acc_cv)
plot(treecv)

