# Final Project on "Census Income" Dataset

# 1.Data Preprocessing:

# a)Replace all the missing values with NA.
setwd("C:\\Users\\user\\Downloads")
getwd()
census_income=read.csv("census-income_.csv",stringsAsFactors = F)
View(census_income)
str(census_income)
census_income$workclass=as.character(census_income$workclass)
census_income$occupation=as.character(census_income$occupation)
census_income$native.country=as.character(census_income$native.country)
census_income$education=as.character(census_income$education)
census_income$marital.status=as.character(census_income$marital.status)
census_income$relationship=as.character(census_income$relationship)
census_income$race=as.character(census_income$race)
census_income$sex=as.character(census_income$sex)
str(census_income)

census_income[census_income==" ?"]= NA  


View(census_income)

# b)Remove all the rows that contain NA values.
census_income=na.omit(census_income)

# c)Remove all whitespaces from the columns.
library(stringr) 
library(dplyr)

census_income=census_income %>%
  mutate_if(is.character, str_trim)

census_income$workclass=as.factor(census_income$workclass)
census_income$occupation=as.factor(census_income$occupation)
census_income$native.country=as.factor(census_income$native.country)
census_income$education=as.factor(census_income$education)
census_income$marital.status=as.factor(census_income$marital.status)
census_income$relationship=as.factor(census_income$relationship)
census_income$race=as.factor(census_income$race)
census_income$sex=as.factor(census_income$sex)
census_income$X=as.factor(census_income$X)

str(census_income)



# 2.Data Manipulation:

summary(census_income)

# a)Extract the "education" column and store it in "census_ed" .
census_ed=census_income$education
str(census_ed)
head(census_ed)

# b)Extract all the columns from "age" to "relationship" and store it in "census_seq".
census_seq=census_income%>%select(age:relationship)
str(census_seq)

# c)Extract the column number "5", "8", "11" and store it in "census_col".
census_col=census_income[,c(5,8,11)]
str(census_col)
head(census_col)

# d)Extract all the male employees who work in state-gov and store it in "male_gov".
male_gov=census_income%>% filter(sex == "Male" & workclass=="State-gov")
head(male_gov)

# e)Extract all the 39 year olds who either have a bachelor's degree or who are native of United States and store the result in "census_us".
table(census_income$native.country)
table(census_income$education)
census_us=census_income%>%filter(age==39&(education=="Bachelors"|native.country=="United-States"))
head(census_us)

# f)Extract 200 random rows from the "census" data frame and store it in "census_200".
census_200=sample_n(census_income,200)
head(census_200)

# g)Get the count of different levels of the "workclass" column.
count_workclass=count(census_income,workclass)
count_workclass

# h)Calculate the mean of "capital.gain" column grouped according to "workclass".

census_income%>%group_by(workclass)%>%summarise(mean(capital.gain))

# 3.Data Visualization:

library(ggplot2)

# a)Build a bar-plot for the "relationship" column and fill the bars according to the "race" column.
ggplot(census_income,aes(x=relationship,fill=race))+geom_bar()

# i.Set x-axis label to 'Categories of Relationships'
ggplot(census_income,aes(x=relationship,fill=race))+geom_bar()+labs(x="Categories of Relationships")

# ii.Set y-axis label to 'Count of Categories'
ggplot(census_income,aes(x=relationship,fill=race))+geom_bar()+labs(x="Categories of Relationships",y="Count of Categories")

# iii.Fill the bars according to "sex"
ggplot(census_income,aes(x=relationship,fill=sex))+geom_bar()+labs(x="Categories of Relationships",y="Count of Categories")

# iv.Set the position of the bars to "dodge"
ggplot(census_income,aes(x=relationship,fill=sex))+geom_bar(position = "dodge")+labs(x="Categories of Relationships",y="Count of Categories")

# v.Set the title of plot to be 'Distribution of Relationships by Sex"
ggplot(census_income,aes(x=relationship,fill=sex))+geom_bar(position = "dodge")+labs(x="Categories of Relationships",y="Count of Categories",title = "Distribution of Relationships by Sex")


# b)Build a Histogram for the "age" column with number of bins equal to 50.
ggplot(census_income,aes(x=age))+geom_histogram(bins = 50)

# i)Fill the bars of the histogram according to yearly income column i.e., "X"
ggplot(census_income,aes(x=age,fill=X))+geom_histogram(bins = 50)

# ii)Set the title of the plot to "Distribution of Age".
ggplot(census_income,aes(x=age,fill=X))+geom_histogram(bins = 50)+labs(title = "Distribution of Age")

# iii)Set the legend title to "Yearly income".
ggplot(census_income,aes(x=age, fill=X))+geom_histogram(col='green')+labs(title = "Distribution of Age",fill='Yearly income')

# iv)Set the theme of the plot to black and white.
ggplot(census_income,aes(x=age, fill=X))+geom_histogram(col='green')+labs(title = "Distribution of Age",fill='Yearly income')+theme_bw()

# c)Build a scatter-plot between "capital.gain" and "hours.per.week". Map "capital.gain" on the x- axis and "hours.per.week" on the y-axis.
ggplot(census_income,aes(x=capital.gain,y=hours.per.week))+geom_point()

# i)Set the transparency of the points to 40% and size as 2.
ggplot(census_income,aes(x=capital.gain,y=hours.per.week))+geom_point(alpha=0.4,size=2)

# ii)Set the color of the points according to the "X" (yearly income) column. 
ggplot(census_income,aes(x=capital.gain,y=hours.per.week, fill=X))+geom_point(alpha=0.4,size=2)

# iii)Set the x-axis label to "Capital Gain", y-axis label to "Hours per Week", title to "Capital Gain vs Hours per Week by Income", and legend label to "Yearly Income".
ggplot(census_income,aes(x=capital.gain,y=hours.per.week, fill=X))+geom_point(alpha=0.4,size=2)+labs(x="Capital Gain", y="Hours per Week", title = "Capital Gain vs Hours per Week by Income", fill="Yearly Income") 

# d)Build a box-plot between "education" and "age" column.Map "education" on the x-axis and "age" on the y-axis.
ggplot(census_income,aes(x=education,y=age))+geom_boxplot()

# i)Fill the box-plots according to the "sex" column.
ggplot(census_income,aes(x=education,y=age,fill=sex))+geom_boxplot()

# ii)	Set the title to "Box-Plot of age by Education and Sex".
ggplot(census_income,aes(x=education,y=age,fill=sex))+geom_boxplot()+labs(title = "Box-Plot of age by Education and Sex") 

# 4.Linear Regression:

# a)Build a simple linear regression model as follows:

# i)Divide the dataset into training and test sets in 70:30 ratio.
set.seed(111)
library("caTools")
split_data=sample.split(census_income$hours.per.week,SplitRatio = 0.70)
train1=subset(census_income,split_data==T)
test1=subset(census_income,split_data==F)
dim(train1)
dim(test1)

# ii)	Build a linear model on the test set where the dependent variable is "hours.per.week" and independent variable is "education.num".
str(census_income)
lin_model=lm(hours.per.week~education.num,data=train1)
summary(lin_model)

# iii)Predict the values on the train set and find the error in prediction. 
linpred = predict(lin_model,newdata=test1)
head(linpred)

lindata=cbind(Actual=test1$hours.per.week,Predicted=linpred)
head(lindata)
class(lindata)

lindata=as.data.frame(lindata)

linerror=lindata$Actual-lindata$Predicted
head(linerror)
lindata=cbind(lindata,linerror)
head(lindata)

# iv)Find the root-mean-square error (RMSE).
sqrt(mean((lindata$linerror)^2))



# 5.Logistic Regression:

# a)Build a simple logistic regression model as follows:

# i)Divide the dataset into training and test sets in 65:35 ratio.
split_data1=sample.split(census_income$X,SplitRatio = 0.65)
train2=subset(census_income,split_data1==T)
test2=subset(census_income,split_data1==F)
dim(train2)
dim(test2)
str(census_income)

# ii)Build a logistic regression model where the dependent variable is "X"(yearly income) and independent variable is "occupation".
log_model=glm(X~occupation,data=train1,family = "binomial")
summary(log_model)

# iii)Predict the values on the test set.
log_pred = predict(log_model,newdata = test2,type = "response")
head(log_pred)
range(log_pred)

library(ROCR) 
predict_log_roc=prediction(log_pred,test2$X)
predict_log_roc
acc=performance(predict_log_roc,"acc")
plot(acc)
table(census_income$X)

# iv)Plot accuracy vs cut-off and pick an ideal value for cut-off.
log.pred=ifelse(log_pred>0.47,">50K","<=50K")  
log.pred

# v)Build a confusion matrix and find the accuracy.
tab=table(log.pred,test2$X)
tab

accuracy=sum(diag(tab))/sum(tab)
accuracy


# vi)Plot the ROC curve and find the auc(Area Under Curve). 
roc=performance(predict_log_roc,"tpr","fpr")
plot(roc)
auc = performance(predict_log_roc, "auc")
auc
auc=auc@y.values[[1]]
auc


# 6.Decision Tree:

# a)Build a decision tree model as follows:

# i)Divide the dataset into training and test sets in 70:30 ratio.
set.seed(333)
split_data=sample.split(census_income$X,SplitRatio = 0.7)
train3=subset(census_income,split_data==T)
test3=subset(census_income,split_data==F)
dim(train3)
dim(test3)

# ii)Build a decision tree model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables.
library(rpart)
##install.packages("rpart.plot")
library(rpart.plot) 

Tree_model=rpart(formula = X~., data = train3, method = "class")

# iii)Plot the decision tree.
rpart.plot(x= Tree_model, type= 5, extra = 0,tweak = 1.2)

# iv)Predict the values on the test set.
tree_prediction=predict(Tree_model,newdata = test3,type = "class")

# v)Build a confusion matrix and calculate the accuracy.
con_mat=table(tree_prediction,test3$X)
con_mat
sum(diag(con_mat))/sum(con_mat)


# 7.Random Forest:

# a)Build a random forest model as follows:
 
# i)Divide the dataset into training and test sets in 80:20 ratio.
set.seed(444)
split_data=sample.split(census_income$X,SplitRatio = 0.8)
train4=subset(census_income,split_data==T)
test4=subset(census_income,split_data==F)
dim(train4)
dim(test4)

# ii)Build a random forest model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables and number of trees as 300.
library(randomForest)

RanFor_model=randomForest(formula=X~., data=train4, ntree=300)
plot(RanFor_model)

# iii)Predict values on the test set
Ranfor_prediction=predict(RanFor_model,newdata = test4,type = "class")

# iv)Build a confusion matrix and calculate the accuracy
con_mat2=table(Ranfor_prediction,test4$X)
con_mat2
sum(diag(con_mat2))/sum(con_mat2)

