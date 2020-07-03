# Import Library
library(moments) #For Skewness & Kurtosis
library(ggplot2) #For Visualization
library(ggcorrplot) #For Correlation matrix
library(corrplot) #For Correlation
library(dplyr)  #For data manipulation/recode data

## 1. Data Understanding
head(heart)
str(heart)
dim(heart)
# 1. There is 300 observations of 14 Variables.
# 2. Dependent Variable is Target which has 2 value 0 and 1. 
#    0 means there is no heart attack and 1 means there is Heart Attack.
# 3. All other Vaariables such as age, sex, cp and others are independent variables.
# 4. There is integer and Numeric DataTypes of variables

## 2. Data Analysis
# 2.1 Age Analysis
range(heart$age)
summary(heart$age)
sd(heart$age)
var(heart$age)
# Minimum age is 29 and Maximum age is 77. Average age of Population is 54.37.

cor(heart$age,heart$target)
# There is low negative cor-relation between age and dependent variable target. 
# This shows that on increasing age probability of heart attack is decreasing.

# We can also see Descriptive analysis through BoxPlot for Better Visualization.
hist(heart$age,labels=TRUE,main="Histogram of Age",xlab="Age Class",ylab="Frequency",col="blue")

boxplot(heart$age,horizontal=TRUE,col="red",main="Boxplot of Age")

# 2.2 Sex Analysis
unique(heart$sex)
# This variable has 2 category 0 represents female and 1 means male.

ggplot(data= heart, aes(x=sex, fill=sex)) + geom_bar() +
  labs(title = "Barplot of Sex", x ="Sex",fill = "Sex") +
# In barplot it is clearly Visible that proportion of male is more as compared to female.

b=table(heart$sex,heart$target)
barplot(b,col=c("red","blue"),legend=rownames(b),beside=TRUE,xlab="Target",
        ylab="Count",main="Side by Side Barplot")
# In side by side Barchart we can see that in case of not suffered with Heart Attack, 
# Feamle is very less while more male has survived heart attack.

# 2.3 CP Analysis(Cheast Pain)
unique(heart$cp)
# There is 4 category of Chest pain starting from 0 up to 3.

ggplot(data= heart, aes(x=cp, fill=cp)) + geom_bar() +
  labs(title = "Barplot of CP", x ="CP",fill = "CP")
# In barplot it is clear that, People having Chest pain 0 are more than 2.
# People with chest pain 2 are more likely to get heart attack.

# 2.4 trestbps Analysis(Blood Pressure)
summary(heart$trestbps)
# Minimum BP(Blood Pressure) is 94 and Maximum is 200.
# Average BP is 131.6

cor(heart$trestbps,heart$target)
# There is negative correlation between BP and Target, 
# On increasing BP chance of getting Heart attack will decrease.

boxplot(heart$trestbps,col="pink",main="Descriptive Analysis of RBP",
        horizontal=TRUE)

hist(heart$trestbps,main="Histogram for RBP",xlab="Rest Blood Pressure Class",
     ylab="Frequency",labels=TRUE)
# We can clearly see in Histogram & Box plot, 
# Maximum number of Population have Rest Blood Pressure between 120 and 140.

# 2.5 chol Analysis(Cholestrol)
summary(heart$chol)
# Minimum Cholestrol among patients is 126, Maxmum is 564 and Average Cholestrol is 246.3.

ggplot(data = heart,aes(x = as.factor(sex), y = chol, fill = sex)) + 
  geom_boxplot()
# We can see Boxplot for descriptive Analysis seperately for Male and Female. 
# In boxplot it is also clear that Male has lower colestrol than Female.

ggplot(data=heart, aes(heart$chol)) + geom_histogram()
# We can clearly see in Histogram that maximum population have cholestrol between 200 and 250 unit.

# 2.6 fbs Analysis(Fasting Blood Sugar)
unique(heart$fbs)
summary(heart$fbs)
# This is a Categorical Variable in which 0 means level is less than 120 unit and 1 means it is greater than 120 unit.

ggplot(data= heart, aes(x=fbs, fill=fbs)) + geom_bar()
# From Bar Graph, maximum Peoples have FBS less than 120.

# 2.7 restecg Analysis(Rest ECG)
unique(heart$restecg)
# There is 3 Category in this Variabble 0,1 and 2.

ggplot(data= heart, aes(x=restecg, fill=restecg)) + geom_bar()
# Category 2 of ECG is very less and category 1 or 2 are nearly same.

# 2.8 thalach Analysis(Heart Rate)
summary(heart$thalach)
# This is continous data with minimum heart rate achieved 71 and maximum 202.

ggplot(data = heart,aes(x = as.factor(sex), y = thalach, fill = sex)) + 
  geom_boxplot()
# Maximum heart Rate achieved is greater in Male than Female.

cor(heart$age,heart$thalach)
# There is negative corelation between age and heart rate, this means on getting older Heart rate is decreasing.

# 2.9 exang Analysis(Exercise Induced Angina)
unique(heart$exang)
# Two category 0 and 1, where 0 means no Exang.

k = table(heart$exang,heart$target)
barplot(k,legend=rownames(k),col=rainbow(2),
        main="Stacked Barplot of Exang and Target",xlab="Target",ylab="Count")
# We can clearly see in stacked barplot that people with no Exang is more likely to get Heart Attack.

# 2.10 oldpeak Analysis
summary(heart$oldpeak)
# This variable is numeric with minimum being 0 and maximum 6.20

hist(heart$oldpeak, main="Histogram of Oldpeak",xlab="Oldpeak Class",
     ylab="Frequency",labels=TRUE)
# We can see in Histogram maximum population is lying between 0 - 0.5 .
# Data is right skewed, not normally distributed.

ggplot(data = heart,aes(x = as.factor(sex), y = oldpeak, fill = sex)) + 
  geom_boxplot()
# In boxplot we can see that oldpeak is generally more in male as compared to female. Also Their are some outliers.

# 2.11 slope Analysis
unique(heart$slope)
# This is integer datatype with 3 categories 0,1 and 2.

ggplot(data= heart, aes(x=slope, fill=slope)) + geom_bar()
# In barplot we can see that population with slope 1 and 2 are nearly same and in maximum number.

# 2.12 ca Analysis
unique(heart$ca)
# This is data with 5 categories 0,1,2,3 and 4.

ggplot(data= heart, aes(x=ca, fill=ca)) + geom_bar()
# Most of population are with 0 ca category.

## 3. Split data into train & test
# Random sample indexes
train <- sample(1:nrow(heart),size = ceiling(0.80*nrow(heart)),replace = FALSE)

# training set
htrain <- heart[train,]
dim(htrain)

# test set
htest <- heart[-train,]
dim(htest)


## 4. Model Building
# 4.1 Logistic Regression

#Train model 
logistic <- glm(target ~ ., data = htrain,family='binomial')
summary(logistic)

#test model
predicted= predict(logistic,htest)
View(predicted)
y_pred<-ifelse(predicted > 0.5,1,0)
View(y_pred)

# Confusion Matrix
cm<-table(y_pred,htest$target)
print(cm)

# Accuracy
Accuracy<-sum(diag(cm))/sum(cm)
print(Accuracy*100)
## Accuracy of Logistic Regression is 81.66%

# F1-score
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)
precision
recall
f1


# 4.2 SVM
library(caret)
# train model
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(target ~., data = htrain, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear

# test model
test_pred <- predict(svm_Linear, newdata = htest)
View(test_pred)


# Target variable convert into factor
htrain[["target"]] = factor(htrain[["target"]])
htest[["target"]] = factor(htest[["target"]])


confusionMatrix(test_pred, htest$target)
## Accuracy of SVM model is 86.67%

