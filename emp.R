# Employee attrition Model
pathemp="C:\\Users\\AAVEJ\\Desktop\\Mtech\\TPL\\Project\\hr_emp.csv"
emp=read.csv(pathemp)
View(emp)
str(emp)
colnames(emp)

# Converting Int data into Factors

emp$Education=as.factor(emp$Education)
emp$EnvironmentSatisfaction=as.factor(emp$EnvironmentSatisfaction)
emp$JobInvolvement=as.factor(emp$JobInvolvement)
emp$JobLevel=as.factor(emp$JobLevel)
emp$JobSatisfaction=as.factor(emp$JobSatisfaction)
emp$PerformanceRating=as.factor(emp$PerformanceRating)
emp$RelationshipSatisfaction=as.factor(emp$RelationshipSatisfaction)
emp$StockOptionLevel=as.factor(emp$StockOptionLevel)
emp$WorkLifeBalance=as.factor(emp$WorkLifeBalance)
emp$attr_value=as.factor(emp$attr_value)
str(emp)

length(colnames(emp))
emp$Attrition=NULL


# Checking NULL values And Zeros


checknull=function(x) return(any(is.na(x)))
checkzero=function(x) return(any(x==0))

#check NULL
c1=colnames(emp)[unlist(lapply(emp,checknull))]
print(c1)

#checkzero

c2=colnames(emp)[unlist(lapply(emp[],checkzero))]
ifelse(length(c2)<=0,"no zero","there are zeros")
print(c2)


# Seperating Catagorical And numeric data

colnames(emp)[unlist(lapply(emp,is.numeric))]
numeric_data=colnames(emp)[unlist(lapply(emp,is.numeric))]
Cat_data=colnames(emp)[unlist(lapply(emp,is.factor))]
numeric_data
Cat_data

#Multicolinearity

corr=cor(emp[,numeric_data])
install.packages('corrplot')
library(corrplot)
corrplot(corr,method = "number",type = "lower")






#outlier
for (c in numeric_data) {
msg=paste("box plot for",c)
boxplot(emp[,c],main=msg,horizontal = T)
}


#updating the outliers
avg_currentmanager=mean(emp$YearsWithCurrManager)
avg_currentmanager=round(avg_currentmanager)
emp$YearsWithCurrManager[emp$YearsWithCurrManager>7]=avg_currentmanager

avg_lastyrpromostion=mean(emp$YearsSinceLastPromotion)
avg_lastyrpromostion  =round(avg_lastyrpromostion  )
avg_lastyrpromostion
emp$YearsSinceLastPromotion[emp$YearsSinceLastPromotion>5]=avg_lastyrpromostion

avg_role=mean(emp$YearsInCurrentRole)
avg_role=round(avg_role)
avg_role
emp$YearsInCurrentRole[emp$YearsInCurrentRole>13]=avg_role


emp2=emp

# From multicolinearity remove the coumn having bigger multicolinearity value
emp2=emp
colnames(emp2)
emp2$YearsAtCompany=NULL
emp2$YearsWithCurrManager=NULL
emp2$YearsSinceLastPromotion=NULL





# Splitting the data


rows=nrow(emp)
rows
s=sample(seq(1,rows),0.7*rows)
train=emp[s,]
test=emp[-s,]
print(paste('train=',nrow(train),'test',nrow(test)))
prop.table(table(emp$attr_value))
prop.table(table(train$attr_value))
prop.table(table(test$attr_value))

lvl_tr=length(levels(factor(train$attr_value)))
lvl_te=length(levels(factor(test$attr_value)))

if(lvl_tr>=lvl_te)
  print("Levels are ok") else
    print("levels are more than training")

#Logistic Regression
m1=glm(attr_value~.,data = train,binomial(link = 'logit'))
summary(m1)


#predections
p1=predict(m1,test,type = 'response')
p1[1:10]
table(test$attr_value)

length(p1[p1<=0.5])
length(p1[p1>0.5])
                                                                     
#converting likelyhood estimated classes 0,1
pred1=ifelse(p1<=0.5,0,1)
print(pred1[1:10])

cbind(p1[1:10],pred1[1:10])

# for confusion matrix
install.packages('installr')
library(installr)
updateR()
install.packages('caret')
library(caret)
install.packages('e1071', dependencies=TRUE)
pred1=as.factor(pred1)
confusionMatrix(test$attr_value,pred1,positive = "1")


rPartMod =train(attr_value ~ ., data=train, method="rpart")#R Square
rPartMod
rpartImp= varImp(rPartMod)
print(rpartImp)








# 2nd MOdel
emp1=emp
emp1$TotalWorkingYears=NULL

length(emp1)


rows1=nrow(emp1)
rows1
s1=sample(seq(1,rows1),0.7*rows1)
train1=emp1[s1,]
test1=emp1[-s1,]
print(paste('train=',nrow(train1),'test',nrow(test1)))
prop.table(table(emp1$attr_value))
prop.table(table(train1$attr_value))
prop.table(table(test1$attr_value))

lvl_tr=length(levels(factor(train1$attr_value)))
lvl_te=length(levels(factor(test1$attr_value)))

if(lvl_tr>=lvl_te)
  print("Levels are ok") else
    print("levels are more than training")

#Logistic Regression
m2=glm(attr_value~.,data = train1,binomial(link = 'logit'))
summary(m2)


#predections
p2=predict(m2,test1,type = 'response')
p2[1:10]

table(test1$attr_value)

length(p2[p2<=0.5])
length(p2[p2>0.5])

#converting likelyhood estimated classes 0,1
pred2=ifelse(p2<=0.5,0,1)
print(pred2[1:10])

cbind(p2[1:10],pred2[1:10])

# for confusion matrix
library(caret)
pred2=as.factor(pred2)
confusionMatrix(test1$attr_value,pred2,positive = "1")
summary(m1)
summary(m2)


AIC(m1,m2)# Akaike Information Criterion

# Feature Selection After multicolinearity

# From multicolinearity remove the coumn having bigger multicolinearity value
emp2=emp
colnames(emp2)
emp2$YearsAtCompany=NULL
emp2$YearsWithCurrManager=NULL
emp2$YearsSinceLastPromotion=NULL




rows=nrow(emp2)
rows
ncol(emp2)
seq=sample(seq(1,rows),0.7*rows)
trainm=emp2[seq,]
testm=emp2[-seq,]
print(paste('train=',nrow(trainm),'test',nrow(testm)))
prop.table(table(emp2$attr_value))
prop.table(table(trainm$attr_value))
prop.table(table(testm$attr_value))

lvl_trm=length(levels(factor(trainm$attr_value)))
lvl_tem=length(levels(factor(testm$attr_value)))

if(lvl_trm>=lvl_tem)
  print("Levels are ok") else
    print("levels are more than training")

#Logistic Regression
mm1=glm(attr_value~.,data = trainm,binomial(link = 'logit'))
summary(mm1)


#predections
pm1=predict(mm1,testm,type = 'response')
pm1[1:10]
table(testm$attr_value)

length(pm1[pm1<=0.5])
length(pm1[pm1>0.5])

#converting likelyhood estimated classes 0,1
predm1=ifelse(pm1<=0.5,0,1)
print(predm1[1:10])

cbind(pm1[1:10],predm1[1:10])

# for confusion matrix
library(caret)
predm1=as.factor(predm1)
confusionMatrix(testm$attr_value,predm1,positive = "1")






# Random Forest


# Feature Selection
install.packages("Boruta")
library(Boruta)

boruta=Boruta(attr_value~.,data=na.omit(train),doTrace=0)
boruta





pos=grep("attr_value",colnames(train))
pos
train_x=train[,1:pos-1]
train_y=train[,pos]
test_x=test[,1:pos-1]
test_y=test[,pos]
library(randomForest)
nrow(train_x)
length(train_y)

mrf=randomForest(train_x,train_y)
mrf
p2=predict(mrf,test_x)
library(caret)
confusionMatrix(test_y,p2,positive = '1')

mrf1=randomForest(train_x,train_y,mtry = 5,ntree = 400)
mrf1
p3=predict(mrf1,test_x)
library(caret)
confusionMatrix(test_y,p3,positive = '1')

