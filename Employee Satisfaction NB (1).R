library(naivebayes)

emp<- read.csv("C:/Users/BHIMI REDDY COTTON/Downloads/Employee Satisfaction NB.R", header = TRUE)
print(emp)


dim(emp)
length(emp)
head(emp)
head(emp, n=10)
tail(emp)
tail(emp,n=10)
emp[6,]
emp[,c(10,11,12,13)]
emp[1:10,]
emp[,9]
str(emp)
# DATA PREPROCESSING
summary(emp)
emp$Dept=as.factor(emp$Dept)
emp$location =as.factor(emp$location )
emp$education=as.factor(emp$education)
emp$recruitment_type=as.factor(emp$recruitment_type)
emp$satisfied =as.factor(emp$satisfied)
summary(emp)


barplot(table(emp$satisfied),
        main="Employee Satisfaction", col="Green")

y<-emp$satisfied
print(y)
x1<- emp$ emp_id
x2<- emp$age
hist(x2, main = "Age of Employees",xlab="Age", ylab="No.of Employees",col = "Brown")
x3<- emp$Dept
barplot(table(x3),
        main="Department", col="Yellow")
x3
x4<- emp$location 
barplot(table(x4))
x4
x5<- emp$education
barplot(table(x5))

x6<- emp$recruitment_type
barplot(table(x6))

x7<- emp$job_level
x8<- emp$rating

x9<- emp$onsite
x10<- emp$awards
x12<- emp$certifications
x13<- emp$salary


emp<- emp[,c(2,3,4,5,6,7,8,9,10,11,12,13)]
print(emp)

# Sampling of Datasets
set.seed(1)

ind <- sample(2, nrow(emp), replace=TRUE, prob=c(0.70, 0.30))
trainData <- emp[ind==1,]
testData <- emp[ind==2,]


print(trainData)
print(testData)

naive_model <- naive_bayes(satisfied ~ age+Dept+location+education+recruitment_type+job_level+
                             rating+onsite+ awards+certifications+ salary, 
                           data=trainData, type="C-classification")
print(naive_model)
plot(naive_model)

#predict on test data
testData1 = testData[,-c(12)]
testData1
testPred <- predict(naive_model, newdata = testData1)
print(testPred)
print(testData$satisfied)


a<-data.frame(age=38,Dept="Marketing",location= "Suburb", education="UG",
              recruitment_type ="Referral", job_level= 2, rating= 5, onsite= 1,
              awards=2, certifications= 0, salary = 29805)
result <- predict(naive_model,a)
print(result)

