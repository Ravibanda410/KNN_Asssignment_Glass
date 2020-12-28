install.packages('ggplot2')  #for Data Visualization
library(ggplot2)

install.packages('corrplot') #Correlation Plot
library(corrplot)

glass <- read.csv("C:/RAVI/Data science/Assignments/Module 18 KNN/KNN Assignment1 dataset/glass.csv/glass.csv")

View(glass)
attach(glass)

table(glass$Type)

str(glass$Type)


# table or proportation of enteries in the datasets. What % of glass of Type 1 and what % of glass of Type 2
#round(prop.table(table(glass$Type))*100,1)

# summarize any three numeric features
summary(glass[c("RI", "Na", "Fe")])
summary(glass$Fe)
head(glass)

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the glass data #normalization function - result should be identical#
glass_n <- as.data.frame(lapply(glass[1:9], normalize))  
glass_n

# confirm that normalization worked
summary(glass_n$Mg)

head(glass_n)

#Data Visualization
plot(glass_n)
corrplot(cor(glass_n))


# create training and test datasets

#random sampling
n  <- nrow(glass_n)
n1 <- n*0.8
n1
n2 <- n-n1
n2

train_index  <- sample(1:n,n1)

glass_train <- glass[train_index, ] 
glass_test  <- glass[-train_index, ] 

#Creating seperate dataframe for 'Type' feature which is our target.

  glass_train_labels <- glass[train_index,10]
  glass_test_labels <- glass[-train_index,10]

#---- Training a model on the data ----

#Find the number of observation
NROW(glass_train_labels)
sqrt(171)    # k=13

# load the "class" library
install.packages("class")  ##KNN 
library(class)

glass_test_pred <- knn(train = glass_train, test = glass_test,
                      cl = glass_train_labels, k=1)
glass_test_pred

#Error in prediction
error <- mean(glass_test_pred!=glass_test_labels)
error

install.packages('caret')
library(caret)

##--------Evaluating model performance ----

# load the "gmodels" library
#library(gmodels)


#Model Evaluation
#Calculate the proportion of correct classification for k = 1

install.packages('e1071', dependencies=TRUE)
# Check prediction against actual value in tabular form for k=1
table(glass_test_pred ,glass_test_labels)
confusionMatrix(table(glass_test_pred, glass_test_labels))


glass_test_pred <- NULL
error_rate <- NULL



for (i in 1:10) {
  glass_test_pred <- knn(train = glass_train, test = glass_test,cl = glass_train_labels,k=i)
  error_rate[i] <- mean(glass_test_pred!=glass_test_labels)
}




knn_error <- as.data.frame(cbind(k=1:10,error_type =error_rate))

#K Value by Visualization
ggplot(knn_error,aes(k,error_type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')


glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=4)
glass_test_pred

#Error in prediction
error <- mean(glass_test_pred!=glass_test_labels)
error
confusionMatrix(table(glass_test_pred,glass_test_labels))



glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=1)
confusionMatrix(table(glass_test_pred,glass_test_labels))


glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=2)
confusionMatrix(table(glass_test_pred,glass_test_labels))


glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=3)
confusionMatrix(table(glass_test_pred,glass_test_labels))


glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=4)
confusionMatrix(table(glass_test_pred,glass_test_labels))


glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=5)
confusionMatrix(table(glass_test_pred,glass_test_labels))

glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=7)
confusionMatrix(table(glass_test_pred,glass_test_labels))

glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=9)
confusionMatrix(table(glass_test_pred,glass_test_labels))

glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=10)
confusionMatrix(table(glass_test_pred,glass_test_labels))

