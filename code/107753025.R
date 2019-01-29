library(data.table)
library(mltools)
library(class)
library(caTools)
library(ggplot2)
library(e1071)



data0 <- read.csv("cdspfs/sales_train_v2.csv",header = TRUE,sep = ",")
sample1 <- sample(nrow(data0),100000)
data1 <- data0[sample1,]
data2 <- read.csv("cdspfs/shops.csv",header = TRUE,sep = ",")
data3 <- read.csv("cdspfs/item_categories.csv",header = TRUE,sep = ",")
data4 <- read.csv("cdspfs/items.csv",header = TRUE,sep = ",")
data5 <- read.csv("cdspfs/test.csv",header = TRUE,sep = ",")
head(data1)
summary(data1)
sum(is.na(data1))
data6.V1 <-as.Date(data1$date,"%d.%m.%Y")
data6.V2 <- as.factor(months(data6.V1))
data6.V3 <- as.factor(weekdays(data6.V1))
data6.V4 <- unclass(as.POSIXlt(data6.V1))
data6.V5 <- as.factor(data6.V4$year)
data6.V22 <- one_hot(as.data.table(data6.V2))
data6.V33 <- one_hot(as.data.table(data6.V3))
data6.V55 <- one_hot(as.data.table(data6.V5))
data7.V1 <- as.factor(data1$date_block_num)
data7.V2 <- as.factor(data1$shop_id)
data7.V3 <- as.factor(data1$item_id)
data7.V11 <- one_hot(as.data.table(data7.V1))
data7.V22 <- one_hot(as.data.table(data7.V2))
#data7.V33 <- one_hot(as.data.table(data7.V3))
data8.1 <- cbind(data7.V11,data7.V22)
data8.2 <- cbind(data8.1,data6.V55)
data8.3 <- cbind(data8.2,data6.V22)
data8.4 <- cbind(data8.3,data6.V33)
data8.5 <- cbind(data8.4,data1$item_price)

colnames(data8.5)[which(names(data8.5) == "V2")] <- "price"
data9 <- cbind(data8.5,data1$item_cnt_day)

n<-10
list1 <- list(1:n)
data10 <- split(data9,list1)
data10.1 <-data10$`1`
data10.2 <-data10$`2`
data10.3 <-rbind(data10$`3`,data10$`4`)
data10.4 <-rbind(data10$`5`,data10$`6`)
data10.5 <-rbind(data10$`7`,data10$`8`)
data10.6 <-rbind(data10$`9`,data10$`10`)
data10.7 <-rbind(data10.3,data10.4)
data10.8 <-rbind(data10.5,data10.6)
data10.9 <-rbind(data10.7,data10.8)
x <- as.numeric(length(data9))  

test1 <- data10.1
test1$V2 <- NULL
valid1 <- data10.2
valid1$V2 <- NULL
train1 <- data10.9
train1$V2 <- NULL
test1.1 <- data10.1$V2
valid1.1 <- data10.2$V2
train1.1 <- data10.9$V2
test1.2 <- data10.1
valid1.2 <- data10.2
train1.2 <- data10.9

#knn
knna<-0
knnc<-0
#for (i in c(3:7)) {
knnpredict <- knn(train1,test1,train1.1,k=5)
y <- summary(test1.1==knnpredict)
z <- summary(1==knnpredict)
cat("my answer:", as.integer( y[3])/(as.integer( y[2])+as.integer( y[3])))
cat("naive answer:",as.integer( z[3])/(as.integer( z[2])+as.integer( z[3])))

#knnresult <- table(x = test1.1, y = knnpredict)
#knnaccuracy <- sum(knnresult==test1.1)/sum(knnresult)
#if(knnaccuracy>knna){
#  knna <- knnaccuracy
#  knnc <- i
#}
#}

#SVM
#svmm <- svm(V2 ~ ., data=train1.2 ,probability=TRUE)
#svmpredict <- predict(svmm, test1.2 ,probability=TRUE)
#svmresult <- table(x = test1.1, y = svmpredict)
#svmaccuracy <- sum(diag(svmresult))/sum(svmresult)

#NBC
#nbcm <- naiveBayes(V2 ~ ., data=train1.2)
#nbcpredict <- predict(nbcm, test1.2)
#nbcresult <- table(x = test1.1, y = predict)
#nbcaccuracy <- sum(diag(nbcresult))/sum(nbcresult)
