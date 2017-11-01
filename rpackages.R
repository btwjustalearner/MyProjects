library(readr)
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE) 
iris <- read.table('guidedata.rdata',header=T)
iris <- read.table('traindata.rdata',header=T)
iris <- read.table('data_imputed.rdata',header=T)
iris <- guidedata
iris <- z
iris.train <- z
iris.test <- guidedata
iris.test <- test
train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
iris.train <- iris[train.idx, ]
iris.test <- iris[-train.idx, ]


iris.train <- iris[(1:2922), ]
iris.test <- iris[(2923:13597), ]

head(iris)
train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
iris.train <- read.table('traindata.rdata',header=T)
iris.test <- read.table('testdata.rdata',header=T)

iris.test <- subset(iris.test, select = -INTRDVX)
for (i in 1:265){
  if(class(iris.train[,i]) != class(iris.test[,i])){
    print(i)
  }
}
str(iris.test[255])


###rpart
library(rpart)
fit_rpart <- rpart(INTRDVX ~ ., data = iris.train1)
predict_rpart <- predict(fit_rpart, newdata = iris.test1, type = "vector")
mean(z$INTRDVX)
mean(predict_rpart)
mse_rpart <- sum((predict_rpart-iris.test$INTRDVX)^2)/nrow(iris.test)
write(predict_rpart,'predict_rpart.txt')
range(iris.train$INTRDVX)
range(predict_rpart)
###party
library(party)
fit_ctree <- ctree(INTRDVX ~ ., data = iris.train1)
predict_ctree <- predict(fit_ctree, newdata = iris.test1, type = "response")
mse_ctree <- sum((predict_ctree-iris.test$INTRDVX)^2)/nrow(iris.test)
mean(predict_ctree)
write(predict_ctree,'predict_ctree.txt')
range(iris.train$INTRDVX)
range(predict_ctree)
###randomForest
library(randomForest)
iris.train1 <- na.roughfix(iris.train)
iris.test1 <- na.roughfix(iris.test)
iris.test <- subset(iris.test, select = -INTRDVX)
fit_rf <- randomForest(INTRDVX ~ ., data = iris.train1)
class(iris.train$INTRDVX)
predict_rf <- predict(fit_rf, newdata = iris.test1, type = "response")
iris.test2 <- iris.train1
predict_rf2 <- predict_rf[1:2922]
predict_rf3 <- predict_rf[2933:13597]
mse_rf <- sum((predict_rf2-iris.test2$INTRDVX)^2)/nrow(iris.test2)
write(predict_rf,'predict_rf.txt')
range(iris.train$INTRDVX)
range(predict_rf3)
mean(predict_rf3)


###ranger
library(ranger)
fit_ranger <- ranger(INTRDVX ~ ., data = iris.train1)
predict_ranger <- predict(fit_ranger, data = iris.test1)
mse_ranger <- sum((predict_ranger-iris.test1$INTRDVX)^2)/nrow(iris.test1)

###nnet(too many weights)
library(nnet)
fit_nnet <- nnet(INTRDVX ~ ., data = iris.train1,, size = 2, rang = 0.1,decay = 5e-4, maxit = 200,na.action = na.omit )
predict_nnet <- predict(fit_nnet, newdata = iris.test1)
mse_nnet <- sum((predict_nnet-iris.test$INTRDVX)^2)/nrow(iris.test)

###treebag
library(caret)
library(plyr)
library(mboost)
fit_treebag <- train(INTRDVX ~ ., data=iris.train1, method = 'treebag')
predict_treebag <- predict(fit_treebag, newdata = iris.test1)
iris.test2 <- iris.train1
predict_treebag2 <- predict_treebag[1:2922]
predict_treebag3 <- predict_treebag[2933:13597]
mse_treebag <- sum((predict_treebag2-iris.test2$INTRDVX)^2)/nrow(iris.test2)
mse_treebag <- sum((predict_treebag-iris.test1$INTRDVX)^2)/nrow(iris.test1)
range(predict_treebag)

###svm
library(e1071)
x <- subset(iris.train, select = -INTRDVX)
y <- iris.train$INTRDVX
fit_svm1 <- svm(x,y)
fit_svm <- svm(INTRDVX ~ ., data=iris.train1, kernel = 'linear', cost = 5, gamma = 0.5)
fit_svm <- svm(INTRDVX ~ ., data=iris.train, scale = TRUE)
predict_svm <- predict(fit_svm, newdata = iris.test1)
iris.test2 <- iris.train1
predict_svm2 <- predict_svm[1:2922]
predict_svm3 <- predict_svm[2933:13597]
mse_svm <- sum((predict_svm2-iris.test2$INTRDVX)^2)/nrow(iris.test2)
mse_svm <- sum((predict_svm-iris.test1$INTRDVX)^2)/nrow(iris.test1)
range(iris.train$INTRDVX)
range(predict_svm)
predict_svm[predict_svm > max(iris.train$INTRDVX)] <- max(iris.train$INTRDVX)
predict_svm[predict_svm < min(iris.train$INTRDVX)] <- min(iris.train$INTRDVX)

#kernel
library(kernlab)
fit_ORFsvm <- train(INTRDVX ~ ., data=iris.train1, method = 'lssvmLinear')
predict_treebag <- predict(fit_treebag, newdata = iris.test1)
mse_treebag <- sum((predict_treebag-iris.test1$INTRDVX)^2)/nrow(iris.test1)
range(predict_treebag)

###lm
fit_lm <- lm(INTRDVX ~ ., data = iris.train1)
predict_lm <- predict(fit_lm, newdata = iris.test1)
mse_lm <- sum((predict_lm-iris.test$INTRDVX)^2)/nrow(iris.test)
range(iris.train$INTRDVX)
range(iris.test$INTRDVX)
range(predict_lm)
predict_lm[predict_lm > max(iris.train$INTRDVX)] <- max(iris.train$INTRDVX)
predict_lm[predict_lm < min(iris.train$INTRDVX)] <- min(iris.train$INTRDVX)

predict_random <- rep(4777,974)
mse_random <- sum((predict_random-iris.test$INTRDVX)^2)/nrow(iris.test)

predict_f<-(predict_rpart+predict_rf+predict_treebag+predict_ctree)/4
mse <- sum((predict_f-iris.test$INTRDVX)^2)/nrow(iris.test)

predict_f<-(predict_rf+predict_svm)/2
mean(predict_f)
mse <- sum((predict_f-iris.test1$INTRDVX)^2)/nrow(iris.test1)

rresult2 <- cbind(predict_f,predict_rf)
colnames(rresult2) <- c("method1", "method2")
head(predict_ctree)
write.table(rresult2, file = "results_project1.txt", row.names = F, col.names = T)

result_ratio <- c(ratio_rpart, ratio_ctree,  ratio_rf, ratio_ranger, ratio_nnet)
result_time <- c(time_rpart, time_ctree, time_rf, time_ranger, time_nnet)
result <- data.frame(Ratio = result_ratio, Time = result_time)
method_name <- c("rpart", "ctree", "randomForest", "ranger", "nnet")
rownames(result) <- method_name
knitr::kable(result)

results = read.table(file = "results_project1.txt", header = T)
dim(results)







