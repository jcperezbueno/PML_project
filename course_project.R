### Course project

setwd("~/Sabatico/Coursera/Data science/Practical Machine Learning/Course Project")

library(caret)

sumisna=function(x){sum(is.na(x))}
sumisblank = function(x){sum(x=="")}

training = read.csv("pml-training.csv")
testing = read.csv("pml-testing.csv")

invalid_variables = c(which(apply(training,2,sumisna) == 19216),which(apply(training,2,sumisblank) == 19216))

training = training[,-invalid_variables]
training = training[training$new_window=="no",]
training = training[,-(1:7)]
      


set.seed(300)
inTrain = createDataPartition(training$classe, p=0.7,list = FALSE)
training_1 = training[inTrain,]
training_2 = training[-inTrain,]

training_1_pre = predict(preProcess(training_1[,-53],method="pca",thresh=0.90),training_1) # Creates the new dataframe

training_2_pre = predict(preProcess(training_1[,-53],method="pca",thresh=0.90),training_2) # Creates the new dataframe

# set.seed(100)
# mod1=randomForest(classe~.,training_1_pre)
set.seed(100)
mod2=train(classe~.,training_1_pre,method="rf")


# pred1 = predict(mod1,training_2_pre)
pred2 = predict(mod2,training_2_pre)

# confusionMatrix(pred1,training_2$classe)
confusionMatrix(pred2,training_2$classe)

accuracy= NULL
for (i in (1:100)){
      set.seed(i)
      ind = sample(nrow(training_2_pre),20,FALSE)
      pred = predict(mod2,training_2_pre[ind,])
      accuracy[i]=confusionMatrix(pred,training_2$classe[ind])$overall[1]
}


pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
}

testing_pre = predict(preProcess(training_1[,-53],method="pca",thresh=0.90),testing)

pred3 = predict(mod2,testing_pre)

pml_write_files(pred3)

