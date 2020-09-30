install.packages("mlbench")
library(mlbench)
data("HouseVotes84")
plot(as.factor(HouseVotes84[,2]))
title(main='Votes cast for issue 1',xlab="vote",ylab="Num reps")
 HouseVotes84$Class
Repub <- HouseVotes84$Class=="republican" 
Democrat <- HouseVotes84$Class=="democrat"
Repub
plot(as.factor(HouseVotes84[Repub,2]))
title(main="Republican cotes cast for issue 1",xlab="vote",ylab="Num reps")
plot(as.factor(HouseVotes84[Democrat,2]))
title(main="Democrat cotes cast for issue 1",xlab="vote",ylab="Num reps")

na_by_col_class <- function(col,cls){
  return(sum(is.na(HouseVotes84[,col])& HouseVotes84$Class==cls))
}
na_by_col_class

p_y_col_class <- function(col,cls){
sum_y <- sum(HouseVotes84[,col]=="y"& HouseVotes84$Class==cls,na.rm = TRUE)
sum_n <- sum(HouseVotes84[,col]=="y"& HouseVotes84$Class==cls,na.rm = TRUE)

 return(sum_y/(sum_y+sum_n))
}

#check the prob of yes vote by decom in 5
p_y_col_class(5,"democrat")
#check the prob of yes vote by repub in 5
p_y_col_class(5,"republication")

#check the prob of yes vote for both
na_by_col_class(5,"republication")
na_by_col_class(2,"democrat")
na_by_col_class(2,"republication")

#impute missing values

for(i in 2:ncol(HouseVotes84)){
  if(sum(is.na(HouseVotes84[,i]>0))){
  c1<- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=="democrat",arr.ind = TRUE)
  c2<- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=="republican",arr.ind = TRUE)
  HouseVotes84[c1,i]<-
    ifelse(runif(na_by_col_class(i,"democrat"))<p_y_col_class(i,"democrat"),"y","n")
  
  HouseVotes84[c2,i]<-
    ifelse(runif(na_by_col_class(i,"republican"))<p_y_col_class(i,"republican"),"y","n")}
}


#divide into test and training sets

HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<0.80,1,0)

#get col number of tran
trainColNum <- grep("train",names(HouseVotes84))

#seperate training and test sets and remove training before modeling
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1,-trainColNum]
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==0,-trainColNum]
trainHouseVotes84


#load e1072 lib and invoke naiveBoyes
library(e1071)
nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)
nb_model
summary(nb_model)
str(nb_model)

nb_test_predict <- predict(nb_model,HouseVotes84[,-1])

mean(nb_test_predict==HouseVotes84$Class)

table(pred=nb_test_predict,true=HouseVotes84$Class)
