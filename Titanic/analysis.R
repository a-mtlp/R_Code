setwd("~/GitHub/R_Code/Titanic")
train <- read.csv('train.csv',header = TRUE)
test <- read.csv('test.csv', header = TRUE)
test.survived <- data.frame(Survived= rep('None', nrow(test)), test[,])
dat.combined <- rbind(train, test.survived)
str(dat.combined)
dat.combined$Pclass<- as.factor(dat.combined$Pclass)
dat.combined$Survived<- as.factor(dat.combined$Survived)
table(dat.combined$Survived)
table(dat.combined$Pclass)
library(ggplot2)
train$Pclass<-as.factor(train$Pclass)
str(train)
ggplot(train, aes(x=Pclass, fill=factor(Survived))) +  geom_bar()
length(unique(as.character(dat.combined$Name)))

dup.names <- as.character(dat.combined[which(duplicated(as.character(dat.combined$Name))),"Name"])
dat.combined$nName %in% dup.names
dat.combined$Name<- as.factor(dat.combined$Name)
str(dat.combined)
str(dup.names)
dat.combined[which(dat.combined$Name %in% dup.names),]

library(stringr)
misses <- dat.combined[which(str_detect(dat.combined$Name,"Miss.")),]
misses

mrses <- dat.combined[which(str_detect(dat.combined$Name,"Mrs.")),]
mrses

males <- dat.combined[which(dat.combined$Sex =='male'),]
males

extractTitle <- function(name){
  name <- as.character(name)
  if(length(grep("Miss.",name))>0 ){
    return ("Miss.")
  }else if(length(grep("Master.",name))>0 ){
    return("Master.")
  }else if(length(grep("Mrs.",name))>0 ){
    return("Mrs.")
  }else if(length(grep("Mr.",name))>0 ){
    return("Mr.")
  }else{
    return ("Other")
  }
  
}
titles <-c()
for(i in 1:nrow(dat.combined)){
  titles<-c(titles,extractTitle(dat.combined$Name[i]))
}
dat.combined$Title <- as.factor(titles)
str(dat.combined)
ggplot(dat.combined[1:891,], aes(x=Title, fill=Survived)) +  
  geom_bar()+
  facet_wrap(~Pclass)
table(dat.combined$Sex)

str(train)
train$Survived <- as.factor(train$Survived)
ggplot(train[,], aes(x = train$Sex, fill = train$Survived))+
  geom_bar()+
  facet_wrap(~Pclass)

summary(dat.combined$Age)


temp.sibsp <-c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch,test$Parch)
dat.combined$family.size<- as.factor(temp.sibsp+temp.parch+1)
boys<- dat.combined[which(dat.combined$Title=="Master."),]
head(boys)
misses<- dat.combined[which(dat.combined$Title=="Miss."),]
head(misses)
summary(misses$Age)

ggplot(misses[misses$Survived!="None",],aes(x=Age, fill= Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass)

misses.alone<- misses[misses$SibSp==0 & misses$Parch==0,]
nrow(misses.alone)
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

summary(dat.combined$SibSp)
length(unique(dat.combined$SibSp))
dat.combined$SibSp<- as.factor(dat.combined$SibSp)
train$SibSp<- as.factor(train$SibSp)
ggplot(dat.combined[1:891,], aes(x=SibSp, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+ Title)+
  ylim(0,100)

str(dat.combined$Ticket)
dat.combined$Ticket<- as.character(dat.combined$Ticket)
str(dat.combined$Ticket)
dat.combined$Ticket[1:20]
ticket.firstchar<- ifelse(dat.combined$Ticket=="", " ", substring(dat.combined$Ticket,1,1))
unique(ticket.firstchar)
dat.combined$Ticket.firstchat<- as.factor(ticket.firstchar)

library(randomForest)

rf.train.1 <- dat.combined[1:891, c("Pclass","Title")]
rf.label <- as.factor(train$Survived)
set.seed(1234)
rf.1 <- randomForest(x= rf.train.1, y= rf.label,importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)
# Pclass and Title are quite predictive

rf.train.2 <- dat.combined[1:891, c("Pclass","Title","SibSp")]
set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree= 1000)
rf.2
varImpPlot(rf.2)
# Pclass Title and SibSp have better Occurancy in predicting 
#but have a worse negative-true rate


rf.train.3 <- dat.combined[1:891, c("Pclass","Title","Parch")]
set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree= 1000)
rf.3
varImpPlot(rf.3)

# Parch is not as predictive as SibSp


rf.train.4 <- dat.combined[1:891, c("Pclass","Title","Parch", "SibSp")]
set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree= 1000)
rf.4
varImpPlot(rf.4)

# combined Parch and sibsp are very predictive

rf.train.5 <- dat.combined[1:891, c("Pclass","Title","family.size")]
set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree= 1000)
rf.5
varImpPlot(rf.5)

# Improvement! the Algorithm cannot combine parch and sibsp to family size
#-> Data Scientist work

rf.train.6 <- dat.combined[1:891, c("Pclass","Title","family.size","SibSp")]
set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree= 1000)
rf.6
varImpPlot(rf.6)
# Occuracy goes down when SibSp combined with family.size


rf.train.7 <- dat.combined[1:891, c("Pclass","Title","family.size","Parch")]
set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree= 1000)
rf.7
varImpPlot(rf.7)
# Occuracy goes down when Parch combined with family.size

test.submit.df <- dat.combined[892:1309, c("Pclass","Title","family.size")]
rf.5.pred <- predict(rf.5, test.submit.df)
table(rf.5.pred)

submit.data <- data.frame(PassengerID = rep(892:1309), Survived= rf.5.pred)
write.csv(submit.data, file="Submit_CSV_TITANIC.csv", row.names = FALSE)



library(caret)
library(doSNOW)

set.seed(2348)
cv.10.fold <- createMultiFolds(rf.label, k=10, times =10)

table(rf.label)
table(rf.label[cv.10.fold[[33]]])
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.fold)
c1<- makeCluster(4, type = "SOCK")
registerDoSNOW(c1)

set.seed(34324)
rf.5.cv.1 <- train(x= rf.train.5, y= rf.label, method="rf", 
                   tuneLength=3, ntree=1000, trControl=ctrl.1)
stopCluster(c1)

rf.5.cv.1
rf.5

set.seed(2348)
cv.10.fold <- createMultiFolds(rf.label, k=3, times =10)
ctrl.2 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.10.fold)
c1<- makeCluster(4, type = "SOCK")
registerDoSNOW(c1)

set.seed(34324) 
rf.5.cv.2 <- train(x= rf.train.5, y= rf.label, method="rf", 
                   tuneLength=3, ntree=1000, trControl=ctrl.2)
stopCluster(c1)

rf.5.cv.2

library(rpart)
library(rpart.plot)
rpart.cv <- function(seed, training, labels, ctrl){
  c1 <- makeCluster(4, "SOCK")
  registerDoSNOW(c1)
  set.seed(seed)
  rpart.cv <-train(x=training, y= labels, method = "rpart", tuneLength=30, trControl = ctrl)
  stopCluster(c1)
  return (rpart.cv)
}

features <- c("Pclass", "Title", "family.size")
rpart.train.1 <- dat.combined[1:891, features]
rpart.1.cv1 <- rpart.cv(94622, rpart.train.1, rf.label,ctrl.2)
rpart.1.cv1

prp(rpart.1.cv1$finalModel, type=0, extra=1,under = TRUE)
