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
