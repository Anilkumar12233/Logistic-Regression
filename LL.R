df.train<- read.csv("~/Logistic Regression/train.csv")
print(head(df.train))
print(str(df.train))




imputate_age <- function(Age,Pclass){
  
  out <- Age
  for (i in 1:length(Age)) {
    if (is.na(Age[i])) {
      if (Pclass[i] == 1) {
        out[i] <- 37
      } else if (Pclass[i] == 2) {
        out[i] <- 29
      } else {
        out[i] <- 24
      }
    } else {
      out[i] <- Age[i]
    }
  }
  return(out)
}

fixed_ages <- imputate_age(df.train$Age, df.train$Pclass)
df.train$Age <- fixed_ages
missmap(df.train, col = c("Yellow", "black"))
df.train <- select(df.train, -PassengerId,-Name,-Ticket,-Cabin)
head(df.train)
df.train$Survived<-factor(df.train$Survived)
df.train$Embarked<-factor(df.train$Embarked) 
df.train$Pclass<-factor(df.train$Pclass)
df.train$Parch<-factor(df.train$Parch)
df.train$SibSp<-factor(df.train$SibSp)
log.model<- glm(Survived~., family = binomial(link = "logit"),data = df.train)
summary(log.model)

set.seed(101)
split <- sample.split(df.train$Survived,SplitRatio = 0.7)
final.train <- subset(df.train,split==TRUE)
final.test<- subset(df.train,split==FALSE)

final.log.model <- glm(Survived~., family = binomial(link="logit"),data= final.train)
summary(final.log.model)

probability <- predict.glm(final.log.model,final.test,type="response")
results <- ifelse(probability>0.5,1,0)
misClass <- mean(results != final.test$Survived)
print(1-misClass)




