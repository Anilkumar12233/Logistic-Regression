library(ggplot2)
df.train<- read.csv("~/Logistic Regression/train.csv")
print(head(df.train))
print(str(df.train))

#exploratory Data Analysis

#visualize the count of survivors and non-survivors based on the "Survived" variable.
ggplot(df.train,aes(Survived))+geom_bar(fill = "darkturquoise")

#To show the count of passengers in each passenger class based on the "Pclass" variable.
ggplot(df.train,aes(Pclass))+geom_bar(aes(fill=factor(Pclass)))

#bar plot to display the count of passengers for each gender based on the "Sex" variable.
ggplot(df.train,aes(Sex))+geom_bar(aes(fill=factor(Sex)))

#histogram to visualize the distribution of passenger ages based on the "Age" variable.
ggplot(df.train,aes(Age))+geom_histogram(bars=30,alpha=0.5,fill="darkturquoise")

#histogram to visualize the distribution of passenger fares based on the "Fare" variable 
ggplot(df.train,aes(Fare))+geom_histogram(fill="green",color="black",alpha=0.5)

# Exploration of Age
density_plot <- ggplot(df.train, aes(x = Age, fill = factor(Survived), color = factor(Survived))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) +
  scale_color_manual(values = c("darkturquoise", "lightcoral")) +
  labs(title = "Density Plot of Age for Surviving and Deceased Populations", x = "Age") +
  xlim(-10, 85) +
  theme_bw()

# Display the density plot
print(density_plot)

#Exploration of Fare
density_plot <- ggplot(final_train, aes(x = Fare, fill = factor(Survived), color = factor(Survived))) +
  geom_density(alpha = 0.5, position = "fill") +
  scale_fill_manual(values = c("lightcoral", "darkturquoise")) +
  scale_color_manual(values = c("lightcoral", "darkturquoise")) +
  labs(title = "Density Plot of Fare for Surviving and Deceased Populations", x = "Fare") +
  xlim(-20, 200) +
  theme_bw()

# Display the density plot
print(density_plot)

#Exploration of Embarked Port
bar_plot2 <- ggplot(df.train, aes(x = Embarked, fill = factor(Survived))) +
  geom_bar() +
  labs(title = "Survivors by Embarked Port", x = "Embarked Port", y = "Count") +
  scale_fill_manual(values = c("#FF0000", "#00FF00"), labels = c("Died", "Survived")) +
  theme_bw()

# Display the bar plot
print(bar_plot2)


pl<-ggplot(df.train,aes(Pclass,Age))
pl<-pl+geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4))
pl+scale_y_continuous(breaks = seq(min(0),max(80),by=2))

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




