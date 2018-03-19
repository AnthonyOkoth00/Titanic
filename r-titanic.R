library(Hmisc)
library(knitr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(gridExtra)
library(ROCR)
library(corrplot)
train <- read.csv("./train.csv", stringsAsFactors = F, na.strings = c("NA", ""))
test <- read.csv("./test.csv", stringsAsFactors = F, na.strings = c("NA", ""))
str(train)
test$Survived <- NA
all <- rbind(train, test)
##3.3 Completeness of the data
sapply(all, function(x) {sum(is.na(x))})
##3.4 Exploring some of the most important variables
all$Sex <- as.factor(all$Sex)
all$Survived <- as.factor(all$Survived)
all$Pclass <- as.ordered(all$Pclass) #because pclass is ordinal
###3.4.1 The response variable; Survived
  ggplot(all[!is.na(all$Survived), ], aes(x = Survived, fill = Survived)) + geom_bar(stat = "count") +
  labs(x = "How many people died and survived on the titanic?") + 
  geom_label(stat = "count", aes(label = ..count..), size = 7) + 
  theme_grey(base_size = 18)
##3.4.2 Sex/gender
  p1 <- ggplot(all, aes(x = Sex, fill = Sex)) + geom_bar(stat = "count", position = "dodge") +
  theme_grey() + labs(x = "All data") + geom_label(stat = "count", aes(label = ..count..)) +
  scale_fill_manual("legend", values = c("female" = "pink", "male" = "green"))
p2 <- ggplot(all[!is.na(all$Survived), ], aes(x = Sex, fill = Survived)) +
  geom_bar(stat = "count", position = "dodge") + theme_grey() + labs(x = "Training data only") +
  geom_label(stat = "count", aes(label = ..count..))
grid.arrange(p1, p2, nrow = 1)


