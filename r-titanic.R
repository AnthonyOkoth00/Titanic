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

###3.4.3 Passenger Class
p3 <- ggplot(all, aes(x = Pclass, fill = Pclass)) + geom_bar(stat = "count", position = "dodge") +
  labs(x = "Pclass, All data") + geom_label(stat = "count", aes(label = ..count..)) +
  theme(legend.position = "none") + theme_grey()
p4 <- ggplot(all[!is.na(all$Survived), ], aes(x = Pclass, fill = Survived)) + geom_bar(stat = "count", position = "dodge") +
labs(x = "Training data only") + theme(legend.position = "none") + theme_grey()
p5 <- ggplot(all[!is.na(all$Survived), ], aes(x = Pclass, fill = Survived)) + geom_bar(stat = "count", position = "stack") +
  labs(x = "Training data only", y = "Count") + facet_grid(.~Sex) + theme(legend.position = "none") + theme_gray()
p6 <- ggplot(all[!is.na(all$Survived), ], aes(x = Pclass, fill = Survived)) + geom_bar(stat = "count", position = "fill") +
  labs(x = "Training data only", y = "Percent") + facet_grid(.~Sex) + theme(legend.position = "none") + theme_gray()
grid.arrange(p3, p4, p5, p6, ncol = 2)
all$Pclass[all$Pclass == "1" & all$Sex == "male"] <- "P1Male"
all$PclassSex[all$Pclass == "1" & all$Sex == "male"] <- "P1Male"
all$PclassSex[all$Pclass == "2" & all$Sex == "male"] <- "P2Male"
all$PclassSex[all$Pclass == "3" & all$Sex == "male"] <- "P3Male"
all$PclassSex[all$Pclass == "1" & all$Sex == "female"] <- "P1Female"
all$PclassSex[all$Pclass == "2" & all$Sex == "female"] <- "P2Female"
all$PclassSex[all$Pclass == "3" & all$Sex == "female"] <- "P3Female"
all$PclassSex <- as.factor(all$PclassSex)


# 4 Feature engineering
#4.1 Creating the Title variable
#Extracting Title and Surname from Name
all$Surname <- sapply(all$Name, function(x) {strsplit(x, split = "[,.]")[[1]][1]})
#correcting some surnames that also include a maiden name
all$Surname <- sapply(all$Surname, function(x) {strsplit(x, split = "[-]")[[1]][1]})
all$Title <- sapply(all$Name, function(x) {strsplit(x, split = "[,.]")[[1]][2]})
all$Title <- sub(" ", "", all$Title) #removing spaces before title
kable(table(all$Sex, all$Title))

all$Title[all$Title %in% c("Mlle", "Ms")] <- "Miss"
all$Title[all$Title == "Mme"] <- "Mrs"
all$Title[!(all$Title %in% c("Master", "Miss", "Mr", "Mrs"))] <- "Rare Title"
all$Title <- as.factor(all$Title)
kable(table(all$Sex, all$Title))

ggplot(all[!is.na(all$Survived), ], aes(x = Title, fill = Survived)) +
  geom_bar(stat = "count", position = "stack") +
  labs(x = "Title") + theme_grey()

###4.2 Finding groups of people traveling together
##4.2.1 Families; siblings, spouses, parents and children
#creating family size variable (Fsize)
all$Fsize <- all$SibSp + all$Parch + 1
ggplot(all[!is.na(all$Survived), ], aes(x = Fsize, fill = Survived)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = "Family Size") + theme_grey()


#4.2.2 Family Size inconsistencies, and correcting the effects of a cancellation
#composing variable that combines total Fsize and Surname
all$FsizeName <- paste(as.character(all$Fsize), all$Surname, sep = "")
SizeCheck <- all %>% group_by(FsizeName, Fsize) %>% summarise(NumObs = n())
SizeCheck$NumFam <- SizeCheck$NumObs / SizeCheck$Fsize
SizeCheck$modula <- SizeCheck$NumObs %% SizeCheck$Fsize
SizeCheck <- SizeCheck[SizeCheck$modula != 0, ]
sum(SizeCheck$NumObs) #Total number of observations with inconsistencies

> kable(SizeCheck[SizeCheck$FsizeName %in% c("3Davies", "5Hocking", "6Richards", "2Wilkes", "3Richards", "4Hocking"), ]) #only display some inconsistencies that are discussed in the text
kable(all[all$FsizeName == "3Davies", c(2, 3, 14, 5, 6, 7, 8, 17, 9, 15)])
all$FsizeName[ca(550, 1222)] <- "2Davies"
all$FsizeName[c(550, 1222)] <- "2Davies"
all$SibSp[550] <- 0
all$Parch[1222] <- 1
all$Fsize[c(550, 1222)] <- 2
kable(all[all$FsizeName == "2Davies", c( 2, 3, 14, 5, 6, 7, 8, 17, 9, 15)])
