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

##4.2.3 Families; what about uncles, aunts, cousins, nieces, grandparents, brothers/sisters-in law?
kable(all[all$Ticket %in% c("29104", "29105", "29106"), c(2, 3, 4, 5, 6, 7, 8, 9, 15)])
NC <- all[all$FsizeName %in% SizeCheck$FsizeName, ] #create data frame with only relevant Fsizenames
#extracting maiden names
NC$Name <- sub("\\s$", "", NC$Name) #removing spaces at the end of Name
NC$Maiden <- sub(".*[^\\)]$", "", NC$Name) #remove when not ending with ")"
NC$Maiden <- sub(".*\\s(.*)\\)$", "\\1", NC$Maiden)
NC$Maiden[NC$Title != "Mrs"] <- "" #cleaning up other stuff between brackets 8sometimes nichmae of a Mr.)
NC$Maiden <- sub("^\\(", '', NC$Maiden) #removing opening brackets (sometimes single name, no spaces between brackets)
# making an exceptions match
NC$Maiden[NC$Name == "Andersen-Jensen, Miss. Carla Christine Nielsine"] <- "Jensen"
#take only Maiden names that also exist as surname in other Observations
NC$Maiden2[NC$Maiden %in% NC$Surname] <- NC$Maiden[NC$Maiden %in% NC$Surname]
#create surname+maiden name combinations
NC$Combi[!is.na(NC$Maiden2)] <- paste(NC$Surname[!is.na(NC$Maiden2)], NC$Maiden[!is.na(NC$Maiden2)])
#create labels dataframe with surname and maiden merged into one column
labels1 <- NC[!is.na(NC$Combi), c("Surname", "Combi")]
labels2 <- NC[!is.na(NC$Combi), c("Maiden", "Combi")]
colnames(labels2) <- c("Surname", "Combi")
labels1 <- rbind(labels1, labels2)
NC$Combi <- NULL
NC <- left_join(NC, labels1, by = "Surname")
#Find the maximum Fsize within each newly found 'second degree' family
CombiMaxF <- NC[!is.na(NC$Combi), ] %>% group_by(Combi) %>% summarise(MaxF = max(Fsize))
NC <- left_join(NC, CombiMaxF, by = "Combi")
#create family names for those larger families
NC$FsizeCombi[!is.na(NC$Combi)] <- paste(as.character(NC$Fsize[!is.na(NC$Combi)]), NC$Combi[!is.na(NC$Combi)], sep = "")
#find the ones in which not all Fsizes are the same
FamMaid <- NC[!is.na(NC$FsizeCombi), ] %>% group_by(FsizeCombi, MaxF, Fsize) %>% summarise(NumObs = n())
FamMaidWrong <- FamMaid[FamMaid$MaxF != FamMaid$NumObs, ]
kable(unique(NC[!is.na(NC$Combi) & NC$FsizeCombi %in% FamMaidWrong$FsizeCombi, c("Combi", "MaxF")]))
NC$MaxF <- NULL #erasing MaxF column maiden combis's
#Find the maximum Fsize within remaining families (no maiden combi's)
FamMale <- NC[is.na(NC$Combi), ] %>% group_by(Surname) %>% summarise(MaxF = max(Fsize))
NC <- left_join(NC, FamMale, by = "Surname")
NCMale <- NC[is.na(NC$Combi), ] %>% group_by(Surname, FsizeName, MaxF) %>% summarise(count = n()) %>% group_by(Surname, MaxF) %>% filter(n() > 1) %>% summarise(NumFsizes = n())
NC$Combi[NC$Surname %in% NCMale$Surname] <- NC$Surname[NC$Surname %in% NCMale$Surname]
kable(NCMale[, c(1, 2)])
#selecting those 37 passengers In Not Correct dataframe
NC <- NC[(NC$FsizeCombi %in% FamMaidWrong$FsizeCombi) | (NC$Surname %in% NCMale$Surname), ]
#calculating the average Fsize for those 9 families
NC1 <- NC %>% group_by(Combi) %>% summarise(Favg = mean(Fsize))
kable(NC1)
NC <- left_join(NC, NC1, by = "Combi") #adding Favg to NC dataframe
NC$Favg <- round(NC$Favg) #rounding those averages to integers
NC <- NC[, c("PassengerId", "Favg")]
all <- left_join(all, NC, by = "PassengerId")
#replacing Fsize by Favg
all$Fsize[!is.na(all$Favg)] <- all$Favg[!is.na(all$Favg)]
###4.2.4 Can we still find more second degree families?
#creating a variable with almost the same ticket numbers (only last 2 digits varying)
all$Ticket2 <- sub("..$", "xx", all$Ticket)
rest <- all %>% select(PassengerId, Title, Age, Ticket, Ticket2, Surname, Fsize) %>% filter(Fsize == "1") %>% group_by(Ticket2, Surname) %>% summarise(count = n())
rest <- rest[rest$count > 1, ]
rest1 <- all[(all$Ticket2 %in% rest$Ticket2 & all$Surname %in% rest$Surname & all$Fsize == "1"), c("PassengerId", "Surname", "Title", "Age", "Ticket", "Ticket2", "Fsize", "SibSp", "Parch")]
rest1 <- left_join(rest1, rest, by = c("Surname", "Ticket2"))
rest1 <- rest1[!is.na(rest1$count), ]
rest1 <- rest1 %>% arrange(Surname, Ticket2) 
kable(rest1[1:12, ])
#replacing Fsize size in my overall dataframe with the count numbers in the table above
all <- left_join(all, rest1)
for (i in 1:nrow(all)) { if (!is.na(all$count[i])) { all$Fsize[i] <- all$count[i]}}


####4.2.4 Can we still find more second degree families?
###4.2.5 Did people book together?
kable(all[all$Ticket == "1601", c("Survived", "Pclass", "Title", "Surname", "Age", "Ticket", "SibSp", "Parch", "Fsize")])

#composing data frame with group size for each Ticket
TicketGroup <- all %>% select(Ticket) %>% group_by(Ticket) %>% summarise(Tsize = n())
all <- left_join(all, TicketGroup, by = "Ticket")
ggplot(all[!is.na(all$Survived), ], aes(x = Tsize, fill = Survived)) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Ticket Size") + theme_grey()
#taking the max of family and ticket size as the group size
all$Group <- all$Fsize
for (i in 1:nrow(all)) {
    all$Group[i] <- max(all$Group[i], all$Tsize)
  }
#Creating final group categories
all$GroupSize[all$Group == 1] <- "solo"
all$GroupSize[all$Group == 2] <- "duo"
all$GroupSize[all$Group == 3 & all$Group <= 4] <- "group"
all$GroupSize[all$Group >= 5] <- "large group"
all$GroupSize <- as.factor(all$GroupSize)

g1 <- ggplot(all[!is.na(all$Survived),], aes(x = Group, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Final Group Sizes') + theme_grey()
g2 <- ggplot(all[!is.na(all$Survived),], aes(x = GroupSize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Final Group Categories') + theme_grey() +
  scale_x_discrete (limits = c('solo', 'duo', 'group', 'large group'))
grid.arrange(g2, g1)