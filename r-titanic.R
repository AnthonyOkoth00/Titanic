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
all$FsizeName <- paste(as.character(all$Fsize), all$Surname, sep="")

SizeCheck <- all %>%
  group_by(FsizeName, Fsize) %>%
  summarise(NumObs=n())
SizeCheck$NumFam <- SizeCheck$NumObs/SizeCheck$Fsize
SizeCheck$modulo <- SizeCheck$NumObs %% SizeCheck$Fsize
SizeCheck <- SizeCheck[SizeCheck$modulo !=0,]
sum(SizeCheck$NumObs) #total number of Observations with inconsistencies
kable(SizeCheck[SizeCheck$FsizeName %in% c('3Davies', '5Hocking', '6Richards', '2Wilkes', '3Richards', '4Hocking'),]) #only display some inconsistencies that are discussed in the text

kable(all[all$FsizeName=='3Davies',c(2,3,14,5,6,7,8,17,9,15)])
all$FsizeName[c(550, 1222)] <- '2Davies'
all$SibSp[550] <- 0
all$Parch[1222] <- 1
all$Fsize[c(550, 1222)] <- 2
kable(all[all$FsizeName=='2Davies',c(2,3,14,5,6,7,8,17,9,15)])

##4.2.3 Families; what about uncles, aunts, cousins, nieces, grandparents, brothers/sisters-in law?
kable(all[all$Ticket %in% c('29104', '29105', '29106'),c(2,3,4,5,6,7,8,9,15)])

NC <- all[all$FsizeName %in% SizeCheck$FsizeName,] #create data frame with only relevant Fsizenames
#extracting maiden names
NC$Name <- sub("\\s$", "", NC$Name) #removing spaces at end Name
NC$Maiden <- sub(".*[^\\)]$", "", NC$Name) #remove when not ending with ')'
NC$Maiden <- sub(".*\\s(.*)\\)$", "\\1", NC$Maiden)
NC$Maiden[NC$Title!='Mrs'] <- "" #cleaning up other stuff between brackets (including Nickname of a Mr)
NC$Maiden <- sub("^\\(", '', NC$Maiden) #removing opening brackets (sometimes single name, no spaces between brackets)
#making an exceptions match
NC$Maiden[NC$Name=='Andersen-Jensen, Miss. Carla Christine Nielsine'] <- 'Jensen'

#take only Maiden names that also exist as surname in other Observations
NC$Maiden2[NC$Maiden %in% NC$Surname] <- NC$Maiden[NC$Maiden %in% NC$Surname] 
#create surname+maiden name combinations
NC$Combi[!is.na(NC$Maiden2)] <- paste(NC$Surname[!is.na(NC$Maiden2)], NC$Maiden[!is.na(NC$Maiden2)])

#create labels dataframe with surname and maiden merged into one column
labels1 <- NC[!is.na(NC$Combi), c('Surname','Combi')]
labels2 <- NC[!is.na(NC$Combi), c('Maiden','Combi')]
colnames(labels2) <- c('Surname', 'Combi')
labels1 <- rbind(labels1, labels2)

NC$Combi <- NULL
NC <- left_join(NC, labels1, by='Surname')

#Find the maximum Fsize within each newly found 'second degree' family
CombiMaxF <- NC[!is.na(NC$Combi),] %>%
  group_by(Combi) %>%
  summarise(MaxF=max(Fsize)) #summarise(MaxF=n())
NC <- left_join(NC, CombiMaxF, by = "Combi")

#create family names for those larger families
NC$FsizeCombi[!is.na(NC$Combi)] <- paste(as.character(NC$Fsize[!is.na(NC$Combi)]), NC$Combi[!is.na(NC$Combi)], sep="")

#find the ones in which not all Fsizes are the same
FamMaid <- NC[!is.na(NC$FsizeCombi),] %>%
  group_by(FsizeCombi, MaxF, Fsize) %>%
  summarise(NumObs=n())
FamMaidWrong <- FamMaid[FamMaid$MaxF!=FamMaid$NumObs,]

kable(unique(NC[!is.na(NC$Combi) & NC$FsizeCombi %in% FamMaidWrong$FsizeCombi, c('Combi', 'MaxF')]))

NC$MaxF <- NULL #erasing MaxF column maiden combi's

#Find the maximum Fsize within remaining families (no maiden combi's)
FamMale <- NC[is.na(NC$Combi),] %>%
  group_by(Surname) %>%
  summarise(MaxF=max(Fsize))
NC <- left_join(NC, FamMale, by = "Surname")

NCMale <- NC[is.na(NC$Combi),] %>%
  group_by(Surname, FsizeName, MaxF) %>%
  summarise(count=n()) %>%
  group_by(Surname, MaxF) %>%
  filter(n()>1) %>%
  summarise(NumFsizes=n())

NC$Combi[NC$Surname %in% NCMale$Surname] <- NC$Surname[NC$Surname %in% NCMale$Surname]

kable(NCMale[, c(1,2)])

kable(all[all$Surname=='Vander Planke', c(2,3,4,5,6,7,8,9,15)])


#selecting those 37 passengers In Not Correct dataframe
NC <- NC[(NC$FsizeCombi %in% FamMaidWrong$FsizeCombi)|(NC$Surname %in% NCMale$Surname),]

#calculating the average Fsize for those 9 families
NC1 <- NC %>%
  group_by(Combi) %>%
  summarise(Favg=mean(Fsize))
kable(NC1)

NC <- left_join(NC, NC1, by = "Combi") #adding Favg to NC dataframe 
NC$Favg <- round(NC$Favg) #rounding those averages to integers
NC <- NC[, c('PassengerId', 'Favg')]
all <- left_join(all, NC, by='PassengerId')

#replacing Fsize by Favg
all$Fsize[!is.na(all$Favg)] <- all$Favg[!is.na(all$Favg)]

###4.2.4 Can we still find more second degree families?
#creating a variable with almost the same ticket numbers (only last 2 digits varying)
all$Ticket2 <- sub("..$", "xx", all$Ticket)
rest <- all %>%
  select(PassengerId, Title, Age, Ticket, Ticket2, Surname, Fsize) %>%
  filter(Fsize=='1') %>%
  group_by(Ticket2, Surname) %>%
  summarise(count=n())
rest <- rest[rest$count>1,]
rest1 <- all[(all$Ticket2 %in% rest$Ticket2 & all$Surname %in% rest$Surname & all$Fsize=='1'), c('PassengerId', 'Surname', 'Title', 'Age', 'Ticket', 'Ticket2', 'Fsize', 'SibSp', 'Parch')]
rest1 <- left_join(rest1, rest, by = c("Surname", "Ticket2"))
rest1 <- rest1[!is.na(rest1$count),]
rest1 <- rest1 %>%
  arrange(Surname, Ticket2)
kable(rest1[1:12,])

#replacing Fsize size in my overall dataframe with the count numbers in the table above
all <- left_join(all, rest1)
for (i in 1:nrow(all)){
  if (!is.na(all$count[i])){
    all$Fsize[i] <- all$count[i]
  }
}


####4.2.4 Can we still find more second degree families?
###4.2.5 Did people book together?
kable(all[all$Ticket=='1601', c('Survived', 'Pclass', 'Title', 'Surname', 'Age', 'Ticket', 'SibSp', 'Parch', 'Fsize')])

#composing data frame with group size for each Ticket
#composing data frame with group size for each Ticket
TicketGroup <- all %>%
  select(Ticket) %>%
  group_by(Ticket) %>%
  summarise(Tsize=n())
all <- left_join(all, TicketGroup, by = "Ticket")

ggplot(all[!is.na(all$Survived),], aes(x = Tsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Ticket Size') + theme_grey()

#taking the max of family and ticket size as the group size
all$Group <- all$Fsize
for (i in 1:nrow(all)){
  all$Group[i] <- max(all$Group[i], all$Tsize[i])
}

#Creating final group categories
all$GroupSize[all$Group==1] <- 'solo'
all$GroupSize[all$Group==2] <- 'duo'
all$GroupSize[all$Group>=3 & all$Group<=4] <- 'group'
all$GroupSize[all$Group>=5] <- 'large group'
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


#clean up
all$count <- NULL
all$Name <- NULL
rm(CombiMaxF)
rm(FamMaid)
rm(FamMaidWrong)
rm(FamMale)
rm(labels1)
rm(labels2)
rm(NC)
rm(NC1)
rm(NCMale)
rm(rest)
#rm(rest1)
rm(SizeCheck)
rm(TicketGroup)
rm(p1); rm(p2); rm(p3); rm(p4); rm(p5); rm(p6)

##Dealing with the Fare variable

###Which data relevant to fare are missing?
#display passengers with missing Embarked
kable(all[which(is.na(all$Embarked)),c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group') ])
all$FarePP <- all$Fare/all$Tsize #creating the Fare Per Person variable

tab2 <- all[(!is.na(all$Embarked) & !is.na(all$Fare)),] %>%
  group_by(Embarked, Pclass) %>%
  summarise(FarePP=median(FarePP))
kable(tab2)
#imputing missing Embarked values
all$Embarked[all$Ticket=='113572'] <- 'C'
#converting Embarked into a factor
all$Embarked <- as.factor(all$Embarked)

#display passengers with missing Fare
kable(all[which(is.na(all$Fare)), c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group')])

all$FarePP[1044] <- 7.8

###The Fare Per Person Variable

tab3 <- all[(!is.na(all$FarePP)),] %>%
  group_by(Pclass) %>%
  summarise(MedianFarePP=median(FarePP))
all <- left_join(all, tab3, by = "Pclass")
all$FarePP[which(all$FarePP==0)] <- all$MedianFarePP[which(all$FarePP==0)]

ggplot(all, aes(x=FarePP)) +
  geom_histogram(binwidth = 5, fill='blue') + theme_grey() +
  scale_x_continuous(breaks= seq(0, 150, by=10))

#Note Hmisc needs to be loaded before dplyr, as the other way around errors occured due to the kernel using the Hmisc summarize function instead of the dplyr summarize function
all$FareBins <- cut2(all$FarePP, g=5)

ggplot(all[!is.na(all$Survived),], aes(x=FareBins, fill=Survived))+
  geom_bar(stat='count') + theme_grey() + facet_grid(.~Pclass)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##Predicting missing Age values
ggplot(all[(!is.na(all$Survived) & !is.na(all$Age)),], aes(x = Age, fill = Survived)) +
  geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density and Age") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_grey()

ggplot(all[!is.na(all$Age),], aes(x = Title, y = Age, fill=Pclass )) +
  geom_boxplot() + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_grey()

#predicting Age with Linear Regression
set.seed(12000)
AgeLM <- lm(Age ~ Pclass + Sex + SibSp + Parch + Embarked + Title + GroupSize, data=all[!is.na(all$Age),])
summary(AgeLM)
all$AgeLM <- predict(AgeLM, all)

par(mfrow=c(1,2))
hist(all$Age[!is.na(all$Age)], main='Original data, non-missing', xlab='Age', col='green')
hist(all$AgeLM[is.na(all$Age)], main= 'LM NA predictions', xlab='Age', col='orange', xlim=range(0:80))

#display which passengers are predicted to be children (age<18) with Linear Regression.
all[(is.na(all$Age) & all$AgeLM <18), c('Sex', 'SibSp', 'Parch', 'Title', 'Pclass', 'Survived', 'AgeLM')]

#imputing Linear Regression predictions for missing Ages
indexMissingAge <- which(is.na(all$Age))
indexAgeSurvivedNotNA<- which(!is.na(all$Age) & (!is.na(all$Survived))) #needed in sections 4.6 and 4.7
all$Age[indexMissingAge] <- all$AgeLM[indexMissingAge]

#replacing NAs with imaginary Deck U, and keeping only the first letter of ech Cabin (=Deck)
all$Cabin[is.na(all$Cabin)] <- "U"
all$Cabin <- substring(all$Cabin, 1, 1)
all$Cabin <- as.factor(all$Cabin)

ggplot(all[(!is.na(all$Survived)& all$Cabin!='U'),], aes(x=Cabin, fill=Survived)) +
  geom_bar(stat='count') + theme_grey() + facet_grid(.~Pclass) + labs(title="Survivor split by class and Cabin")

c1 <- round(prop.table(table(all$Survived[(!is.na(all$Survived)&all$Cabin!='U')], all$Cabin[(!is.na(all$Survived)&all$Cabin!='U')]),2)*100)
kable(c1)

###4.6 How to deal with Children in the model?
ggplot(all[all$Age<14.5 & !is.na(all$Survived),], aes(x=Pclass, fill=Survived))+
  geom_bar(stat='count') + theme_grey(base_size = 18)

all$IsChildP12 <- 'No'
all$IsChildP12[all$Age<=14.5 & all$Pclass %in% c('1', '2')] <- 'Yes'
all$IsChildP12 <- as.factor(all$IsChildP12)


###4.7 What does Embarked tell us?
d1 <- ggplot(all[!is.na(all$Survived),], aes(x = Embarked, fill = Survived)) +
  geom_bar(stat='count') + theme_grey() + labs(x = 'Embarked', y= 'Count')
d2 <- ggplot(all[!is.na(all$Survived),], aes(x = Embarked, fill = Survived)) +
  geom_bar(stat='count', position= 'fill') + theme_grey() + labs(x = 'Embarked', y= 'Percent')

grid.arrange(d1, d2, nrow=1)

ggplot(all[indexAgeSurvivedNotNA,], aes(x = Age, fill = Survived)) +
  geom_histogram(aes(fill=factor(Survived))) + labs(title="Survival density, known-ages, and Embarked") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + theme_grey() + facet_grid(.~Embarked)

tab1 <- rbind(table(all$Embarked[!is.na(all$Survived)]),table(all$Embarked[indexAgeSurvivedNotNA]))
tab1 <- cbind(tab1, (rowSums(tab1)))
tab1 <- rbind(tab1, tab1[1,]-tab1[2,])
tab1 <- rbind(tab1, round((tab1[3,]/tab1[1,])*100))
rownames(tab1) <- c("All", "With Age", "Missing Age", "Percent Missing")
colnames(tab1) <- c("C", "Q", "S", "Total")
kable(tab1)

###4.8 Ticket survivors

TicketSurvivors <- all %>%
  group_by(Ticket) %>%
  summarize(Tsize = length(Survived),
            NumNA = sum(is.na(Survived)),
            SumSurvived = sum(as.numeric(Survived)-1, na.rm=T))
all <- left_join(all, TicketSurvivors)
## Joining, by = c("Ticket", "Tsize")
all$AnySurvivors[all$Tsize==1] <- 'other'
all$AnySurvivors[all$Tsize>=2] <- ifelse(all$SumSurvived[all$Tsize>=2]>=1, 'survivors in group', 'other')
all$AnySurvivors <- as.factor(all$AnySurvivors)

kable(x=table(all$AnySurvivors), col.names= c('AnySurvivors', 'Frequency'))

###4.9 Adding an “Is Solo” variable" based on Siblings and Spouse (SibSp) only
all$IsSolo[all$SibSp==0] <- 'Yes'
all$IsSolo[all$SibSp!=0] <- 'No'
all$IsSolo <- as.factor(all$IsSolo)

ggplot(all[!is.na(all$Survived),], aes(x = IsSolo, fill = Survived)) +
  geom_bar(stat='count') + theme_grey(base_size = 18)


###5 Predictions (with caret cross validation)
#splitting data into train and test set again
trainClean <- all[!is.na(all$Survived),]
testClean <- all[is.na(all$Survived),]

##5.1 Random Forest model
#install.packages('e1071', dependencies=TRUE)
set.seed(2017)
caret_matrix <- train(x=trainClean[,c('PclassSex', 'GroupSize', 'FarePP', 'AnySurvivors', 'IsChildP12')], y=trainClean$Survived, data=trainClean, method='rf', trControl=trainControl(method="cv", number=5))
caret_matrix




