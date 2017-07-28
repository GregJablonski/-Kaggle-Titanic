library(tidyverse)
library(stringr)
setwd("C:\\Users\\grjablon\\Desktop\\git\\KaggleTitanic")


train <- read_csv("train.csv")
test <- read_csv("test.csv")
full <- bind_rows(train, test)

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)

Rare_title <- c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir", "the Countess")

full$Title[full$Title == "Mlle"] <- "Miss" 
full$Title[full$Title == "Mme"] <- "Mrs" 
full$Title[full$Title == "Ms"] <- "Miss" 
full$Title[full$Title %in% Rare_title] <- "Rare Title" 

full$Surname <- str_extract(full$Name, '^([^,]*)' )

full$Family <- full$SibSp

full$Family[full$Family >= 2] <- "Big Family"
full$Family[full$Family == 0] <- "Single"
full$Family[full$Family == 1] <- "Pair"

full$Deck <- sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1])

full$Age[is.na(full$Age)] <- mean(full$Age, na.rm = 'TRUE') 
full$Embarked[is.na(full$Embarked)] <- "C"
full$Fare[is.na(full$Fare)] <- median(full$Fare[full$Pclass == '3' & full$Embarked == 'S'], na.rm = TRUE)
