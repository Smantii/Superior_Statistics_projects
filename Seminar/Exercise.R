#Reading Data
#Data File
data_raw <- read.csv("~/Downloads/Data/spambase.data", header = F)

#Names File
library(readr)
data_raw_names <- read.delim("~/Downloads/Data/spambase.names", header = FALSE)
data_raw_names <- data_raw_names[-(1:30),]
data_raw_names <- as.data.frame(data_raw_names)

library(dplyr)
library(tidyr)
data_raw_names <- data_raw_names %>%
  separate(data_raw_names, c("Variable", "Type"), sep = ":")

#Assigning Name to Dataset
names(data_raw) <- data_raw_names$Variable
names(data_raw)[is.na(names(data_raw))] <- "classes"


data <- data_raw

#Checking for missing values
any(is.na(data))

#Renaming levels of Diagnosis Column
data$classes <- as.factor(data$classes)
data$classes <- recode(data$classes, "0" = "Not Spam", "1" = "Spam")
summary(data$classes)


data$classes = 2*data$classes - 1


#shuffle
set.seed(42)
rows <- sample(nrow(data))
data = data[rows,]

#data stratification
set.seed(42)
library(caret)
train.index <- createDataPartition(data$classes, p = .85, list = FALSE)
train_x <- data[train.index,1:57]
train_y <- data[train.index,58]
test_x <- data[-train.index,1:57]
test_y <- data[-train.index,58]
train_x = scale(train_x)
test_x = scale(test_x)


set.seed(42)
train_control <- trainControl(method = "cv", number = 5)
