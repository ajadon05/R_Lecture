library(dplyr)
library(nnet)
library(purrr)


if(!require(dplyr)){
  install.packages("dplyr")
} else {
  library(dplyr)
}
#library() loads a package, and require() tries to load a package. 
#â€śif you failed to try to load this package, please failâ€ť

#1
### Loading Libraries
library(readr)

### Loading Data
train <- read_csv("Data/train.csv")
summary(train)

str(train)

### First Part ###
#1) TASK: Change Sex feature from a character data type to a factor data type
train$Sex <- as.factor(train$Sex)

train %>%
  mutate(Sex = as.factor(Sex))

mutate(train, Sex = as.factor(Sex))

#2) TASK: Write a function that changes all character data types, 
#   to a factor data type 
character_to_factor <- function(dataframe) {
  
  map_df(dataframe, function(element) {
    
    if(is.character(element)) {
      as.factor(element)
    } else {
      element
    }
    
  })
}

character_to_factor <- function(dataframe) {
  
  map_df(dataframe, function(element) {
    
    if(is.character(element)) {
      as.factor(element)
    } else {
      element
    }
  })
}


str(train)
train <- character_to_factor(train)
head(train)

### OR ###
factor_df <- train %>%
  select_if(is.factor)

df_names = names(train)[sapply(train, is.factor)]

factor_df <- train[, df_names]

### Second Part ###
#3) TASK: Count how many distinct groups exist in the Sex column
#   HINT: You can use base R or with a help of a package
length(unique(train$Sex))

train %>%
  summarise(CountDistinctNumberOfGroups = n_distinct(Sex))

#4) TASK: For Sex feature, count how often is each group represented in the 
#   data set. 
table(train$Sex)

train %>%
  group_by(Sex) %>%
  summarise(Count = n()) 

#5) TASK: Rename the Sex feature to a Target.
#One way
train$Target <- train$Sex
train$Sex <- NULL

#Second Way
names(train)[names(train) == "Sex"] <- "Target"
names(train)

#Third Way
train <- train %>%
  rename(
    Target = Sex
  )




### Third Part ###
#6) TASK: How many NAs are there in the train data set? 
sum(is.na(train))

table(is.na(train))

#7) TASK: How many NAs are there per each feature? Store the result into a 
#   list data type. Name the list: NA_Counter. 

NA_Counter <- map_df(train, function(x) {
  
  sum(is.na(x))
  
})

NaVector <- sapply(train, function(x) {
  sum(is.na(x))
})
str(NaVector)
#types of outputs: 
#sapply is for a vector
#lapply is for a list
#map is for a list as well
#map_df is for a data frame


#8) TASK: Create a mean imputation function, that imputes the NA values only
#   in those columns that are numeric. 

NA_imputer_train <- function(train) {
  
  map_df(train, function(Length) {
    
    if(is.numeric(Length)) {
      ifelse(is.na(Length), mean(Length, na.rm = TRUE), element) 
    } else { 
      element 
    }
  })
}


train <- NA_imputer_train(train)
sum(is.na(train))

### Fourt Part ###
#9) TASK: Create a new feature under the name Target_Compare, that is a replica 
#   of the Target feature in the training set. 

#Example
library(ggplot2)

ggplot(data = train, mapping = aes(Target)) +
  geom_bar()

ggplot(data = train, mapping = aes(x = Height, y = Diameter, colour = Target)) +
  geom_point() 

train <- train %>%
  mutate(MyAlgorithm = ifelse(Height < 0.10, 'I', 
                              ifelse(Height > 0.2, 'F', 'M')))

train %>%
  group_by(Target) %>%
  summarise(n())


train %>%
  group_by(MyAlgorithm) %>%
  summarise(n())

#Examples of ifelse statements
#1
#ifelse(<condition>, <yes>, ifelse(<condition>, <yes>, <no>))

#2
#ifelse(<condition>, ifelse(<condition>, <yes>, <no>), <no>)

#3
#ifelse(<condition>, 
#       ifelse(<condition>, <yes>, <no>), 
#       ifelse(<condition>, <yes>, <no>)
#)

#4
#ifelse(<condition>, <yes>, 
#       ifelse(<condition>, <yes>, 
#              ifelse(<condition>, <yes>, <no>)
#       )
#)

### Fifth Part ###
#11) TASK: Validate your accuracy on the training set between Target and 
#    MyAlgorithm feature. 
#    You can do that by creating a new column (AccuracyOfMyAlgorithm) that 
#    checks if the values of Target and Target Compare are the same row wise. 

train <- train %>%
  mutate(AccuracyOfMyAlgorithm = ifelse(Target == MyAlgorithm, 1, 0))

sum(train$AccuracyOfMyAlgorithm)/nrow(train)

train %>%
  filter(AccuracyOfMyAlgorithm == 1) %>%
  View()
