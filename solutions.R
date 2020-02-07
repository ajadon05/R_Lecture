library(dplyr)
library(nnet)
library(purrr)


if(!require(dplyr)){
  install.packages("dplyr")
} else {
  library(dplyr)
}
#library() loads a package, and require() tries to load a package. 
#“if you failed to try to load this package, please fail”

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

str(train)
train <- character_to_factor(train)

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
train %>%
  group_by(Sex) %>%
  summarise(Count = n()) 

#5) TASK: Rename the Sex feature to a Target.
#One way
train$Target <- train$Sex
train$Sex <- NULL

#Second Way
names(train)[names(train) == "Sex"] <- "Target"

#Third Way
train <- train %>%
  rename(
    Target = Sex
  )


### Third Part ###
#6) TASK: How many NAs are there in the train data set? 
sum(is.na(train))

#7) TASK: How many NAs are there per each feature? Store the result into a 
#   list data type. Name the list: NA_Counter. 

NA_Counter <- as.data.frame(lapply(train, function(x) {
  
  sum(is.na(x))
  
}))


#8) TASK: Create a mean imputation function, that imputes the NA values only
#   in those columns that are numeric. 

NA_imputer_train <- function(dataframe) {
  
  map_df(dataframe, function(element) {
    
    if(is.numeric(element)) {
      ifelse(is.na(element), mean(element, na.rm = TRUE), element) 
    } else { 
      element 
    }
  })
}

train <- NA_imputer_train(train)

### Fourt Part ###
#9) TASK: Create a new feature under the name Target_Compare, that is a replica 
#   of the Target feature in the training set. 

#Example
library(ggplot2)

ggplot(data = train, mapping = aes(Target)) +
  geom_bar()

ggplot(data = train, mapping = aes(x = Height, y = Diameter, colour  = Target)) +
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

### Sixth Part ###
#Loading Test set

#12) TASK: 
#    For each numeric feature in the training set calculate their mean 
#    value, then impute the missing values in test set based on the calculated
#    mean value of the training set. Note that the target feature is Sex,
#    so that means you have to rename it to Target, so it is same as in training set. 
test <- read_csv("Data/test.csv")

target <- test$Sex

mean_imputed_values_trainining_set <- sapply(train[, -1], mean)

mean_imputed_values_trainining_set <- mean_imputed_values_trainining_set[-c(8,9)]


NA_imputer_test <- function(dataframe, imputed_values) {
  
  map2_dfr(dataframe, imputed_values,
           ~ replace(.x, is.na(.x), .y))
}

test <- NA_imputer_test(test[,-1], mean_imputed_values_trainining_set)



#13) TASK: Use the already pre-built function character_to_factor and change the
#    all character data types to a factor data type. 
#    Or use any other more simpler method for changing a feature's data type. 

test <- character_to_factor(test) 

#14) TASK: Copy and paste the definition of your MyAlgorithm feature into the
#    test data set. Then evaluate the result, same as in the train part. 

test <- test %>%
  mutate(MyAlgorithm = ifelse(Height < 0.10, 'I', 
                              ifelse(Height > 0.2, 'F', 'M')))

test <- test %>%
  mutate(AccuracyOfMyAlgorithm = ifelse(Target == MyAlgorithm, 1, 0))

sum(test$AccuracyOfMyAlgorithm)/nrow(test)

test %>%
  filter(AccuracyOfMyAlgorithm == 1) %>%
  View()

###

