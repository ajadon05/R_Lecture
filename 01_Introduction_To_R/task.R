library(dplyr)
library(nnet)
library(purrr)

### Loading Libraries
library(readr)

### Loading Data
train <- read_csv("Data/train.csv")
summary(train)

str(train)

### First Part ###
#1) TASK: Change Sex feature from a character data type to a factor data type



#2) TASK: Write a function that changes all character data types, 
#   to a factor data type 

character_to_factor <- function(x) {
  
}

### Second Part ###
#3) TASK: Count how many distinct groups exist in the Sex column
#   HINT: You can use base R or with a help of a package



#4) TASK: For Sex feature, count how often is each group represented in the 
#   data set. 


#5) TASK: Rename the Sex feature to a Target.



### Third Part ###
#6) TASK: How many NAs are there in the train data set? 



#7) TASK: How many NAs are there per each feature? Store the result into a 
#   list data type. Name the list: NA_Counter. 



#8) TASK: Create a mean imputation function, that imputes the NA values only
#   in those columns that are numeric. 


NA_imputer_train <- function(dataframe){
  
}


### Fourt Part ###
#9) TASK: Create a new feature under the name Target_Compare, that is a replica 
#   of the Target feature in the training set. 



#10) TASK: By utilizing descriptive statistics & visualization try to predict 
#   whether Target feature is either M, I or F. Use all kinds of conditionals
#   that will enable you to predict correctly M, I or F. 

#   Below you will find examples of visualization with ggplot2 and how to create
#   a MyAlgorithm feature. 


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




### Sixth Part ###
#Loading Test set

#12) TASK: 
#    For each numeric feature in the training set calculate their mean 
#    value, then impute the missing values in test set based on the calculated
#    mean value of the training set. Note that the target feature is Sex,
#    so that means you have to rename it to Target, so it is same as in training set. 




#13) TASK: Use the already pre-built function character_to_factor and change the
#    all character data types to a factor data type. 
#    Or use any other more simpler method for changing a feature's data type. 



#14) TASK: Copy and paste the definition of your MyAlgorithm feature into the
#    test data set. Then evaluate the result, same as in the train part. 




