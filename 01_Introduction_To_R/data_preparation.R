df <- read.csv("Data/Abalone.csv")
library(purrr)

#df$Diameter[df$Diameter < 0.11] = NA


# Creating NA
df[, -1] <- map_df(df[, -1], function(dataframe) { 
  dataframe[sample(c(TRUE, NA), 
    prob = c(0.85, 0.15), 
    size = length(dataframe), 
    replace = TRUE) ]
})
  
# Train Test Split
## 75% of the sample size
sample_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = sample_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

write.csv(train, 'Data/train.csv', row.names = FALSE)
write.csv(test, 'Data/test.csv', row.names = FALSE)
