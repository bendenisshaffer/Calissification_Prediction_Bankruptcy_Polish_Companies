require(dplyr)

PCA_polish_data = read.csv("data/clean/pca_polish_dt.csv")

data_sp = PCA_polish_data %>% group_by(year) # group them by year / we are using the cleaned file after imputation!!

data_sp$rownumber = 1:nrow(data_sp) # an indicator column for each row to check if the split worked fine

set.seed(123) 
sample = sample.split(data_sp, SplitRatio = 2/3) # we pick 2/3 split ratio
train = subset(data_sp, sample == TRUE) %>% as.data.frame() # 2/3 proportion of the training set
test = subset(data_sp, sample == FALSE) %>% as.data.frame() # 1/3 proportion for the test set 

# the split worked fine so if we want
# we can delete the extra column
train = subset(train , select = -rownumber)
test = subset(test , select = -rownumber)

write.csv(train, "data/clean/train.csv", row.names = FALSE)
write.csv(test, "data/clean/test.csv", row.names = FALSE)
