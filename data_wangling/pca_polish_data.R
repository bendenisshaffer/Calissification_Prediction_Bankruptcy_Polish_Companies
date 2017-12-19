
clean_polish_dt = read.csv("data/clean/clean_polish_dt.csv")

X = as.matrix(log(clean_polish_dt[,-c(58,57,56,47,46,27,26,21,14)]^2 + 1))
M = pca$rotation[,1:5]

PCA_polish_data = as.data.frame((X %*% M))
PCA_polish_data = cbind(PCA_polish_data,clean_polish_dt[,c(58,57,56,47,46,27,26,21,14)])
PCA_polish_data$year = as.factor(PCA_polish_data$year)
PCA_polish_data$class = as.factor(PCA_polish_data$class)

write.csv(PCA_polish_data, "data/clean/pca_polish_dt.csv", row.names = FALSE)
