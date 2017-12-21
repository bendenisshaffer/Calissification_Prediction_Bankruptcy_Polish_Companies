require(kernlab)
require(parallel)
require(gridExtra)
require(ggplot2)

train = read.csv("data/clean/train.csv")
test = read.csv("data/clean/test.csv")

train$year = as.factor(train$year)
train$class = as.factor(train$class)
test$year = as.factor(test$year)
test$class = as.factor(test$class)

cost = c(1,2,4,6,8,10,12,14,16)  #define list of tuning parameters

#function that fits a gaussian svm for different values of cost
fit_svm = function(cost){
        poly_ksvm <- ksvm(class ~ ., data = train,
                          kernel = "laplacedot",
                          kpar = "automatic",
                          C = cost,
                          cross = 4)
        return(poly_ksvm)
}

#apply the funciton to the list of parameters in parallel
lap_svm = mclapply(cost, fit_svm)

save(lap_svm, file = "saved_output/objects/laplacian_svm.RData")

#function that computes the train error tables
svm_tb_train = function(model){
        pred_pca_ksvm = predict(model, train[,-7])
        tb = table(pred_pca_ksvm, train[,7])
        return(tb)
}

#train error tables
train_confusion_svm = mclapply(lap_svm, svm_tb_train) 

#function that computes the test error tables
svm_tb_test = function(model){
        pred_pca_ksvm = predict(model, test[,-7])
        tb = table(pred_pca_ksvm, test[,7])
        return(tb)
}

#test error tables
test_confusion_svm = mclapply(lap_svm, svm_tb_test)


#SVM plot of FPR
testing_FPR_svm = sapply(test_confusion_svm, function(x) 1 -  x[2,2]/(x[2,1]+x[2,2]))
training_FPR_svm = sapply(train_confusion_svm, function(x) 1 - x[2,2]/(x[2,1]+x[2,2]))

df_svm = data.frame(Error = c(testing_FPR_svm,training_FPR_svm), 
                    Set = c(rep("Test",length(cost)),rep("Train",length(cost))),
                    Cost = rep(cost,2))

g1 = ggplot(df_svm, aes(x = Cost, y = Error, color = Set)) + 
        geom_line(size = 1) + 
        labs(title = "False Positive Rate/\nTesting&Training Sets SVM") +
        theme_economist()%+replace%theme(legend.position ="bottom",
                                         legend.direction = "vertical",
                                         legend.title = element_blank())+scale_color_fivethirtyeight()


#SVM plot of FNR
testing_FNR_svm = sapply(test_confusion_svm, function(x) x[1,2]/(x[1,1]+x[1,2]))
training_FNR_svm = sapply(train_confusion_svm, function(x) x[1,2]/(x[1,1]+x[1,2]))

df_svm = data.frame(Error = c(testing_FNR_svm,training_FNR_svm), 
                    Set = c(rep("Test",length(cost)),rep("Train",length(cost))),
                    Cost = rep(cost,2))

g2 = ggplot(df_svm, aes(x = Cost, y = Error, color = Set)) + 
        geom_line(size = 1) + 
        labs(title = "False Positive Rate/\nTesting&Training Sets SVM") +
        theme_economist()%+replace%theme(legend.position ="bottom",
                                         legend.direction = "vertical",
                                         legend.title = element_blank())+scale_color_fivethirtyeight()


### CV error and mean error

cv_error = sapply(lap_svm, function(x) x@cross)
mean_error_train = sapply(lap_svm, function(x) x@error)
mean_error_test = sapply(test_confusion_svm, function(x) (x[2,1]+x[1,2])/sum(x))

df_svm = data.frame(Error = c(cv_error, mean_error_train, mean_error_test), 
                    Type = c(rep("CV",length(cost)), rep("Train",length(cost)), rep("Test",length(cost))),
                    Cost = rep(cost,3))

g3 = ggplot(df_svm, aes(x = Cost, y = Error, color = Type)) + 
        geom_line(size = 1) + 
        labs(title = "False Positive Rate/\nTesting&Training Sets SVM") +
        theme_economist()%+replace%theme(legend.position ="bottom",
                                         legend.direction = "vertical",
                                         legend.title = element_blank())+scale_color_fivethirtyeight()



ggg = grid.arrange(g1,g2,g3, ncol = 3, nrow = 1)
ggsave("saved_output/plots/laplaceian_svm.png", plot = ggg, device = "png")
   
   
   
   