require(parallel)
require(ggthemes)
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
        gaussian_ksvm <- ksvm(class ~ ., data = train,
                              kernel = "rbfdot",
                              kpar = list(sigma=0.05),
                              C = cost,
                              cross = 4)
        return(gaussian_ksvm)
}

#apply the funciton to the list of parameters in parallel
gaussian_svm = mclapply(cost, fit_svm)


save(gaussian_svm, file = "saved_output/objects/gaussian_svm.RData")

#function that computes the train error tables
gaussian_svm_tb_train = function(model){
        pred_pca_ksvm = predict(model, train[,-7])
        tb = table(pred_pca_ksvm, train[,7])
        return(tb)
}

#train error tables
train_confusion_svm = mclapply(gaussian_svm, gaussian_svm_tb_train) 

#function that computes the test error tables
gaussian_svm_tb_test = function(model){
        pred_pca_ksvm = predict(model, test[,-7])
        tb = table(pred_pca_ksvm, test[,7])
        return(tb)
}

#test error tables
test_confusion_svm = mclapply(gaussian_svm, gaussian_svm_tb_test) 


#SVM plot of FPR
gaussian_testing_FPR_svm = sapply(test_confusion_svm, function(x) 1 -  x[2,2]/(x[2,1]+x[2,2]))
gaussian_training_FPR_svm = sapply(train_confusion_svm, function(x) 1 - x[2,2]/(x[2,1]+x[2,2]))

df_svm_FPR = data.frame(Error = c(gaussian_testing_FPR_svm,gaussian_training_FPR_svm), 
                        Set = c(rep("Test",length(cost)),rep("Train",length(cost))),
                        Cost = rep(cost,2))

g1 = ggplot(df_svm_FPR, aes(x = Cost, y = Error, color = Set)) + 
        geom_line(size = 1) + 
        labs(title = "False Positive Rate/\nTesting&Training Sets SVM")+
        theme_economist()%+replace%theme(legend.position ="bottom",
                                         legend.direction = "vertical",
                                         legend.title = element_blank())+scale_color_colorblind()


#SVM plot of FNR
gaussian_testing_FNR_svm = sapply(test_confusion_svm, function(x) x[1,2]/(x[1,1]+x[1,2]))
gaussian_training_FNR_svm = sapply(train_confusion_svm, function(x) x[1,2]/(x[1,1]+x[1,2]))

df_svm_FNR = data.frame(Error = c(gaussian_testing_FNR_svm,gaussian_training_FNR_svm), 
                        Set = c(rep("Test",length(cost)),rep("Train",length(cost))),
                        Cost = rep(cost,2))

g2 = ggplot(df_svm_FNR, aes(x = Cost, y = Error, color = Set)) + 
        geom_line(size = 1) + 
        labs(title = "False Negative Rate/\nTesting&Training Sets SVM")+
        theme_economist()%+replace%theme(legend.position ="bottom",
                                         legend.direction = "vertical",
                                         legend.title = element_blank())+scale_color_colorblind()



### CV error and mean error

gaussian_cv_error = sapply(gaussian_svm, function(x) x@cross)
gaussian_mean_error_train = sapply(gaussian_svm, function(x) x@error)
gaussian_mean_error_test = sapply(test_confusion_svm, function(x) (x[2,1]+x[1,2])/sum(x))

df_svm_cv_mean = data.frame(Error = c(gaussian_cv_error, gaussian_mean_error_train, gaussian_mean_error_test), 
                            Type = c(rep("CV",length(cost)), rep("Train",length(cost)),rep("Test",length(cost))),
                            Cost = rep(cost,3))

g3 = ggplot(df_svm_cv_mean, aes(x = Cost, y = Error, color = Type)) + 
        geom_line(size = 1) + 
        labs(title = "SVM Mean and CV error")+
        theme_economist()%+replace%theme(legend.position ="bottom",
                                         legend.direction = "vertical",
                                         legend.title = element_blank())+scale_color_colorblind()

require(gridExtra)

ggg = grid.arrange(g1,g2,g3, ncol = 3, nrow = 1)

ggsave("saved_output/plots/gaussian_svm.png", plot = ggg, device = "png")

