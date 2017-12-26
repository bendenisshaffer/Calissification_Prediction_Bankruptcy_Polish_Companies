require(ggplot2)
require(gridExtra)
require(e1071)
require(neuralnet)
require(kernlab)

train = read.csv("data/clean/train.csv")
test = read.csv("data/clean/test.csv")

train$year = as.factor(train$year)
train$class = as.factor(train$class)
test$year = as.factor(test$year)
test$class = as.factor(test$class)

nntest = data.frame(test[,c(1:6,8:14)])
nntest$class = test[,7]
nntest2 = data.frame(model.matrix(~factor(nntest$class)-1))
nntest = nntest[,-6]
nntest4 = scale(data.frame(nntest[,1:12]))
nntest5 = data.frame(nntest4)
nntest6 = cbind(nntest5, nntest2)

load("saved_output/objects/gaussian_svm.RData")
load("saved_output/objects/nn8.RData")
load("saved_output/objects/rf.RData")

pred = function(nn, dat) {
        yhat = compute(nn, dat)$net.result
        yhat = apply(yhat, 1, round) # rounding is better
        return(yhat)
}

gg1 = ggplot(test, aes(x = PC1, y = PC2, color = class)) + geom_point(aes(alpha = class)) +
        labs(title = "True Bankruptcies", color = "Status") + theme_bw() + xlim(-30,25) + ylim(-5,35) + 
        scale_color_manual(labels = c("Not_BK","BK"), values = c("blue","red"))

svm_pred = predict(gaussian_svm[[6]], newdata = test[,-7])

DT = cbind(test, pred = svm_pred)
gg2 = ggplot(DT, aes(x = PC1, y = PC2, color = pred)) + geom_point(aes(alpha = pred)) +
        labs(title = "Gaussian SVM", color = "Status") + theme_bw() + xlim(-30,25) + ylim(-5,35) + 
        scale_color_manual(labels = c("Not_BK","BK"), values = c("blue","red"))

nn_test_result = as.matrix(t(pred(NN8[[1]], nntest6[,1:12])), nrow = length(nntest6[,1]), ncol= 2)
DT_NNet = cbind(test[,1:2], pred = as.factor(nn_test_result[,1]))
gg3 = ggplot(DT_NNet, aes(x = PC1, y = PC2, color = pred)) + geom_point(aes(alpha = pred)) +
        labs(title = "Neural Net", color = "Status") + theme_bw() + xlim(-30,25) + ylim(-5,35) +
        scale_color_manual(labels = c("Not_BK","BK"), values = c("blue","red"))


DT_rf = cbind(test[,1:2], pred = md[[6]]$test$predicted)
gg4 = ggplot(DT_rf, aes(x = PC1, y = PC2, color = pred)) + geom_point(aes(alpha = pred)) +
        labs(title = "Random Forest", color = "Status") + theme_bw() + xlim(-30,25) + ylim(-5,35) + 
        scale_color_manual(labels = c("Not_BK","BK"), values = c("blue","red"))


ggg = grid.arrange(gg1,gg2,gg3,gg4, ncol = 2, nrow = 2)
ggsave("saved_output/plots/results_plot.png", plot = ggg, device = "png")
