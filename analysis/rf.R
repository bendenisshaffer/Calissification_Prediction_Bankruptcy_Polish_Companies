require(caTools)
require(randomForest)
require(ggplot2)
require(gridExtra)

train = read.csv("data/clean/train.csv")
test = read.csv("data/clean/test.csv")

Xtrain = train[,c(1:6,8:14)]
Ytrain = train$class
Xtest = test[,c(1:6,8:14)]
Ytest = test$class

md = list(1,2,3,4,5,6,7,8,9,10)
for(i in 1:10){
        rf = randomForest(x = Xtrain, 
                          y = Ytrain, 
                          ntree = 1000, 
                          mtry = i,
                          xtest = Xtest,
                          ytest = Ytest)
        md[[i]] = rf
}

save(md, file = "saved_output/objects/rf.RData")

png(filename = "saved_output/plots/rf_plots.png", width = 1200)

par(mfrow = c(1,3))

plot(md[[1]]$err.rate[1:200,3], type = "l", ylim = c(0.65,1), main = "False Positive Rate")
for(i in 2:10){
        lines(md[[i]]$err.rate[1:200,3], col = i)
}

plot(md[[1]]$err.rate[1:200,2], type = "l", ylim = c(0,0.04), main = "False Negative Rate")
for(i in 2:10){
        lines(md[[i]]$err.rate[1:200,2], col = i)
}


plot(md[[1]]$err.rate[1:200,1], type = "l", ylim = c(0.035,0.07), main = "OOB error")
for(i in 2:10){
        lines(md[[i]]$err.rate[1:200,1], col = i)
}

dev.off()


test_error = list(1,2,3,4,5,6,7,8,9,10)
for(i in 1:10){
        test_error[[i]] = table(test$class,md[[i]]$test[1]$predicted)
}

train_error = list(1,2,3,4,5,6,7,8,9,10)
for(i in 1:10){
        train_error[[i]] = md[[i]]$confusion
}

testing_FPR = sapply(test_error, function(x) 1 -  x[2,2]/(x[2,1]+x[2,2]))
training_FPR = sapply(train_error, function(x) 1 - x[2,2]/(x[2,1]+x[2,2]))

df = data.frame(Error = c(testing_FPR,training_FPR), 
                Set = c(rep("Test",10),rep("Train",10)),
                M = rep(1:10,2))

g1 = ggplot(df, aes(x = M, y = Error, color = Set)) + 
        geom_line(size = 1) + 
        labs(title = "False Positive Rate on Testing and Training Sets")+
        theme_bw()%+replace%theme(legend.position ="bottom",
                                  legend.direction = "vertical",legend.title = element_blank())

### Plot of FNR for Training and Testing Data
testing_FNR = sapply(test_error, function(x) x[1,2]/(x[1,1]+x[1,2]))
training_FNR = sapply(train_error, function(x) x[1,2]/(x[1,1]+x[1,2]))

df = data.frame(Error = c(testing_FNR,training_FNR), 
                Set = c(rep("Test",10),rep("Train",10)),
                M = rep(1:10,2))

g2 = ggplot(df, aes(x = M, y = Error, color = Set)) + 
        geom_line(size  = 1) + 
        labs(title = "False Negative Rate on Testing and Training Sets")+
        theme_bw()%+replace%theme(legend.position ="bottom",
                                  legend.direction = "vertical",legend.title = element_blank())


g = grid.arrange(g1,g2, ncol = 2, nrow = 1)
ggsave(filename = "saved_output/plots/rf_FPR_FNR_plots.png", plot = g, device = "png")













