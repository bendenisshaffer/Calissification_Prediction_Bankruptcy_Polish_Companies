require(e1071)
require(neuralnet)
require(parallel)
require(ggplot2)
require(gridExtra)
library(ggthemes)

train = read.csv("data/clean/train.csv")
test = read.csv("data/clean/test.csv")
train$year = as.factor(train$year)
train$class = as.factor(train$class)
test$year = as.factor(test$year)
test$class = as.factor(test$class)


train2 = train
test2 = test


train = data.frame(train[,c(1:6,8:14)])
test = data.frame(test[,c(1:6,8:14)])
train[,14] = train2[,7]
test[,14] = test2[,7]
# I think we only need e1071 library



xnames = colnames(train2)
xnames = xnames[c(1:6,8:14)]
nntrain = data.frame(train2[c(1:6,8:14)])
nntest = data.frame(test2[c(1:6,8:14)])
nntrain$class = train2[,7]
nntest$class = test2[,7]

# have to break down V58 into 2 columns
nntrain2 = data.frame(model.matrix(~factor(nntrain$class)-1))
nntest2 = data.frame(model.matrix(~factor(nntest$class)-1))

#get rid of year for now:
nntrain = nntrain[,-6]
nntest = nntest[,-6]
#Now see if we can center the data:
nntrain4 = scale(data.frame(nntrain[,1:12]))
nntest4 = scale(data.frame(nntest[,1:12]))


nntrain5 = data.frame(nntrain4)
nntest5 = data.frame(nntest4)


# Add back the two columns of BK and non-BK
nntrain6 = cbind(nntrain5, nntrain2)
nntest6 = cbind(nntest5, nntest2)
colnames(nntrain6)[13] = "Not_BK"
colnames(nntrain6)[14] = "BK"
colnames(nntest6)[13] = "Not_BK"
colnames(nntest6)[14] = "BK"


# Now we do NNEts

n = names(nntrain6[,1:12])
f = as.formula(paste("BK + Not_BK ~", paste(n, collapse = " + ")))
# Does this work? No. 2, and 3 layer NNets don't work.
# nn1 = neuralnet(f, data= nntrain6, hidden= 10, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)



pred = function(nn, dat) {
        yhat = compute(nn, dat)$net.result
        yhat = apply(yhat, 1, round) # rounding is better
        return(yhat)
}


hid = 3:30

fit_NN = function(hid){
        NN = neuralnet(f, data = nntrain6, hidden= hid, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
        return(NN)
}

NN = mclapply(hid,fit_NN)
NN1 = NN[1]
NN2 = NN[2]
NN3 = NN[3]
NN4 = NN[4]
NN5 = NN[5]
NN6 = NN[6]
NN7 = NN[7]
NN8 = NN[8]
NN9 = NN[9]
NN10 = NN[10]
NN11 = NN[11]
NN12 = NN[12]
NN13 = NN[13]
NN14 = NN[14]
NN15 = NN[15]
NN16 = NN[16]
NN17 = NN[17]
NN18 = NN[18]
NN19 = NN[19]
NN20 = NN[20]
NN21 = NN[21]
NN22 = NN[22]
NN23 = NN[23]
NN24 = NN[24]
NN25 = NN[25]
NN26 = NN[26]
NN27 = NN[27]
NN28 = NN[28]

save(NN1, file = "saved_output/objects/nn1.RData")
save(NN2, file = "saved_output/objects/nn2.RData")
save(NN3, file = "saved_output/objects/nn3.RData")
save(NN4, file = "saved_output/objects/nn4.RData")
save(NN5, file = "saved_output/objects/nn5.RData")
save(NN6, file = "saved_output/objects/nn6.RData")
save(NN7, file = "saved_output/objects/nn7.RData")
save(NN8, file = "saved_output/objects/nn8.RData")
save(NN9, file = "saved_output/objects/nn9.RData")
save(NN10, file = "saved_output/objects/nn10.RData")
save(NN11, file = "saved_output/objects/nn11.RData")
save(NN12, file = "saved_output/objects/nn12.RData")
save(NN13, file = "saved_output/objects/nn13.RData")
save(NN14, file = "saved_output/objects/nn14.RData")
save(NN15, file = "saved_output/objects/nn15.RData")
save(NN16, file = "saved_output/objects/nn16.RData")
save(NN17, file = "saved_output/objects/nn17.RData")
save(NN18, file = "saved_output/objects/nn18.RData")
save(NN19, file = "saved_output/objects/nn19.RData")
save(NN20, file = "saved_output/objects/nn20.RData")
save(NN21, file = "saved_output/objects/nn21.RData")
save(NN22, file = "saved_output/objects/nn22.RData")
save(NN23, file = "saved_output/objects/nn23.RData")
save(NN24, file = "saved_output/objects/nn24.RData")
save(NN25, file = "saved_output/objects/nn25.RData")
save(NN26, file = "saved_output/objects/nn26.RData")
save(NN27, file = "saved_output/objects/nn27.RData")
save(NN28, file = "saved_output/objects/nn28.RData")

Mean_test_error = as.list(rep(0, 26))
Mean_train_error = as.list(rep(0, 26))
table_test = list()
table_train = list()

for(i in 1:26){
        train_result = as.matrix(t(pred(NN[[i]], nntrain6[,1:12])), nrow = length(nntrain6[,1]), ncol = 2)
        test_result = as.matrix(t(pred(NN[[i]], nntest6[,1:12])), nrow = length(nntest6[,1]), ncol= 2)
        table_test[[i]] = table(test_result[1:length(nntest[,1])],  nntest6$BK)
        table_train[[i]] = table(train_result[1:length(nntrain[,1])],  nntrain6$BK)
        Mean_test_error[[i]] = mean(test_result != nntest6[,c("BK","Not_BK")])
        Mean_train_error[[i]] = mean(train_result != nntrain6[,c("BK","Not_BK")])
}




## FALSE POSITIVE RATE:
test_FPR_NNet = sapply(table_test, function(x) 1 -  x[2,2]/(x[2,1]+x[2,2]))
train_FPR_NNet = sapply(table_train, function(x) 1 -  x[2,2]/(x[2,1]+x[2,2]))

df_FPR_NNet = data.frame(Error = c(test_FPR_NNet,train_FPR_NNet), 
                         Set = c(rep("Test",26),rep("Train",26)),
                         Nodes = rep(seq(from = 3,to = 28, by = 1),2))

g1 = ggplot(df_FPR_NNet, aes(x = Nodes, y = Error, color = Set)) + 
        geom_line(size = 1) + 
        labs(title = "False Positive Rate/\nTesting&Training Sets NNet") +
        theme_economist()%+replace%theme(legend.position ="bottom",
                                         legend.direction = "vertical",
                                         legend.title = element_blank())+scale_color_fivethirtyeight()

## FALSE NEGATIVE RATE:

testing_FNR_NNet = sapply(table_test, function(x) x[1,2]/(x[1,1]+x[1,2]))
training_FNR_NNet = sapply(table_train, function(x) x[1,2]/(x[1,1]+x[1,2]))

df_FNR_NNet = data.frame(Error = c(testing_FNR_NNet,training_FNR_NNet), 
                         Set = c(rep("Test",26),rep("Train",26)),
                         Nodes = seq(from = 3,to = 28, by = 1))

g2 = ggplot(df_FNR_NNet, aes(x = Nodes, y = Error, color = Set)) + 
        geom_line(size = 1) + 
        labs(title = "False Positive Rate/\nTesting&Training Sets NNet") +
        theme_economist()%+replace%theme(legend.position ="bottom",
                                         legend.direction = "vertical",
                                         legend.title = element_blank())+scale_color_fivethirtyeight()

mean_test_error_matrix = rep(0, 26)
mean_train_error_matrix = rep(0, 26)
for(i in 1:26){
        mean_test_error_matrix[i] = Mean_test_error[[i]]
        mean_train_error_matrix[i] = Mean_train_error[[i]]
}

df_MTE_NNet = data.frame(Error = c(mean_test_error_matrix,mean_train_error_matrix), 
                         Set = c(rep("Test",26),rep("Train",26)),
                         Nodes = seq(from = 3,to = 28, by = 1))

g3 = ggplot(df_MTE_NNet, aes(x = Nodes, y = Error, color = Set)) + 
        geom_line(size = 1) + 
        labs(title = "False Positive Rate/\nTesting&Training Sets NNet") +
        theme_economist()%+replace%theme(legend.position ="bottom",
                                         legend.direction = "vertical",
                                         legend.title = element_blank())+scale_color_fivethirtyeight()


ggg = grid.arrange(g1,g2,g3, ncol = 3, nrow = 1)
ggsave(filename = "saved_output/plots/nn_plots.png", plot = ggg, device = "png")
