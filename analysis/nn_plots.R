require(neuralnet)
require(NeuralNetTools)
require(nnet)


load("saved_output/objects/nn8.RData")

png(filename = "saved_output/plots/nn.png")
plotnet(NN8[[1]])
dev.off()
