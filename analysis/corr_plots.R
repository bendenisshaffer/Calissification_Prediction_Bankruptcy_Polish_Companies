require(corrplot)

clean_polish_dt = read.csv("data/clean/clean_polish_dt.csv")

corr = cor(clean_polish_dt[,-c(57,58)])

png("saved_output/plots/corr_plot.png", width = 600, height = 600, pointsize = 25)
corrplot(corr, method = "square", type = "lower", order = "hclust", tl.pos = "d", tl.cex = 0.3, main = "\nCorrelation Plot")
dev.off()
