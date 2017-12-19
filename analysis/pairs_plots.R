library(GGally)

PCA_polish_data = read.csv("data/clean/pca_polish_dt.csv")

gg = ggpairs(PCA_polish_data,columns = c(1:7) , mapping = aes(color = class, alpha = 0.35))

ggsave("saved_output/plots/pair_plots.png", plot = gg, device = "png")
