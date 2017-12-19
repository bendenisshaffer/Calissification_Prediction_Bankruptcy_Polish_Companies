
clean_polish_dt = read.csv("data/clean/clean_polish_dt.csv")

pca = prcomp(log(clean_polish_dt[,-c(58,57,56,47,46,27,26,21,14)]^2 + 1))
var = (pca$sdev^2)/sum(pca$sdev^2)
vd = data.frame(Var = var, PC = 1:49)

ggplot_PCs = ggplot(vd, aes(x = PC, y = Var)) + geom_line() +
        geom_col(aes(fill = -Var)) +
        labs(title = "Percentage of Variance Explained by PC's", x =" Principal Component ", y = "Variance")+
        theme_dark()+ scale_fill_continuous(name = "% of Variance \n explained")+xlim(0,40)

save(pca, file = "saved_output/objects/pca.RData")
ggsave("saved_output/plots/pac_plot.png", ggplot_PCs, device = "png")