library(foreign)
library(dplyr)
library(ggplot2)
library(parallel)
library(stringr)
source("functions/reduce_impute_polish.R")


files = str_c("data/raw/",dir(path = "data/raw/"))

data = lapply(files, read.arff)

polish_dt = data[[1]]
polish_dt$year = 1
for(i in 2:length(data)) { 
        
        df = data[[i]]
        df$year = i
        
        polish_dt = rbind(polish_dt,df)
        
}



clean_polish_dt = reduce_impute_polish(polish_dt, margin = 200, rate = 2) #default tolerance arguments can be adjusted.

clean_polish_dt$Attr21 = as.integer(clean_polish_dt$Attr21)
clean_polish_dt$Attr27 = as.integer(clean_polish_dt$Attr27)
clean_polish_dt$Attr28 = as.integer(clean_polish_dt$Attr28)
clean_polish_dt$Attr53 = as.integer(clean_polish_dt$Attr53)
clean_polish_dt$Attr54 = as.integer(clean_polish_dt$Attr54)
clean_polish_dt$Attr64 = as.integer(clean_polish_dt$Attr64)
clean_polish_dt$class = as.integer(clean_polish_dt$class)
clean_polish_dt$year = as.integer(clean_polish_dt$year)


write.csv(clean_polish_dt, "data/clean/clean_polish_dt.csv")
