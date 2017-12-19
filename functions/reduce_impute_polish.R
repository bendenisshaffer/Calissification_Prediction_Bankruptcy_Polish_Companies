reduce_impute_polish = function(data, margin = 200, rate = 2){
        
        require(mice)
        require(dplyr)
        
        #rate (times greater then the natural rate of banckrupcies for NA cases)
        #margin (if missing at random but more then margin na's in the column)
        
        names = tail(names(sort(apply(data,2, function(x) sum(is.na(x))))),14)
        data$Attr14 = log(data$Attr14 + 1)
        
        r = list(length = 14)
        for(i in 1:14){
                r[i] = sum(is.na(data[data$class == 1,names[i]])) / sum(is.na(data[data$class == 0,names[i]]))
        }
        
        nr = length(data[data$class == 1,"class"])/length(data[data$class == 0,"class"])
        
        make_factor = names[round(as.numeric(r)/nr, 2) > rate] #missing not at random so make factor
        impute = names[!(round(as.numeric(r)/nr, 2) > rate)]   #missing at random, impute is pass margin
        
        for(i in 1:length(impute)){
                if(sum(is.na(data[,impute[i]])) > margin){
                        data = data[,-which(names(data) == impute[i])]
                }
        }
        
        for(i in 1:length(make_factor)){
                data[,make_factor[i]] = as.factor(ifelse(is.na(data[,make_factor[i]]),1,0))
        }
        
        imputed_polish_dt = mice(data, method = "pmm", print = FALSE)
        imputed_polish_dt = complete(imputed_polish_dt)
        
        imputed_polish_dtt = imputed_polish_dt %>% filter(!is.na(Attr46)) %>%
                filter(!is.na(Attr19)) %>% filter(!is.na(Attr14)) %>% filter(!is.na(Attr17))
        
        return(imputed_polish_dtt)
}


# names = tail(names(sort(apply(polish_dt,2, function(x) sum(is.na(x))))),14)
# 
# polish_dt$Attr14 = log(polish_dt$Attr14 + 1)
# 
# r = list(length = 14)
# for(i in 1:14){
#         r[i] = sum(is.na(polish_dt[polish_dt$class == 1,names[i]])) / sum(is.na(polish_dt[polish_dt$class == 0,names[i]]))
# }
# 
# nr = length(polish_dt[polish_dt$class == 1,"class"])/length(polish_dt[polish_dt$class == 0,"class"])
# 
# make_factor = names[round(as.numeric(r)/nr, 2) > 2]
# impute = names[!(round(as.numeric(r)/nr, 2) > 2)]
# 
# for(i in 1:length(impute)){
#         if(sum(is.na(polish_dt[,impute[i]])) > 200){
#                 polish_dt = polish_dt[,-which(names(polish_dt) == impute[i])]
#         }
# }
# 
# 
# for(i in 1:length(make_factor)){
#         polish_dt[,make_factor[i]] = as.factor(ifelse(is.na(polish_dt[,make_factor[i]]),1,0))
# }
# 
# imputed_polish_dt = mice(polish_dt, method = "pmm")
# imputed_polish_dt = complete(imputed_polish_dt)
# 
# imputed_polish_dt$rown = 1:nrow(complete(imputed_polish_dt))
# identical(polish_dt[is.na(polish_dt[,"Attr14"]),"rown"],polish_dt[is.na(polish_dt[,"Attr17"]),"rown"])
# identical(polish_dt[is.na(polish_dt[,"Attr17"]),"rown"],polish_dt[is.na(polish_dt[,"Attr20"]),"rown"])
# identical(polish_dt[is.na(polish_dt[,"Attr20"]),"rown"],polish_dt[is.na(polish_dt[,"Attr44"]),"rown"])
# identical(polish_dt[is.na(polish_dt[,"Attr44"]),"rown"],polish_dt[is.na(polish_dt[,"Attr56"]),"rown"])
# identical(polish_dt[is.na(polish_dt[,"Attr56"]),"rown"],polish_dt[is.na(polish_dt[,"Attr19"]),"rown"])
# identical(polish_dt[is.na(polish_dt[,"Attr19"]),"rown"],polish_dt[is.na(polish_dt[,"Attr46"]),"rown"])
# 
# 
# imputed_polish_dtt = imputed_polish_dt %>% filter(!is.na(Attr46)) %>%
#         filter(!is.na(Attr19)) %>% filter(!is.na(Attr14)) %>% filter(!is.na(Attr17))

