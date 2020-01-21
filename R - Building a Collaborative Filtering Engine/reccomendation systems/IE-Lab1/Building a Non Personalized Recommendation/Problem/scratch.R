#Q1



##Q2
tran_critics <-as.data.frame(sapply(tran_critics, as.numeric))
plot(tran_critics$Sophia,tran_critics$Nuria)
abline(lm(tran_critics$Nuria~tran_critics$Sophia))


cor(tran_critics$John,tran_critics$Maria, use = "complete.obs")
##Q3
rowMeans()  55.0L
summary(tran_critics)
mean(na.omit(tran_critics$Sophia))
mean(tran_critics$Sophia, na.rm = T)
sapply(tran_critics, function(x){mean(na.omit(x))})

mean_na <- function(x){
  mean(na.omit(x))
}

mean_na(tran_critics$Sophia)
mean_na(tran_critics[,1])
sophia_avg <- mean_na(tran_critics$Sophia)
sophia_avg
View(tran_critics)


x=vector()
y=vector()
abs_wau=vector()
sophia=vector()

for (movie in 1:nrow(tran_critics)){
  if(identical(tran_critics[,movie],tran_critics$Sophia)==TRUE) {
    next
  }else{
  for(user in 1:ncol(tran_critics)){
    rum <- tran_critics[movie,user]
    ru_avg <- mean_na(tran_critics[,user])
    
    #wau <- cor(tran_critics$Sophia, tran_critics[,user],use = "complete.obs")
    if(is.na(rum)==TRUE | identical(tran_critics[,user],tran_critics$Sophia)==TRUE) {
      next
    }else{
    wau <- cor(tran_critics$Sophia, tran_critics[,user],use = "complete.obs")
    x[user] <- (rum-ru_avg)*wau
    print(x)
    abs_wau[user] <-abs(wau)
}
   
    }
    
  }
  y[movie]<-sum(x, na.rm = T)
  sophia[movie] <- sophia_avg+(sum(y)/sum(na.omit(abs_wau)))
}
x
y
abs_wau
sophia
sort(sophia, decreasing = TRUE)
















#####################
for (movie in 1:nrow(tran_critics)){
  for(user in 1:ncol(tran_critics)){
    for (movie in 1:nrow(tran_critics)){
      rum <- tran_critics[movie,user]
      ru_avg <- mean_na(tran_critics[,user])
      wau <- cor(tran_critics$Sophia, tran_critics[,user],use = "complete.obs")
      if(is.na(rum)==TRUE | identical(tran_critics[,i],tran_critics$Sophia)==TRUE) {
        next
      }else{
        x <- (rum-ru_avg)*wau
        sum_wau <-abs(wau)
        
      }
      
    }
    print(sort(sophia_avg+(sum(x)/sum(abs(sum_wau))), decreasing = TRUE))
  }
  
}    
  

identical(tran_critics[,15],tran_critics$Sophia)
