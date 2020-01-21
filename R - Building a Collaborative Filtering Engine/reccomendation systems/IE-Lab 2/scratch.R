library(data.table)
question_topics <- read.csv("C:\\Users/ignac/Desktop/ie/mbd term 2/reccomendation systems/IE-Lab 2/Problem/questiontopics.csv")
feedback <- read.csv("C:\\Users/ignac/Desktop/ie/mbd term 2/reccomendation systems/IE-Lab 2/Problem/userfeedback.csv")
answers <- read.csv("C:\\Users/ignac/Desktop/ie/mbd term 2/reccomendation systems/IE-Lab 2/Problem/useranswers.csv")


answers<-answers[1:4]
feedback<-feedback[1:4]
rownames(question_topics) <- question_topics$X

question_topics$X<-NULL
rownames(answers)<-rownames(question_topics)
rownames(feedback)<- rownames(question_topics)

question_answer <- sort(rowMeans(answers, na.rm = T), decreasing = T)
question_answer
question_feedback <- mean_ratings <- sort(rowMeans(feedback, na.rm = T), decreasing = T)
question_feedback



usertopics <- function(a){
  j <- vector()
  index<- grep(a, colnames(feedback))
  x<- as.data.frame(question_topics)[!is.na(feedback[,a]) &  feedback[,a]>0,]
  for(i in 1:ncol(x)){
    if(sum(x[i])>0){
      j[i]<-colnames(x[i])
      
    }
  }
  j <- j[!is.na(j)] 
  return(j)
}
  
sd(c(5,6,6,7,9))

user1_topics <- usertopics("User.1")
user2_topics <- usertopics("User.2")
user3_topics <- usertopics("User.3")



simple_unary_user_profile <- data.frame()
for(k in 1:ncol(feedback)){
  j<- vector()
  for(i in 1:ncol(question_topics)){
    j[i]<- sum(question_topics[,i]*feedback[,k], na.rm = T)
  }
  simple_unary_user_profile<-rbind(simple_unary_user_profile,j)

}
simple_unary_user_profile

colnames(simple_unary_user_profile)<-colnames(question_topics)
rownames(simple_unary_user_profile) <- colnames(feedback)
simple_unary_user_profile

apply(question_topics,1,sum)
CB_unit_weight <- data.frame()


for(k in 1:nrow(question_topics)){
  for(i in 1:ncol(question_topics)){
    CB_unit_weight[k,i] <- question_topics[k,i]/sum(question_topics[k,]) 
  }
}
CB_unit_weight

unit_weight_user_profile <- data.frame()
for(k in 1:ncol(feedback)){
  j<- vector()
  for(i in 1:ncol(CB_unit_weight)){
    j[i]<- sum(CB_unit_weight[,i]*feedback[,k], na.rm = T)
  }
  unit_weight_user_profile<-rbind(unit_weight_user_profile,j)
  
}
unit_weight_user_profile

colnames(unit_weight_user_profile)<-colnames(question_topics)
rownames(unit_weight_user_profile) <- colnames(feedback)
unit_weight_user_profile

df <- sapply(question_topics,sum)
ldf <- log10(nrow(question_topics)/df)

log10(5)
?log

ldf*unit_weight_user_profile[1,]


idf_user_profile <- data.frame()


for(i in 1:nrow(unit_weight_user_profile)){
  idf_user_profile <- rbind(idf_user_profile,ldf*unit_weight_user_profile[i,])
  print(idf_user_profile)
}
idf_user_profile



unary_predict_table <- data.frame()


for(i in 1:nrow(user_profile)){
  for(k in 1:nrow(question_topics)){
    unary_predict_table[k,i]<- cosine(as.numeric(question_topics[k,]),as.numeric(user_profile[i,]))
  
  }
}

unary_predict_table
ncol(question_topics)


wordcount <- c(234,2443,203,24,4234,34,234,235,129,2342,1655,54,1334,1564,83,3456,1668,126,1868,38)
length(wordcount)

x<-Sys.Date()
dates <- c(x-455,x-1,x-1,x-322,x-1,x-157,x-230,x-100,x-455, x-1, x-1, x-56, x-1,x,x-1, x-7,x-1,x-1,x-23, x-1)
length(dates)


feedback
read_table <- data.frame()
for(i in 1:ncol(feedback)){
  for(k in 1:nrow(feedback)){
  if(!is.na(feedback[k,i]) | !is.na(answers[k,i]) ){
    read_table[k,i] <- 1
  }
    else{
      read_table[k,i]<- 0
    }
  }
}
rownames(read_table) <- rownames(question_topics)
colnames(read_table) <- colnames(idf_predict_table)
read_table



read_table


bathroom_pred <- vector()
bathroom_pred_list <- data.frame("dummy" = c(1:20))
predict_table_all_users <- idf_predict_table
predict_table_all_users$User.4<- idf_average
predict_table_all_users

for(i in 1:ncol(predict_table_all_users)){
  for(k in 1:nrow(read_table)){
    if(read_table[k,i]==0 & wordcount[k]<300 & predict_table_all_users[k,i]>0){
      bathroom_pred[k]=predict_table_all_users[k,i]
    }
    else{bathroom_pred[k]=0}
  }
  print(length(bathroom_pred))
  bathroom_pred_list <- cbind(bathroom_pred_list,bathroom_pred)

}
bathroom_pred_list$dummy <- NULL
colnames(bathroom_pred_list)<- colnames(predict_table_all_users)
bathroom_pred_list





















cor(as.numeric(predict_table_all_users$User.1),as.numeric(predict_table_all_users$User.2), use = "complete.obs", method = "pearson")
cor(as.numeric(predict_table_all_users$User.1),as.numeric(predict_table_all_users$User.3), use = "complete.obs", method = "pearson")
cor(as.numeric(predict_table_all_users$User.1),as.numeric(predict_table_all_users$User.4), use = "complete.obs", method = "pearson")
cor(as.numeric(predict_table_all_users$User.2),as.numeric(predict_table_all_users$User.3), use = "complete.obs", method = "pearson")
cor(as.numeric(predict_table_all_users$User.2),as.numeric(predict_table_all_users$User.4), use = "complete.obs", method = "pearson")
cor(as.numeric(predict_table_all_users$User.3),as.numeric(predict_table_all_users$User.4), use = "complete.obs", method = "pearson")
#this is to calculate the pearson corellation, aand using complete.obs to neglect the nas.




formvect <- vector() #creates empty vector to sore values of the rui-rbar*wau for each user
sumformvect <- vector()  # creates empty vector to store sum of rui-rbar*wau for each movie
absvect <- vector()
sumabsvect <- vector()
user1ratings <- vector()#creates empty vector to get the recommended movies ratings
user1_avg <- mean(tran_critics$Sophia, na.rm = T)



formvect <- vector()
sumformvect <- vector()
absvect <- vector()
sumabsvect <- vector()
user1ratings <- vector()
user1_avg <- mean(predict_table_all_users$User.1, na.rm = T)




for (q in 1:nrow(question_topics)) {
  for(user in 1:nrow(predict_table_all_users)){
    rum <- predict_table_all_users[q,user]
    ravg <- mean(predict_table_all_users[,user], na.rm = T)
    if(is.na(rum)==T| identical(predict_table_all_users$User.1, predict_table_all_users[,user])){form <- 0}
    else{
      wau <- cor(predict_table_all_users$User.1,predict_table_all_users[,user], use = "complete.obs")
      
      form <- (rum - ravg)*wau
      
    }
    formvect[user] <- form
    absvect[user] <- (wau) 
    
  }
  sumformvect[movie]<- sum(formvect, na.rm = T)
  #sumabsvect[movie]<- sum(abs(absvect), na.rm = T)
  sophiaratings[movie] <- sophia_avg + (sumformvect[movie]/6.928035)
}

sum(absvect)



tran_critics$ratings <- sophiaratings
rownames(tran_critics)<- colnames(critics)[2:ncol(critics)]
xy <-tran_critics[is.na(tran_critics$Sophia)==T,]
xy <- xy[order(xy$ratings, decreasing = T),]
xy <- xy[,c(15,ncol(xy))]
xy








