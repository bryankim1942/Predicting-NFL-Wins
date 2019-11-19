library(MASS)
library(car)
library(corrplot)
library(randomForest)
ptrain <- read.csv('football_train.csv')
ptest <- read.csv('football_test.csv')

train <- ptrain[,-1] #remove ID's
test <- ptest[,-1]

# Create new predictors
train$PassSuccess <- (train$PassesCompleted) / (train$PassesAttempted)
train$OppPassSuccess <- (train$OppPassesCompleted) / (train$OppPassesAttempted)
train$RPassSuccess <- (train$PassSuccess) / (train$OppPassSuccess)
train$RPassesCompleted <- (train$PassesCompleted) / (train$OppPassesCompleted)
train$RPassesAttempted <- (train$PassesAttempted) / (train$OppPassesAttempted)

train$RYards <- (train$Yards) / (train$OppYards)
train$ROffensivePlays <- (train$OffensivePlays) / (train$OppOffensivePlays)
train$YardSuccess <- (train$Yards) / (train$OffensivePlays)
train$OppYardSuccess <- (train$OppYards) / (train$OppOffensivePlays)
train$RYardSuccess <- (train$YardSuccess) / (train$OppYardSuccess)

train$RTurnOversLost <- (train$TurnOversLost) / (train$OppTurnOversLost)
train$RFumblesLost <- (train$FumblesLost) / (train$OppFumblesLost)
train$RFirstDowns <- (train$FirstDowns) / (train$OppFirstDowns)
train$RYardsGainedPassing <- (train$YardsGainedPassing) / (train$OppYardsGainedPassing)
train$RInterceptionsThrown <- (train$InterceptionsThrown) / (train$OppInterceptionsThrown)
train$RRushingAttempts <- (train$RushingAttempts) / (train$OppRushingAttempts)
train$RYardsGainedRushing <- (train$YardsGainedRushing) / (train$OppYardsGainedRushing)
train$RPenaltiesCommitedByTeam <- (train$PenaltiesCommitedByTeam) / (train$OppPenaltiesCommitedByTeam)
train$RPenaltiesInYards <- (train$PenaltiesInYards) / (train$OppPenaltiesInYards)
train$RFirstDownsByPenalty <- (train$FirstDownsByPenalty) / (train$OppFirstDownsByPenalty)
train$RNumberOfDrives <- (train$NumberOfDrives) / (train$OppNumberOfDrives)

# Create the same predictors for the test data
test$PassSuccess <- (test$PassesCompleted) / (test$PassesAttempted)
test$OppPassSuccess <- (test$OppPassesCompleted) / (test$OppPassesAttempted)
test$RPassSuccess <- (test$PassSuccess) / (test$OppPassSuccess)
test$RPassesCompleted <- (test$PassesCompleted) / (test$OppPassesCompleted)
test$RPassesAttempted <- (test$PassesAttempted) / (test$OppPassesAttempted)

test$RYards <- (test$Yards) / (test$OppYards)
test$ROffensivePlays <- (test$OffensivePlays) / (test$OppOffensivePlays)
test$YardSuccess <- (test$Yards) / (test$OffensivePlays)
test$OppYardSuccess <- (test$OppYards) / (test$OppOffensivePlays)
test$RYardSuccess <- (test$YardSuccess) / (test$OppYardSuccess)

test$RTurnOversLost <- (test$TurnOversLost) / (test$OppTurnOversLost)
test$RFumblesLost <- (test$FumblesLost) / (test$OppFumblesLost)
test$RFirstDowns <- (test$FirstDowns) / (test$OppFirstDowns)
test$RYardsGainedPassing <- (test$YardsGainedPassing) / (test$OppYardsGainedPassing)
test$RInterceptionsThrown <- (test$InterceptionsThrown) / (test$OppInterceptionsThrown)
test$RRushingAttempts <- (test$RushingAttempts) / (test$OppRushingAttempts)
test$RYardsGainedRushing <- (test$YardsGainedRushing) / (test$OppYardsGainedRushing)
test$RPenaltiesCommitedByTeam <- (test$PenaltiesCommitedByTeam) / (test$OppPenaltiesCommitedByTeam)
test$RPenaltiesInYards <- (test$PenaltiesInYards) / (test$OppPenaltiesInYards)
test$RFirstDownsByPenalty <- (test$FirstDownsByPenalty) / (test$OppFirstDownsByPenalty)
test$RNumberOfDrives <- (test$NumberOfDrives) / (test$OppNumberOfDrives)

# Random forest to find best predictors
set.seed(70)
choose<-c()
for (i in 1:100) { # this takes a while
  train1_index<-sort(sample(1:380,340))
  train1<-train[train1_index,]     
  test1<-train[-train1_index,]   
  rf<-randomForest(train1$Wins~.,data=train1,importance=TRUE)
  importance1<-rf$importance[,"%IncMSE"]   #get the predictor according to importance
  index<-order(importance1,decreasing=TRUE)[1:30]
  choose<-c(choose,colnames(train1[,index]))    #put the top 30 important predictors into the list "choose"
}
choose2<-table(choose) #count how many times the predictors are chosen in 100 iterations
choose2_index<-order(as.numeric(choose2),decreasing=TRUE)[1:30]  #get the top 30 important predictors according to the the counts 
preds<-names(choose2[choose2_index])
predictors<-preds[-which(preds=="Wins")]
predictors

col_pre<-c()
for (j in 1:length(predictors)){ #get the indices of the chosen predictors in the original train data
  col_pre<-c(col_pre, which(predictors[j] == colnames(train)))   
}
col_pre
toppreds <- train[,col_pre]

#subda<- cor(data.frame(train$Wins, train[,col_pre])) #corrplot which predictors are correlated with each other
#corrplot(subda ,method="number", type="lower", number.cex = 0.75, tl.cex = 0.75) 

badpreds <- c()
for(i in 1:length(col_pre)){ #find predictors with high cor with each other and put the one that is ranked lower in a string
  for(j in 1:length(col_pre)){
    if(i==j)next
    if(abs(cor(toppreds[i],toppreds[j]))>0.88){
      if(j>i){
        badpreds <- c(badpreds,j)
      }
      else if(i>j){
        badpreds <- c(badpreds,i)
      }
    }
  }
}
badpreds
finalpreds <- toppreds[,-unique(badpreds)] # remove the bad predictors found above from our chosen predictors.
length(finalpreds)# should have 27 predictors

predictors <-names(finalpreds);predictors # the final predictors to use
col_pre1<-c()
for (j in 1:length(predictors)){
  col_pre1<-c(col_pre1,which(predictors[j] == colnames(train)))
}

Wins <-as.numeric(train$Wins)
subdata1<-data.frame(Wins,train[,col_pre1])

names(subdata1)
m1 <- lm(Wins~., data=subdata1)
mean(summary(m1)$residuals^2) #MSE
summary(m1)$adj.r.squared #adjusted Rsquared
summary(m1)$r.squared #Rsquared
pred <-round(predict(m1, newdata = test))

test_ids <- 381:544
output <- data.frame(ID = test_ids, Wins= pred)
write.csv(output, 'regrun12.csv', quote = FALSE, row.names = FALSE)
