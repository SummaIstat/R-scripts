######################################################
# Script to fit logistic model,
# random forest and neural net 
# to links 
# resulting from the use of the search engine
# having in input the denomination of the enterprises
# for which the correct URLs are known
# (training set)
######################################################
library(caret)
# Read data (links with associated scores)
url <- read.csv("links_with_scores.csv")
url <- url[1:10000,c("COD_AZIENDA","SCORE_VECTOR","SCORE","MATCH_DOMAIN_NO_EXT")]
# Consider links for each enterprise
a <- split(url,url$COD_AZIENDA)
# Consider the link with maximum score for each enterprise
url1 <- NULL
for (i in 1:length(a)) {
  url1 <- rbind(url1,a[[i]][a[[i]]$SCORE == max(a[[i]]$SCORE),][1,])
  cat("\n",i)
}
#save(url1,file="urls_MATCH_DOMAIN_NO_EXT.RData")
url <- url1
rm(url1)
# Elimination of duplicates
urlunique <- url[!duplicated(url$COD_AZIENDA),]
url <- urlunique
# Prepare data for model fitting
url$SCORE_VECTOR <- as.character(url$SCORE_VECTOR)
url2 <- NULL
url2$ent_code <- url$COD_AZIENDA
url2$correct_Yes_No <- url$MATCH_DOMAIN_NO_EXT
table(url2$correct_Yes_No)
url2$telephone <- substring(url$SCORE_VECTOR,1,1) 
table(url2$telephone)
url2$simpleURL <- substring(url$SCORE_VECTOR,2,2) 
table(url2$simpleURL)
url2$link_position <- substring(url$SCORE_VECTOR,3,3) 
table(url2$link_position)
url2$VAT <- substring(url$SCORE_VECTOR,4,4) 
table(url2$VAT)
url2$municipality <- substring(url$SCORE_VECTOR,5,5) 
table(url2$municipality)
url2$province <- substring(url$SCORE_VECTOR,6,6) 
table(url2$province)
url2$zip_code <- substring(url$SCORE_VECTOR,7,7) 
table(url2$zip_code)
url2 <- as.data.frame(url2)
write.table(url2,"matches_DOMAIN_NO_EXT.txt",sep="\t",quote=F,row.names=F,col.names=T)
urls <- read.delim("matches_DOMAIN_NO_EXT.txt")

# Prepare training and test set (50-50)
set.seed(1234)
v <- sample(c(1:nrow(urls)),round(nrow(urls)/2))
train <- urls[v,]
test <- urls[-v,]

#train <- urls[1:round(nrow(urls)/2),]
#test <- urls[(round(nrow(urls)/2)+1):(nrow(urls)),]


table(train$correct_Yes_No)/sum(table(train$correct_Yes_No))
table(test$correct_Yes_No)/sum(table(test$correct_Yes_No))
t1 <- table(train$correct_Yes_No)/nrow(train)
t1
t2 <- table(test$correct_Yes_No)/nrow(test)
t2
quant <- round(t1[1],2)
quant

observTrain <- train$correct_Yes_No
observTest <- test$correct_Yes_No
# ----------------------------------------------------------------
# Random Forest
library(randomForest)
set.seed(1111)
train$correct_Yes_No <- as.factor(train$correct_Yes_No)
rf <- randomForest(correct_Yes_No ~ ., data = train[,2:ncol(train)], 
                   ntree=200, importance=TRUE)
plot(rf)
predictedTrain <- predict(rf, train[,2:ncol(train)],type="prob")[,2]
hist(predictedTrain)
t <- quantile(predictedTrain,quant)
predictedTrain <- ifelse(predictedTrain > t,1,0)
tavTrain <- table(observTrain,predictedTrain)
tavTrain/nrow(train)

predicted <- predict(rf, test[,2:ncol(test)],type="prob")[,2]
summary(predicted)
hist(predicted)
t <- quantile(predicted,quant)
predictedTest <- ifelse(predicted > t,1,0)
tavTest <- table(observTest,predictedTest)
tavTest/sum(tavTest)

tav <- tavTest
tav
sink("results.txt")
cat("\n")
cat("\nRANDOM FOREST")
#cat("\nAccuracy           ", (tav[1,1]+tav[2,2])/sum(tav))
#cat("\nSensitivity        ", tav[2, 2]/(tav[2, 1] + tav[2, 2]))
#cat("\nSpecificity        ", tav[1, 1]/(tav[1, 1] + tav[1, 2]))
cat("\nEstimate on train   ", (tavTrain[2,1]+tavTrain[2, 2]) /sum(tavTrain), "\n")
cat("\nEstimate on test    ", (tav[1, 2]+tav[2, 2]) /sum(tav), "\n")
cm <- confusionMatrix(t(tavTest), positive = "1")
cat("\n")
confusionMatrix(t(tavTest), positive = "1")
F1 <- 2 * (cm$byClass[1] * cm$byClass[3]) / (cm$byClass[1] + cm$byClass[3])
cat("\n F1 measure        ",F1,"\n")
sink()

predictedRF <- predicted
save(rf,file="randomForest.RData")
# ----------------------------------------------------------------
# Neural net
library(nnet)
set.seed(1111)
net <- nnet(correct_Yes_No ~ ., data = train[,2:ncol(train)], size = 3, 
            rang = 0.2, decay = 5e-04, 
            maxit = 2000)
summary(net)
predictedTrain <- predict(net, train[,2:ncol(train)],type="raw")
hist(predictedTrain)
t <- quantile(predictedTrain,quant)
predictedTrain <- ifelse(predictedTrain > t,1,0)
tavTrain <- table(observTrain,predictedTrain)
tavTrain/nrow(train)

predicted <- predict(net, test[,2:ncol(train)],type="raw")
summary(predicted)
hist(predicted)
t <- quantile(predicted,quant)
predictedTest <- ifelse(predicted > t,1,0)
tavTest <- table(observTest,predictedTest)
tavTest/sum(tavTest)

tav <- tavTest
tav
sink("results.txt",append=T)
cat("\n")
cat("\nNEURAL NETWORK")
#cat("\nAccuracy           ", (tav[1,1]+tav[2,2])/sum(tav))
#cat("\nSensitivity        ", tav[2, 2]/(tav[2, 1] + tav[2, 2]))
#cat("\nSpecificity        ", tav[1, 1]/(tav[1, 1] + tav[1, 2]))
cat("\nEstimate on train   ", (tavTrain[2,1]+tavTrain[2, 2]) /sum(tavTrain), "\n")
cat("\nEstimate on test    ", (tav[1, 2]+tav[2, 2]) /sum(tav), "\n")
cm <- confusionMatrix(t(tavTest), positive = "1")
cat("\n")
confusionMatrix(t(tavTest), positive = "1")
F1 <- 2 * (cm$byClass[1] * cm$byClass[3]) / (cm$byClass[1] + cm$byClass[3])
cat("\n F1 measure        ",F1,"\n")
sink()

predictedNnet <- predicted
save(net,file="neuralNetwork.RData")
# ----------------------------------------------------------------
# Logistic model
logistic <- glm(correct_Yes_No ~ ., family = binomial(logit), data = train[,2:ncol(train)])

summary(logistic)
predictedTrain <- predict(logistic, train[,2:ncol(train)],type="response")
hist(predictedTrain)
t <- quantile(predictedTrain,quant)
predictedTrain <- ifelse(predictedTrain > t,1,0)
tavTrain <- table(observTrain,predictedTrain)
tavTrain/nrow(train)

predicted <- predict(logistic, test[,2:ncol(train)],type="response")
summary(predicted)
hist(predicted)
t <- quantile(predicted,quant)
predictedTest <- ifelse(predicted > t,1,0)
tavTest <- table(observTest,predictedTest)
tavTest/sum(tavTest)


tav <- tavTest
tav
sink("results.txt",append=T)
cat("\n")
cat("\nLOGISTIC")
#cat("\nAccuracy           ", (tav[1,1]+tav[2,2])/sum(tav))
#cat("\nSensitivity        ", tav[2, 2]/(tav[2, 1] + tav[2, 2]))
#cat("\nSpecificity        ", tav[1, 1]/(tav[1, 1] + tav[1, 2]))
cat("\nEstimate on train   ", (tavTrain[2,1]+tavTrain[2, 2]) /sum(tavTrain), "\n")
cat("\nEstimate on test    ", (tav[1, 2]+tav[2, 2]) /sum(tav), "\n")
cm <- confusionMatrix(t(tavTest), positive = "1")
cat("\n")
confusionMatrix(t(tavTest), positive = "1")
F1 <- 2 * (cm$byClass[1] * cm$byClass[3]) / (cm$byClass[1] + cm$byClass[3])
cat("\n F1 measure        ",F1,"\n")
sink()

predictedLogistic <- predicted
save(logistic,file="LogisticModel.RData")
#----------------------------------------------------------------
library(ROCR)
pdf("precision_recall.pdf")
split.screen(c(2,1))
screen(1)
predLogistic <- prediction(predictedLogistic, observTest)
perfLogistic <- performance(predLogistic, "tpr", "fpr")
plot(perfLogistic,col=palette()[1])
predRF <- prediction(predictedRF, observTest)
perfRF <- performance(predRF, "tpr", "fpr")
plot(perfRF,col=palette()[2],add=T)
predNNet <- prediction(predictedNnet , observTest)
perfNNet  <- performance(predNNet , "tpr", "fpr")
plot(perfNNet ,col=palette()[3],add=T)
title("ROC",cex.main=0.9)
legend("bottomright", legend = c(
  "Logistic",
  "Random Forest",
  "Neural Net"
), 
col = palette()[1:3], 
ncol = 2, cex = 0.6, lwd = 3, text.font = 1)
screen(2)
predLogistic <- prediction(predictedLogistic, observTest)
perfLogistic <- performance(predLogistic, "prec", "rec")
plot(perfLogistic,col=palette()[1])
predRF <- prediction(predictedRF, observTest)
perfRF <- performance(predRF, "prec", "rec")
plot(perfRF,col=palette()[2],add=T)
predNNet <- prediction(predictedNnet , observTest)
perfNNet  <- performance(predNNet , "prec", "rec")
plot(perfNNet ,col=palette()[3],add=T)
title("Precision and recall",cex.main=0.9)
legend("bottomleft", legend = c(
                                "Logistic",
                                "Random Forest",
                                "Neural Net"
                                 ), 
       col = palette()[1:3], 
       ncol = 2, cex = 0.6, lwd = 3, text.font = 1)
dev.off()

# ------------- Take the Logistic model -------------------------
predicted <- predictedLogistic

ordinati <- cbind(test,predicted)

aggr <- ordinati[,c("ent_code","predicted","correct_Yes_No")]
colnames(aggr) <- c("ident","predicted","corretto")
aggr$gruppo <- cut(aggr$predicted, breaks=quantile(aggr$predicted,probs=seq(0,1,0.1)),include.lowest=TRUE)
aggr$corretti <- ifelse(aggr$corretto == 1,1,0)
aggr$errati <- ifelse(aggr$corretto == 1,0,1)
res <- aggregate(aggr[,c("corretti","errati")],by=list(aggr$gruppo),FUN=sum)

res$errore <- res$errati / (res$errati+res$corretti)
res$group <- c(1:nrow(res))
colnames(res) <- c("score_class","true","false","classification_error","group")
 
pdf("plot_logistic_MATCH_DOMAIN_NO_EXT.pdf")
plot(res$group,res$true,type="l",col="red",ylim=c(0,max(res$true)))
lines(res$group,res$false,type="l",col="blue")
legend("topleft","blue=errors, red=correct")
title("True and false by class of score (logistic model)")
dev.off()

write.table(res,"res_logistico_MATCH_DOMAIN_NO_EXT.txt",sep="\t",quote=F,row.names=F,col.names=T)

sink("logistic_MATCH_DOMAIN_NO_EXT2.txt")
attach(res)
cat("\n Logistic with 10 classes")
cat("\n Upper five classes taken as correct links \n")
cat("\n")
res
cat("\n")
recall <- sum(true[6:10])/sum(true)
cat("\n Recall: ",recall)
precision <- sum(true[6:10])/(sum(true[6:10])+sum(false[6:10]))
cat("\n Precision: ",precision)
f1 <- 2*(precision*recall)/(precision+recall)
cat("\n F1 measure: ",f1)
cat("\n")
cat("\n")
cat("\n")
cat("\n Lower four classes taken as incorrect links \n")
cat("\n")

recall <- sum(true[1:4])/sum(true)
cat("\n Recall: ",recall)
precision <- sum(true[1:4])/(sum(true[1:4])+sum(false[1:4]))
cat("\n Precision: ",precision)
f1 <- 2*(precision*recall)/(precision+recall)
cat("\n F1 measure: ",f1)

perc <- (sum(true[5]+ sum(false[5])))/(sum(true[1:10])+sum(false[1:10]))
pos <- (sum(true[5])/(sum(true[1:10])))
cat("\n Percentage of enterprises to submit to crowdsourcing (5th class): ", perc)
cat("\n with expected positives: ", pos)
sink()

save.image("URLs_21_07_16.RData")
detach(res)
links <- urls[urls$correct_Yes_No == 1,c("ent_code","link_position")]
write.table(links,"links.txt",sep="\t",quote=F,row.names=F,col.names=T)
#------------------------------------------------------------------------------
