# ----------------------------------------------------------------
# Script to apply random forests
# ----------------------------------------------------------------
set.seed(1234)
unlink(".RData")
library(caret)
library(randomForest)
library(ROCR)
matrixTrain <- read.delim("matrix_train.txt",sep=" ")
matrixTest <- read.delim("matrix_test.txt",sep=" ")

colnames(matrixTrain)[1201] <- "NACE4"
colnames(matrixTest)[1201] <- "NACE4"
table(matrixTrain$NACE4)
table(matrixTest$NACE4)
# load("matrixTrain.RData")
# load("matrixTest.RData")
#matrixTrain$NACE4 <- ifelse(matrixTrain$NACE4 == 2, 0, 1)
#matrixTest$NACE4 <- ifelse(matrixTest$NACE4 == 2, 0, 1)
# wrd <- read.csv("rf_200_words.csv")
# colnames(wrd) <- "words"
# words <- as.character(wrd$words)
# matrixTrain <- matrixTrain[, c("NACE4",words)]
# matrixTest <- matrixTest[, c("NACE4",words)]


#-----------------------------------------------------------------------
# Only 1's
# for (i in (3:ncol(matrixTrain))) {
#   matrixTrain[ ,i] <- ifelse(matrixTrain[ ,i] > 0, 1, 0)
#   matrixTest[ ,i] <- ifelse(matrixTest[ ,i] > 0, 1, 0)
# }
# hist(matrixTrain$carrello)
# hist(matrixTrain$shop)
table(matrixTrain$NACE4)/sum(table(matrixTrain$NACE4))
table(matrixTest$NACE4)/sum(table(matrixTest$NACE4))
t1 <- table(matrixTrain$NACE4)/nrow(matrixTrain)
t1
t2 <- table(matrixTest$NACE4)/nrow(matrixTest)
t2
quant <- round(t1[1],2)
quant
observTrain <- matrixTrain$NACE4
observTest <- matrixTest$NACE4


# ----------------------------------------------------------------
# Random Forest
matrixTrain$NACE4 <- as.factor(matrixTrain$NACE4)
matrixTest$NACE4 <- as.factor(matrixTest$NACE4)
rf <- randomForest(NACE4 ~ ., 
               # maxdepth = 3,
               importance=TRUE, 
               ntree = 1000,
               data=matrixTrain,do.trace=TRUE)
save.image()
plot(rf)
# save(rf,file="rf_model.RData")
pdf("Terms importance.pdf")
varImpPlot(rf, n.var=25, main="Importance of terms")
dev.off()
imp <- as.data.frame(importance(rf))
imp$words <- row.names(imp)
imp <- imp[order(imp$MeanDecreaseGini,decreasing=TRUE),]
row.names(imp) <- NULL
write.table(imp,"terms_importance.csv",sep=";",quote=F,row.names=F,col.names=T)

predictedTrain <- predict(rf, matrixTrain, type="response")
# hist(pred[,2])
# t <- quantile(pred[,2],quant)
# predictedTrain <- ifelse(pred[,2] > t,1,0)
tavTrain <- table(observTrain,predictedTrain)
tavTrain

predictedTest <- predict(rf, matrixTest, type="response")
# hist(pred[,2])
# t <- quantile(pred[,2],quant)
# predictedTest <- ifelse(pred[,2] > t,1,0)
tavTest <- table(observTest,predictedTest)
tavTest


#tav <- tavTrain + tavTest
tav <- tavTest
tav
sink("resultsRandomForests.txt",append=F)
cat("\n")
cat("\n--------------")
cat("\nRandom Forests")
cat("\n--------------\n")
cat("\nImportant terms\n")
imp[1:100,5:7]
#cat("\nAccuracy           ", (tav[1,1]+tav[2,2])/sum(tav))
#cat("\nSensitivity        ", tav[2, 2]/(tav[2, 1] + tav[2, 2]))
#cat("\nSpecificity        ", tav[1, 1]/(tav[1, 1] + tav[1, 2]))
# cat("\nEstimate on train   ", (tavTrain[2,1]+tavTrain[2, 2]) /sum(tavTrain), "\n")
# cat("\nEstimate on test    ", (tav[1, 2]+tav[2, 2]) /sum(tav), "\n")
cm <- confusionMatrix(t(tavTest), positive = "1")
cat("\n")
confusionMatrix(t(tavTest), positive = "1")
# F1 <- 2 * (cm$byClass[1] * cm$byClass[3]) / (cm$byClass[1] + cm$byClass[3])
# cat("\n F1 measure        ",F1,"\n")
sink()

# predictedTrain <- as.numeric(predictedTrain)
# predictedTest <- as.numeric(predictedTest)
# pdf("rf_ROC.pdf")
# predict <- prediction(predictedTrain, observTrain)
# perf <- performance(predict, "tpr", "fpr")
# plot(perf, col = "red")
# predict <- prediction(predictedTest, observTest)
# perf <- performance(predict, "tpr", "fpr")
# plot(perf, add = TRUE, col = "blue")
# title("ROC for rf (red= train, blue= test)")
# dev.off()

save.image(file="NACE.RData")
# ----------------------------------------------------------------