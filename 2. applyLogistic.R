######################################################
# Script to apply the logistic model to links
# resulting from the use of the search engine
# having in input the denomination of the enterprises
######################################################

# Read data
url <- read.delim("scored_links.csv")
url <- url[1:100,]
# Consider links for each enterprise
a <- split(url,url$FIRM_ID)
# Consider the link with maximum score for each enterprise
url1 <- NULL
for (i in 1:length(a)) {
  url1 <- rbind(url1,a[[i]][a[[i]]$SCORE == max(a[[i]]$SCORE),][1,])
  cat("\n",i)
}
rm(a)
gc()
url <- url1
# Elimination of duplicates
url <- url[!duplicated(url$FIRM_ID),]

# Prepare data for logistic model prediction
url$SCORE_VECTOR <- as.character(url$SCORE_VECTOR)
urls <- NULL
urls$FIRM_ID <- url$FIRM_ID
urls$LINK_POSITION <- url$LINK_POSITION
urls$URL <- url$URL
urls$telephone <- as.numeric(substring(url$SCORE_VECTOR,1,1)) 
table(urls$telephone)
urls$simpleURL <- as.numeric(substring(url$SCORE_VECTOR,2,2)) 
table(urls$simpleURL)
urls$link_position <- as.numeric(substring(url$SCORE_VECTOR,3,3)) 
table(urls$link_position)
urls$VAT <- as.numeric(substring(url$SCORE_VECTOR,4,4)) 
table(urls$VAT)
urls$municipality <- as.numeric(substring(url$SCORE_VECTOR,5,5)) 
table(urls$municipality)
urls$province <- as.numeric(substring(url$SCORE_VECTOR,6,6)) 
table(urls$province)
urls$zip_code <- as.numeric(substring(url$SCORE_VECTOR,7,7)) 
table(urls$zip_code)
urls <- as.data.frame(urls)


#urls <- read.delim("links_scored.txt")
# Apply logistic to data
load("logisticModel.RData")
summary(logistic)
urls$predictedScore <- predict(logistic, urls[,2:ncol(urls)],type="response")
pdf("hist.pdf")
hist(urls$predictedScore)
dev.off()
#-------------------------------------------------------------------------------------------
# ATTRIBUTION OF LINK/NONLINK/CROWD 
# the values reported here are based on the results obtained by the fitting of the logistic
# (see the file "results.csv" output of the models_etimation script)
urls$link <- ifelse(urls$predictedScore > 0.72,1,0)
urls$nonlink <- ifelse(urls$predictedScore <= 0.44,1,0)
urls$crowd <- ifelse(urls$predictedScore > 0.44 
                     & urls$predictedScore <= 0.72,1,0)
#-------------------------------------------------------------------------------------------

sink("results.txt")
cat("\nURL to be taken")
table(urls$link)/nrow(urls)
cat("\nURL to be excluded")
table(urls$nonlink)/nrow(urls)
cat("\nURL to be crowdsourced")
table(urls$crowd)/nrow(urls)
sink()
write.table(urls,"links_scored.txt",sep="\t",quote=F,row.names=F,col.names=T)
links <- urls[urls$link == 1, c("FIRM_ID","URL")]
write.table(links,"links_reliable.txt",quote=F,sep="\t",row.names=F,col.names=T)

save.image("applyLogistic.RData")

