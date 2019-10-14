#=========================================================
# Computation of estimates on e-commerce
#=========================================================
# Version with design based estimates correctly calibrated
#=========================================================

# Input files:
# file_inp1 = predicted values (0/1) in the sample 
# file_inp2 = predicted probabilities in the sample 
# file_inp3 = predicted values (0/1) in the population
# file_inp4 = predicted probabilities in the population
# file_inp5 = survey data
# file_inp6 = population register (ASIA)

#---------------------------------------------------

# Output files:
# file_out1 = estimates design based
# file_out2 = estimates combined
# file_out3 = estimates synthetic (model based)
# file_out4 = estimates compared
# file_out5 = estimates compared (plots) 

#---------------------------------------------------
#
setwd("G:/00_ICT_2018/ecommerce 2018")
file_inp1 = "RDFbalanced_train.txt"
file_inp2 = "prob_RDF_train.txt"
file_inp3 = "RDFbalanced.txt"
file_inp4 = "prob_RDF.txt"
file_inp5 = "surveyICT2018_ateco2.csv"
file_inp6 = "asia2018.csv"
file_out1 = "ICT_estimates_design_coef_cal_2.csv"
file_out2 = "ICT_estimates_combined.csv"
file_out3 = "ICT_estimates2018_model2018_based.csv"
file_out4 = "Web_ordering_estimates_compared.csv"
file_out5 = "Web_ordering_estimates_compared_plots.pdf"

library(ReGenesees)

# ============================================================
# Read population
# ============================================================
#
asia <- read.csv(file_inp6)
asia$dom3 <- as.factor(asia$dom3)
asia$naceICT <- as.factor(asia$naceICT)
asia$naceist <- as.factor(asia$naceist)
asia$mac4 <- as.factor(asia$mac4)
asia$clad4 <- as.factor(asia$clad4)
asia$reg21 <- as.factor(asia$reg21)
asia$TN <- as.factor(asia$TN)
asia$ATECO2 <- as.factor(asia$ATECO2)
asia <- asia[,c("codice_unita",
                "dom3",
                "naceICT",
                "naceist",
                "mac4",
                "clad4",
                "reg21",
                "TN",
                "ATECO2",
                "addetti",
                "imprese")]
#
# ============================================================
# Read survey data
# ============================================================
#
survey <- read.csv(file_inp5,dec=".",stringsAsFactors = FALSE)
survey$dom3 <- as.factor(survey$dom3)
survey$naceICT <- as.factor(survey$naceICT)
survey$naceist <- as.factor(survey$naceist)
survey$mac4 <- as.factor(survey$mac4)
survey$clad4 <- as.factor(survey$clad4)
survey$reg21 <- as.factor(survey$reg21)
survey$ATECO2 <- as.factor(survey$ateco2)
survey$TN <- as.factor(survey$TN)
survey$strato <- as.factor(survey$strato)
survey$addetti <- survey$addetti
survey$imprese <- survey$imprese
survey$WEB <- survey$e_web
survey$WEBORD <- survey$e_webord

survey$WEB <- ifelse(is.na(survey$WEB),0,survey$WEB)
table(survey$WEB,useNA="ifany")

survey$WEBORD <- ifelse(is.na(survey$WEBORD),0,survey$WEBORD)
table(survey$WEBORD,useNA="ifany")
survey <- survey[,c("codice_unita","coef_cal","WEB",
                    "WEBORD","coef","strato",
                    "dom3","naceICT","naceist","mac4","clad4","reg21",
                    "ATECO2","TN",
                    "addetti","imprese")]
# write.table(survey,"survey_reduced.csv",sep=",",row.names=F,col.names=T,quote=F)

# ============================================================
# Observed and predicted units
# These are the observed units also with the predictions 
# made with scraped websites
# ============================================================

train_pred <- read.delim(file_inp1,header=FALSE,sep=" ")
train_prob <- read.delim(file_inp2,sep=" ",header=FALSE)
train <- cbind(train_pred,train_prob)
train <- train[,c(1,2,4)]
sum(duplicated(train$codice_unita))
colnames(train) <- c("codice_unita","pred","pred_prob")
sum(train$pred_prob)
# write.table(train,"train_probs.txt",sep="\t",row.names=F,col.names=T,quote=F)

obs_and_pred <- merge(survey,train)
obs_and_pred$diff <- obs_and_pred$pred_prob - obs_and_pred$WEBORD
obs_and_pred$coef <- NULL

# ============================================================
# Only observed units
# These are the observed units that declared a website (WEB == 1)
# but whose website was neither found nor scraped
# ============================================================

obs <- survey[!survey$codice_unita %in% obs_and_pred$codice_unita & survey$WEB == 1, ]

# ============================================================
# Predicted units
# These are the units in the population with websites
# whose retrieval and scraping was successful
# ============================================================
test_pred <- read.delim(file_inp3,header=FALSE,sep=" ")
test_prob <- read.delim(file_inp4,sep=" ",header=FALSE)
test <- cbind(test_pred,test_prob)
test <- test[,c(1,2,4)]
colnames(test) <- c("codice_unita","pred","pred_prob")

# they must be present in the frame
pred <- merge(test,asia) 
pred$WEB <- 1
pred$WEBORD <- pred$pred

# ============================================================
# Estimation of population with websites
# (estimates from the survey)
# ============================================================
website_tot <- sum(survey$WEB * survey$coef_cal)

website_ATECO2 <- tapply(survey$WEB * survey$coef_cal,survey$ATECO2,sum)/tapply(survey$coef_cal,survey$ATECO2,sum)
website_ATECO2

website_naceist <- tapply(survey$WEB * survey$coef_cal,survey$naceist,sum)/tapply(survey$coef_cal,survey$naceist,sum)
website_naceist

website_naceICT <- tapply(survey$WEB * survey$coef_cal,survey$naceICT,sum)/tapply(survey$coef_cal,survey$naceICT,sum)
website_naceICT

website_clad4 <- tapply(survey$WEB * survey$coef_cal,survey$clad4,sum)/tapply(survey$coef_cal,survey$clad4,sum)
website_clad4

website_reg21 <- tapply(survey$WEB * survey$coef_cal,survey$reg21,sum)/tapply(survey$coef_cal,survey$reg21,sum)
website_reg21

website_dom3 <- tapply(survey$WEB * survey$coef_cal,survey$dom3,sum)/tapply(survey$coef_cal,survey$dom3,sum)
website_dom3

websites <- c(website_ATECO2,
              website_naceist[2:27],
              website_naceICT[2],
              website_clad4[2:4],
              website_dom3[2:16],
              website_reg21[2:21])

# ============================================================
# Design based estimates
# (estimates from the survey)
# ============================================================
options(RG.lonely.psu = "average")

survey$naceist.into.mac4 <- survey$naceist %into% survey$mac4
asia$naceist.into.mac4 <- asia$naceist %into% asia$mac4

head (survey)

template.popolazione <- pop.template(data= survey, calmodel= ~ (imprese + addetti):(mac4:(naceist.into.mac4 + naceICT + clad4 + reg21) + TN:(naceist + naceICT)) - 1, partition= FALSE)

totali.noti <- fill.template(universe= asia, template= template.popolazione, mem.frac= 10)


camp <- e.svydesign(data= survey, ids= ~ codice_unita, strata= ~ strato, 
                    weights= ~ coef, fpc= NULL, self.rep.str= NULL, check.data= TRUE)

camp_cal<- e.calibrate(design = camp, df.population = totali.noti,
                       calmodel = ~(imprese + addetti):(mac4:(naceist.into.mac4 + naceICT + clad4 + reg21) + TN:(naceist + naceICT)) - 1,
                       partition = FALSE, calfun = "raking", bounds = c(0.01, 2.585), aggregate.stage = NULL,
                       sigma2 = ~addetti, maxit = 50, epsilon = 1e-07, force = TRUE)

webord_Total <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Mean", 
                            # by =~naceist - 1,
                            conf.int= TRUE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)

Zapsmall(webord_Total)

webord_naceist <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Mean", 
                            by =~naceist - 1,
                            conf.int= TRUE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)

Zapsmall(webord_naceist)

webord_naceICT <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Mean", 
                          by =~naceICT - 1,
                          conf.int= TRUE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)

Zapsmall(webord_naceICT)

webord_clad4 <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Mean", 
                         by =~clad4 - 1,
                         conf.int= TRUE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)

Zapsmall(webord_clad4)

webord_dom3 <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Mean", 
                          by =~dom3 - 1,
                          conf.int= TRUE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)

Zapsmall(webord_dom3)

webord_reg21 <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Mean", 
                           by =~reg21 - 1,
                           conf.int= TRUE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)

Zapsmall(webord_reg21)

webord_ATECO2 <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Mean", 
                           by =~ATECO2 - 1,
                           conf.int= TRUE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)

Zapsmall(webord_ATECO2)


out <- NULL
out$dom <- c("Total",rep("naceist",27),
             rep("naceICT",2),
             rep("clad4",4),
             rep("dom3",16),
             rep("reg21",21),
             rep("ATECO2",62))
out$estimate <- c("Total",
                  levels(survey$naceist),
                  levels(survey$naceICT),
                  levels(survey$clad4),
                  levels(survey$dom3),
                  levels(survey$reg21),
                  levels(survey$ATECO2))
out$sample <- c(nrow(survey),
                as.numeric(table(survey$naceist)),
                as.numeric(table(survey$naceICT)),
                as.numeric(table(survey$clad4)),
                as.numeric(table(survey$dom3)),
                as.numeric(table(survey$reg21)),
                as.numeric(table(survey$ATECO2)))
out$population <- c(nrow(asia),
                    as.numeric(table(asia$naceist)),
                    as.numeric(table(asia$naceICT)),
                    as.numeric(table(asia$clad4)),
                    as.numeric(table(asia$dom3)),
                    as.numeric(table(asia$reg21)),
                    as.numeric(table(asia$ATECO2)))
out$websites <- c(round(website_tot),
                  round(website_naceist),
                  round(website_naceICT),
                  round(website_clad4),
                  round(website_dom3),
                  round(website_reg21),
                  round(website_ATECO2))
# out$websites <- c(round(website$Mean*nrow(asia)),
#                   round(website_naceist$Mean*as.numeric(table(asia$naceist))),
#                   round(website_naceICT$Mean*as.numeric(table(asia$naceICT))),
#                   round(website_clad4$Mean*as.numeric(table(asia$clad4))),
#                   round(website_dom3$Mean*as.numeric(table(asia$dom3))),
#                   round(website_reg21$Mean*as.numeric(table(asia$reg21))),
#                   round(website_ATECO2$Mean*as.numeric(table(asia$ATECO2))))
out$webordMean <- c(round(webord_Total$Mean*100,2),
                    round(webord_naceist$Mean*100,2),
                    round(webord_naceICT$Mean*100,2),
                    round(webord_clad4$Mean*100,2),
                    round(webord_dom3$Mean*100,2),
                    round(webord_reg21$Mean*100,2),
                    round(webord_ATECO2$Mean*100,2))
out$webordLower <- c(round(webord_Total$`CI.l(95%)`*100,2),
                     round(webord_naceist$`CI.l(95%)`*100,2),
                     round(webord_naceICT$`CI.l(95%)`*100,2),
                     round(webord_clad4$`CI.l(95%)`*100,2),
                     round(webord_dom3$`CI.l(95%)`*100,2),
                     round(webord_reg21$`CI.l(95%)`*100,2),
                     round(webord_ATECO2$`CI.l(95%)`*100,2))
out$webordUpper <- c(round(webord_Total$`CI.u(95%)`*100,2),
                     round(webord_naceist$`CI.u(95%)`*100,2),
                     round(webord_naceICT$`CI.u(95%)`*100,2),
                     round(webord_clad4$`CI.u(95%)`*100,2),
                     round(webord_dom3$`CI.u(95%)`*100,2),
                     round(webord_reg21$`CI.u(95%)`*100,2),
                     round(webord_ATECO2$`CI.u(95%)`*100,2))
out <- as.data.frame((out))


write.table(out,file_out1,sep=";",row.names=F,col.names=TRUE,quote=FALSE)

# ============================================================
# Combined estimates, sum of
# obs 
# obs_and_pred
# pred
# ============================================================
# obs_and_pred

camp_U1 <- e.svydesign(obs_and_pred, ids= ~ codice_unita, strata= ~ strato, 
                       weights= ~ coef_cal, fpc= NULL, self.rep.str= NULL, check.data= TRUE)

reached_websites <- website_tot / nrow(asia)
options(RG.lonely.psu = "average")
tot_U1= pop.template(data = obs_and_pred, 
                     # calmodel = ~ATECO2 - 1
                     calmodel = ~ATECO2 + naceICT + clad4 + reg21 - 1
) 
tot_U1 = fill.template(universe = asia, template = tot_U1)
tot_U1_web = tot_U1
tot_U1_web[,] = tot_U1 * reached_websites
sum(tot_U1_web[1:62])

bounds.hint(camp_U1, tot_U1_web)
#pop.desc(tot_cov)
camp_cal_U1 = e.calibrate(design = camp_U1, df.population = tot_U1_web,
                          calmodel = ~ATECO2 + naceICT + clad4 + reg21 - 1,
                          calfun = "linear", bounds = c(0.01, 6.915), 
                          aggregate.stage = NULL, 
                          # sigma2 = ~ADDETTI,
                          maxit = 50, epsilon = 1e-07, force = TRUE)


summary(weights(camp_cal_U1))
g.range(camp_cal_U1)
check.cal(camp_cal_U1)

#------------------------------------------------------------------------------------
# U1 - U2

camp_U2 <- e.svydesign(data= obs, ids= ~ codice_unita, strata= ~ strato, 
                       weights= ~ coef_cal, fpc= NULL, self.rep.str= NULL, check.data= TRUE)

# not_reached_websites <- (nrow(asia)*website$Mean - 
#                         nrow(ict[ict$mode=="pred" | ict$mode == "obs_and_pred",])) / nrow(asia)

not_reached_websites <- (website_tot - nrow(pred)) / nrow(asia)

options(RG.lonely.psu = "average")
tot_U2 = pop.template(data = obs, 
                      calmodel = ~ATECO2 + naceist + naceICT + clad4 + reg21 - 1) 
tot_U2 = fill.template(universe = asia, template = tot_U2)
sum(tot_U2[1:62])
tot_U2_web = tot_U2
tot_U2_web[,] = tot_U2 * not_reached_websites
sum(tot_U2_web[1:62])

bounds.hint(camp_U2, tot_U2_web)
#pop.desc(tot_cov)
camp_cal_U2 = e.calibrate(design = camp_U2, df.population = tot_U2_web,
                          calmodel = ~ATECO2 + naceist + naceICT + clad4 + reg21 - 1,
                          calfun = "linear", bounds = c(0.01, 10.978), 
                          aggregate.stage = NULL, 
                          # sigma2 = ~ADDETTI,
                          maxit = 50, epsilon = 1e-07, force = TRUE)


summary(weights(camp_cal_U2))
g.range(camp_cal_U2)
check.cal(camp_cal_U2)
# sum(camp_cal_U2$variables$coef_cal.cal*camp_cal_U2$variables$WEBORD)
# sum(camp_cal_U2$variables$WEBORD)
# sum(camp_cal_U2$variables$coef_cal.cal)

#------------------------------------------------------------------------------------
# Estimates of web-ordering

WEBORDTotal <- sum(obs_and_pred$pred_prob) + 
  sum(pred$pred_prob) +
  svystatTM(camp_cal_U1, y= ~ diff,  estimator= "Total", 
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total +
  svystatTM(camp_cal_U2, y= ~ WEBORD ,  estimator= "Total", 
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total 

Zapsmall(WEBORDTotal)

WEBORDMean <- (sum(obs_and_pred$pred_prob) + 
                 sum(pred$pred_prob) +
                 svystatTM(camp_cal_U1, y= ~ diff,  estimator= "Total", 
                           conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total +
                 svystatTM(camp_cal_U2, y= ~ WEBORD ,  estimator= "Total", 
                           conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total ) / nrow(asia)
Zapsmall(WEBORDMean)

WEBORD_ATECO2 <- tapply(obs_and_pred$pred_prob,obs_and_pred$ATECO2,sum) +
  tapply(pred$pred_prob,pred$ATECO2,sum) +
  svystatTM(camp_cal_U1, y= ~ diff,  estimator= "Total", 
            by =~ATECO2 - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total +
  svystatTM(camp_cal_U2, y= ~ WEBORD ,  estimator= "Total", 
            by =~ATECO2 - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total 

Zapsmall(WEBORD_ATECO2)

WEBORD_naceist <- tapply(obs_and_pred$pred_prob,obs_and_pred$naceist,sum) +
  tapply(pred$pred_prob,pred$naceist,sum) +
  svystatTM(camp_cal_U1, y= ~ diff,  estimator= "Total", 
            by =~naceist - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total +
  svystatTM(camp_cal_U2, y= ~ WEBORD ,  estimator= "Total", 
            by =~naceist - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total 

Zapsmall(WEBORD_naceist)

WEBORD_naceICT <- tapply(obs_and_pred$pred_prob,obs_and_pred$naceICT,sum) +
  tapply(pred$pred_prob,pred$naceICT,sum) +
  svystatTM(camp_cal_U1, y= ~ diff,  estimator= "Total", 
            by =~naceICT - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total +
  svystatTM(camp_cal_U2, y= ~ WEBORD ,  estimator= "Total", 
            by =~naceICT - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total 

Zapsmall(WEBORD_naceICT)

WEBORD_clad4 <- tapply(obs_and_pred$pred_prob,obs_and_pred$clad4,sum) +
  tapply(pred$pred_prob,pred$clad4,sum) +
  svystatTM(camp_cal_U1, y= ~ diff,  estimator= "Total", 
            by =~clad4 - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total +
  svystatTM(camp_cal_U2, y= ~ WEBORD ,  estimator= "Total", 
            by =~clad4 - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total 
Zapsmall(WEBORD_clad4)


WEBORD_dom3 <-  tapply(obs_and_pred$pred_prob,obs_and_pred$dom3,sum) +
  tapply(pred$pred_prob,pred$dom3,sum) +
  svystatTM(camp_cal_U1, y= ~ diff,  estimator= "Total", 
            by =~dom3 - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total +
  svystatTM(camp_cal_U2, y= ~ WEBORD ,  estimator= "Total", 
            by =~dom3 - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total 

Zapsmall(WEBORD_dom3)

WEBORD_reg21 <- tapply(obs_and_pred$pred_prob,obs_and_pred$reg21,sum) +
  tapply(pred$pred_prob,pred$reg21,sum) +
  svystatTM(camp_cal_U1, y= ~ diff,  estimator= "Total", 
            by =~reg21 - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total +
  svystatTM(camp_cal_U2, y= ~ WEBORD ,  estimator= "Total", 
            by =~reg21 - 1,
            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)$Total 

Zapsmall(WEBORD_reg21)


out <- NULL
out$dom <- c("Total",rep("naceist",27),
             rep("naceICT",2),
             rep("clad4",4),
             rep("dom3",16),
             rep("reg21",21),
             rep("ATECO2",62))
out$estimate <- c("Total",
                  levels(as.factor(obs_and_pred$naceist)),
                  levels(as.factor(obs_and_pred$naceICT)),
                  levels(as.factor(obs_and_pred$clad4)),
                  levels(as.factor(obs_and_pred$dom3)),
                  levels(as.factor(obs_and_pred$reg21)),
                  levels(as.factor(obs_and_pred$ATECO2)))
out$sample <- c(nrow(survey),
                as.numeric(table(survey$naceist)),
                as.numeric(table(survey$naceICT)),
                as.numeric(table(survey$clad4)),
                as.numeric(table(survey$dom3)),
                as.numeric(table(survey$reg21)),
                as.numeric(table(survey$ATECO2)))
out$population <- c(nrow(asia),
                    as.numeric(table(asia$naceist)),
                    as.numeric(table(asia$naceICT)),
                    as.numeric(table(asia$clad4)),
                    as.numeric(table(asia$dom3)),
                    as.numeric(table(asia$reg21)),
                    as.numeric(table(asia$ATECO2)))
# out$websites <- c(round(website_tot),
#                   round(website_naceist),
#                   round(website_naceICT),
#                   round(website_clad4),
#                   round(website_dom3),
#                   round(website_reg21))
out$WEBORD <- c(round(WEBORDTotal),
                round(WEBORD_naceist),
                round(WEBORD_naceICT),
                round(WEBORD_clad4),
                round(WEBORD_dom3),
                round(WEBORD_reg21),
                round(WEBORD_ATECO2))
out <- as.data.frame((out))
#out$websites_100 <- round(out$websites * 100 / out$population,2)
out$WEBORD_100 <- round(out$WEBORD * 100 / out$population,2)

write.table(out,file_out2,sep=";",row.names=F,col.names=TRUE,quote=FALSE)

# ============================================================
# Full model based estimates 
# (synthetic estimates)
# ============================================================

obs_and_pred$strato <- NULL
obs_and_pred$coef_cal <- NULL
obs_and_pred$diff <- NULL
preds <- rbind(obs_and_pred,pred)
preds$WEBORD <- preds$pred_prob

preds$stratum <- as.factor(paste(preds$naceist,preds$clad4,preds$reg21,sep=""))
preds$wgt <- nrow(asia) / nrow(preds)

# -------------------------------
# sink("new.txt")
# sum(preds$WEBORD)
# sum(preds$pred_prob)
# sum(preds$pred_prob*preds$wgt) / sum(preds$wgt)
# table(preds$ATECO2)
# table(preds$naceICT)
# table(preds$clad4)
# table(preds$reg21)
# tot_cov
# sink()


camp <- e.svydesign(data= preds, ids= ~ codice_unita, strata= ~ stratum, 
                    weights= ~ wgt, fpc= NULL, self.rep.str= NULL, check.data= TRUE)
options(RG.lonely.psu = "average")
tot_cov = pop.template(data = asia, 
                       calmodel = ~ATECO2 + naceICT + clad4 + dom3 + reg21 - 1) 
tot_cov = fill.template(universe = asia, template = tot_cov)
sum(tot_cov[1:62])
sum(tot_cov[63])
tot_cov_web = tot_cov
tot_cov_web[,] =  tot_cov * websites
sum(tot_cov_web[1:62])

bounds.hint(camp, tot_cov_web)
#pop.desc(tot_cov)
camp_cal = e.calibrate(design = camp, df.population = tot_cov_web,
                       calmodel = ~ATECO2 + naceICT + clad4 + dom3 + reg21 - 1,
                       calfun = "linear", bounds = c(0.01, 2.00), aggregate.stage = NULL, 
                       # sigma2 = ~ADDETTI,
                       maxit = 50, epsilon = 1e-07, force = TRUE)

summary(weights(camp_cal))
g.range(camp_cal)
check.cal(camp_cal)

#------------------------------------------------------------------------------------
# Estimates of web ordering

WEBORDTotal <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Total", 
                         conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(WEBORDTotal)

WEBORDMean <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Mean", 
                        conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(WEBORDMean)

WEBORD_ATECO2 <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Total", 
                           by =~ATECO2 - 1,
                           conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(WEBORD_ATECO2)

WEBORD_naceist <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Total", 
                            by =~naceist - 1,
                            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(WEBORD_naceist)


WEBORD_naceICT <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Total", 
                            by =~naceICT - 1,
                            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(WEBORD_naceICT)


WEBORD_clad4 <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Total", 
                          by =~clad4 - 1,
                          conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(WEBORD_clad4)


WEBORD_dom3 <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Total", 
                         by =~dom3 - 1,
                         conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(WEBORD_dom3)


WEBORD_reg21 <- svystatTM(camp_cal, y= ~ WEBORD ,  estimator= "Total", 
                          by =~reg21 - 1,
                          conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(WEBORD_reg21)

out <- NULL
out$dom <- c("Total",rep("naceist",27),
             rep("naceICT",2),
             rep("clad4",4),
             rep("dom3",16),
             rep("reg21",21),
             rep("ATECO2",62))
out$estimate <- c("Total",
                  levels(preds$naceist),
                  levels(preds$naceICT),
                  levels(preds$clad4),
                  levels(preds$dom3),
                  levels(preds$reg21),
                  levels(preds$ATECO2))
out$sample <- c(nrow(survey),
                as.numeric(table(survey$naceist)),
                as.numeric(table(survey$naceICT)),
                as.numeric(table(survey$clad4)),
                as.numeric(table(survey$dom3)),
                as.numeric(table(survey$reg21)),
                as.numeric(table(survey$ATECO2)))
out$population <- c(nrow(asia),
                    as.numeric(table(asia$naceist)),
                    as.numeric(table(asia$naceICT)),
                    as.numeric(table(asia$clad4)),
                    as.numeric(table(asia$dom3)),
                    as.numeric(table(asia$reg21)),
                    as.numeric(table(asia$ATECO2)))
# out$websites <- c(round(website_tot),
#                   round(website_naceist),
#                   round(website_naceICT),
#                   round(website_clad4),
#                   round(website_dom3),
#                   round(website_reg21))
out$WEBORD <- c(round(WEBORDTotal$Total),
                round(WEBORD_naceist$Total.WEBORD),
                round(WEBORD_naceICT$Total.WEBORD),
                round(WEBORD_clad4$Total.WEBORD),
                round(WEBORD_dom3$Total.WEBORD),
                round(WEBORD_reg21$Total.WEBORD),
                round(WEBORD_ATECO2$Total.WEBORD))
out <- as.data.frame((out))
#out$websites_100 <- round(out$websites * 100 / out$population,2)
out$WEBORD_100 <- round(out$WEBORD * 100 / out$population,2)


write.table(out,file_out3,sep=";",row.names=F,col.names=TRUE,quote=FALSE)

# ============================================================
# Comparison among estimates
# (tables and plots)
# ============================================================

est0 <- read.csv2(file_out1,dec=".")
colnames(est0)[1] <- c("type")
colnames(est0)[2] <- c("dom")
colnames(est0)[6] <- c("design")
colnames(est0)[7] <- c("design_lower")
colnames(est0)[8] <- c("design_upper")

est1 <- read.csv2(file_out3,dec=".")
est1 <- est1[,c("dom","estimate","WEBORD_100")]
colnames(est1) <- c("type","dom","model_based")

est2 <- read.csv2(file_out2,dec=".")
est2 <- est2[,c("dom","estimate","WEBORD_100")]
colnames(est2) <- c("type","dom","combined")


estims <- merge(est0,est1,by=c("dom","type")) 
estims <- merge(estims,est2,by=c("dom","type")) 
# estims <- estims[order(estims$websites),]



pdf(file=file_out5)

# ---------------- cl44 ------------------------------------
est <- estims[estims$type == "clad4" | estims$type == "Total",]
num <- length(droplevels(est$dom))
maxi = (max(c(est$combined),(est$design_upper),(est$model_based)))
plot(est$design_upper, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 3,cex.lab = 1.3,
     xlab = "", 
     ylab = "Web ordering % ",
     ylim = c(0,maxi), axes = F)
points(est$design, col = "darkgreen", lty = 4, type = "p", pch = "°", cex = 2)
points(est$design_lower, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 2)
points(est$model_based, col = "darkred", lty = 4, type = "p", pch = "+", cex = 2)
points(est$combined, col = "blue", lty = 4, type = "p", pch = "*", cex = 2)

abline(0,0)
axis(1, at=1:num, labels = est$dom,las=2)
axis(2, at=seq(0,maxi+10,5), labels=seq(0,maxi+10,5))
legend("topleft",
       legend = c("Design based","Model based","Combined"), 
       pch = c("°","+","*"),
       col = c("darkgreen","darkred","blue"),
       ncol = 1, cex = 0.9, text.font = 1.5)
title("Web ordering by 4 classes of employees",cex.main=1)



# ---------------- mac4 * cl4 ------------------------------------
est <- estims[estims$type == "dom3" | estims$type == "Total",]
num <- length(droplevels(est$dom))
maxi = max(c(est$combined),(est$design_upper),(est$model_based))+5
plot(est$design_upper, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 3,cex.lab = 1.3,
     xlab = "", 
     ylab = "Web ordering % ",
     ylim = c(0,maxi), axes = F)
points(est$design, col = "darkgreen", lty = 4, type = "p", pch = "°", cex = 2)
points(est$design_lower, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 2)
points(est$model_based, col = "darkred", lty = 4, type = "p", pch = "+", cex = 2)
points(est$combined, col = "blue", lty = 4, type = "p", pch = "*", cex = 2)

abline(0,0)
axis(1, at=1:num, labels = est$dom,las=2)
axis(2, at=seq(0,maxi+10,5), labels=seq(0,maxi+10,5))
legend("topleft",
       legend = c("Design based","Model based","Combined"), 
       pch = c("°","+","*"),
       col = c("darkgreen","darkred","blue"),
       ncol = 1, cex = 0.9, text.font = 1.5)
title("Web ordering by 4 groups NACE and 4 classes of employees",cex.main=1)

# ---------------- naceist ------------------------------------
est <- estims[estims$type == "naceist" | estims$type == "Total",]
num <- length(droplevels(est$dom))
maxi = (max(c(est$combined),(est$design_upper),(est$model_based)))
plot(est$design_upper, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 3,cex.lab = 1.3,
     xlab = "", 
     ylab = "Web ordering % ",
     ylim = c(0,maxi), axes = F)
points(est$design, col = "darkgreen", lty = 4, type = "p", pch = "°", cex = 2)
points(est$design_lower, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 2)
points(est$model_based, col = "darkred", lty = 4, type = "p", pch = "+", cex = 2)
points(est$combined, col = "blue", lty = 4, type = "p", pch = "*", cex = 2)

abline(0,0)
axis(1, at=1:num, labels = est$dom,las=2)
axis(2, at=seq(0,maxi+10,5), labels=seq(0,maxi+10,5))
legend("topleft",
       legend = c("Design based","Model based","Combined"), 
       pch = c("°","+","*"),
       col = c("darkgreen","darkred","blue"),
       ncol = 1, cex = 0.9, text.font = 1.5)
title("Web ordering by 27 groups NACE",cex.main=1)

# ---------------- regions ------------------------------------
est <- estims[estims$type == "reg21" | estims$type == "Total",]
num <- length(droplevels(est$dom))
maxi = max(c(est$combined),(est$design_upper),(est$model_based))+5
plot(est$design_upper, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 3,cex.lab = 1.3,
     xlab = "", 
     ylab = "Web ordering % ",
     ylim = c(0,maxi), axes = F)
points(est$design, col = "darkgreen", lty = 4, type = "p", pch = "°", cex = 2)
points(est$design_lower, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 2)
points(est$model_based, col = "darkred", lty = 4, type = "p", pch = "+", cex = 2)
points(est$combined, col = "blue", lty = 4, type = "p", pch = "*", cex = 2)

abline(0,0)
axis(1, at=1:num, labels = est$dom,las=2)
axis(2, at=seq(0,maxi+10,5), labels=seq(0,maxi+10,5))
legend("topleft",
       legend = c("Design based","Model based","Combined"), 
       pch = c("°","+","*"),
       col = c("darkgreen","darkred","blue"),
       ncol = 1, cex = 0.9, text.font = 1.5)
title("Web ordering by regions",cex.main=1)

# ---------------- ICTs ------------------------------------
est <- estims[estims$type == "naceICT" | estims$type == "Total",]
num <- length(droplevels(est$dom))
# maxi = (max(c(est$combined),(est$design_upper),(est$model_based)))
maxi = 25
plot(est$design_upper, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 3,cex.lab = 1.3,
     xlab = "", 
     ylab = "Web ordering % ",
     ylim = c(0,maxi), axes = F)
points(est$design, col = "darkgreen", lty = 4, type = "p", pch = "°", cex = 2)
points(est$design_lower, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 2)
points(est$model_based, col = "darkred", lty = 4, type = "p", pch = "+", cex = 2)
points(est$combined, col = "blue", lty = 4, type = "p", pch = "*", cex = 2)

abline(0,0)
axis(1, at=1:num, labels = est$dom,las=2)
axis(2, at=seq(0,maxi+10,5), labels=seq(0,maxi+10,5))
legend("topleft",
       legend = c("Design based","Model based","Combined"), 
       pch = c("°","+","*"),
       col = c("darkgreen","darkred","blue"),
       ncol = 1, cex = 0.9, text.font = 1.5)
title("Web ordering by ICT (no/yes)",cex.main=1)

# ---------------- ATECO2 ------------------------------------
est <- estims[estims$type == "ATECO2" | estims$type == "Total",]
est <- est[order(est$type,est$dom),]
num <- length(droplevels(est$dom))
maxi = (max(c(est$combined),(est$design_upper),(est$model_based)))
# maxi = (max(c(est$design_upper,est$model_based)))

plot(est$design_upper, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 3,cex.lab = 1.3,
     xlab = "", 
     ylab = "Web ordering % ",
     # ylim = c(0,100), axes = F)
     ylim = c(0,maxi), axes = F)
points(est$design, col = "darkgreen", lty = 4, type = "p", pch = "°", cex = 2)
points(est$design_lower, col = "darkgreen", lty = 4, type = "l", pch = "°", cex = 2)
points(est$model_based, col = "darkred", lty = 4, type = "p", pch = "+", cex = 2)
points(est$combined, col = "blue", lty = 4, type = "p", pch = "*", cex = 2)

abline(0,0)
axis(1, at=1:num, labels = est$dom,las=2)
axis(2, at=seq(0,maxi+10,5), labels=seq(0,maxi+10,5))
legend("topleft",
       legend = c("Design based","Model based","Combined"), 
       pch = c("°","+","*"),
       col = c("darkgreen","darkred","blue"),
       ncol = 1, cex = 0.9, text.font = 1.5)
title("Web ordering by ATECO2 (no/yes)",cex.main=1)


dev.off()

est <- estims[,c("dom","type","sample","population","design","design_lower","design_upper","model_based","combined")]
write.table(est,file_out4,sep=";",row.names=F,
            col.names=T,quote=F)



