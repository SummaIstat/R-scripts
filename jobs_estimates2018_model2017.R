#===================================================
# Computation of estimates on e-commerce
#===================================================

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
setwd("G:/00_ICT_2018/job_2018_2017")
file_inp1 = "RDFbalanced.txt"
file_inp2 = "prob_RDF.txt"
file_inp3 = "surveyICT2018_ateco2.csv"
file_inp4 = "asia2018.csv"
file_out1 = "ICT_jobs_estimates2018_model2017_based.csv"

library(ReGenesees)

# ============================================================
# Read population
# ============================================================
#
asia <- read.csv(file_inp4)
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
survey <- read.csv(file_inp3,dec=".",stringsAsFactors = FALSE)
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
# survey$JOBS <- survey$e_JOBS

survey$WEB <- ifelse(is.na(survey$WEB),0,survey$WEB)
table(survey$WEB,useNA="ifany")

# survey$JOBS <- ifelse(is.na(survey$JOBS),0,survey$JOBS)
# table(survey$JOBS,useNA="ifany")
survey <- survey[,c("codice_unita","coef_cal","WEB",
                    "coef","strato",
                    "dom3","naceICT","naceist","mac4","clad4","reg21",
                    "ATECO2","TN",
                    "addetti","imprese")]
# write.table(survey,"survey_reduced.csv",sep=",",row.names=F,col.names=T,quote=F)


# ============================================================
# Predicted units
# These are the units in the population with websites
# whose retrieval and scraping was successful
# ============================================================
test_pred <- read.delim(file_inp1,header=FALSE,sep=" ")
test_prob <- read.delim(file_inp2,sep=" ",header=FALSE)
test <- cbind(test_pred,test_prob)
test <- test[,c(1,2,4)]
colnames(test) <- c("codice_unita","pred","pred_prob")

# they must be present in the frame
pred <- merge(test,asia) 
pred$WEB <- 1
pred$JOBS <- pred$pred

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
              # website_naceist[2:27],
              website_naceICT[2],
              website_clad4[2:4],
              website_dom3[2:16],
              website_reg21[2:21])

# ============================================================
# Full model based estimates 
# (synthetic estimates)
# ============================================================

preds <- pred
preds$JOBS <- preds$pred_prob

preds$stratum <- as.factor(paste(preds$naceist,preds$clad4,preds$reg21,sep=""))
preds$wgt <- nrow(asia) / nrow(preds)

# -------------------------------
# sink("new.txt")
# sum(preds$JOBS)
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
                       calfun = "linear", bounds = c(0.01, 10.00), aggregate.stage = NULL, 
                       # sigma2 = ~ADDETTI,
                       maxit = 50, epsilon = 1e-07, force = TRUE)

summary(weights(camp_cal))
g.range(camp_cal)
check.cal(camp_cal)

#------------------------------------------------------------------------------------
# Estimates of web ordering

JOBSTotal <- svystatTM(camp_cal, y= ~ JOBS ,  estimator= "Total", 
                         conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(JOBSTotal)

JOBSMean <- svystatTM(camp_cal, y= ~ JOBS ,  estimator= "Mean", 
                        conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(JOBSMean)

JOBS_ATECO2 <- svystatTM(camp_cal, y= ~ JOBS ,  estimator= "Total", 
                           by =~ATECO2 - 1,
                           conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(JOBS_ATECO2)

JOBS_naceist <- svystatTM(camp_cal, y= ~ JOBS ,  estimator= "Total", 
                            by =~naceist - 1,
                            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(JOBS_naceist)


JOBS_naceICT <- svystatTM(camp_cal, y= ~ JOBS ,  estimator= "Total", 
                            by =~naceICT - 1,
                            conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(JOBS_naceICT)


JOBS_clad4 <- svystatTM(camp_cal, y= ~ JOBS ,  estimator= "Total", 
                          by =~clad4 - 1,
                          conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(JOBS_clad4)


JOBS_dom3 <- svystatTM(camp_cal, y= ~ JOBS ,  estimator= "Total", 
                         by =~dom3 - 1,
                         conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(JOBS_dom3)


JOBS_reg21 <- svystatTM(camp_cal, y= ~ JOBS ,  estimator= "Total", 
                          by =~reg21 - 1,
                          conf.int= FALSE, conf.lev= 0.95, deff= FALSE, na.rm= FALSE)
Zapsmall(JOBS_reg21)

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
out$JOBS <- c(round(JOBSTotal$Total),
                round(JOBS_naceist$Total.JOBS),
                round(JOBS_naceICT$Total.JOBS),
                round(JOBS_clad4$Total.JOBS),
                round(JOBS_dom3$Total.JOBS),
                round(JOBS_reg21$Total.JOBS),
                round(JOBS_ATECO2$Total.JOBS))
out <- as.data.frame((out))
#out$websites_100 <- round(out$websites * 100 / out$population,2)
out$JOBS_100 <- round(out$JOBS * 100 / out$population,2)


write.table(out,file_out1,sep=";",row.names=F,col.names=TRUE,quote=FALSE)




