
#-------------------------------------------------------------------------------------------------------------
#Loading Packages
if(!require('pacman'))install.packages('pacman')
pacman::p_load(dplyr, tidyverse, corrplot, ggplot2, mice, VIM, Boruta, Amelia)

search()
theme_set(theme_classic())

#-------------------------------------------------------------------------------------------------------------
#Loading Datasets
application <- read.csv("application.csv", header=TRUE, sep=",")
bureau <- read.csv("bureau.csv", header=TRUE, sep=",")
bureau_balance <- read.csv("bureau_balance.csv", header=TRUE, sep=",")
credit_card_balance <- read.csv("credit_card_balance.csv", header=TRUE, sep=",")
installments_payments <- read.csv("installments_payments.csv", header=TRUE, sep=",")
pos_cash_balance <- read.csv("POS_CASH_balance.csv", header=TRUE, sep=",")
previous_application <- read.csv("previous_application.csv", header=TRUE, sep=",")

final_feature <- read.csv("final_feature.csv", header=TRUE, sep=",")

#-------------------------------------------------------------------------------------------------------------
#Checking Structure of data
str(application)
str(bureau)
str(bureau_balance)
str(credit_card_balance)
str(installments_payments)
str(pos_cash_balance)
str(previous_application)

#-------------------------------------------------------------------------------------------------------------
#Checking for null values
sum(is.na(application))
#HIgh number of missing values for main application dataset

application %>%  summarise_each(funs(100*mean(is.na(.))))
bureau %>%  summarise_each(funs(100*mean(is.na(.))))
bureau_balance %>%  summarise_each(funs(100*mean(is.na(.))))
credit_card_balance %>%  summarise_each(funs(100*mean(is.na(.))))
installments_payments %>%  summarise_each(funs(100*mean(is.na(.))))
pos_cash_balance %>%  summarise_each(funs(100*mean(is.na(.))))
previous_application %>%  summarise_each(funs(100*mean(is.na(.))))

previous_application %>%  summarise_each(funs(100*mean(is.na(.))))

#-------------------------------------------------------------------------------------------------------------
#Feature Engineering

#Removing unwanted featrues such as Average and Mode as I have used  Median
names(application)[1]<-"SK_ID_CURR"
application <- application[!(application$CODE_GENDER == "XNA"),]                           
application <- select(application,-contains("AVG"))
application <- select(application,-contains("MODE"))
application %>%  summarise_each(funs(100*mean(is.na(.))))

# Creating data frame of columns with missing values in Appliation dataset 
application_md <- application[,c(10,11,22,30,42:63,84:89)]

application_md1 <- application_md[,c(1:4)]
application_md2 <- application_md[,c(5:8)]
application_md3 <- application_md[,c(9:12)]
application_md4 <- application_md[,c(13:16)]
application_md5 <- application_md[,c(17:20)]
application_md6 <- application_md[,c(21:24)]
application_md7 <- application_md[,c(25:28)]
application_md8 <- application_md[,c(29:32)]

# Checking for the missing values pattern 

md.pattern(application_md1)
application_md1_miss = aggr(application_md1, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                            labels=names(application_md1), prop = T, combined = F,
                            cex.axis=.45, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

marginplot(application_md1[c(3,1)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)

md.pattern(application_md2)
application_md2_miss = aggr(application_md2, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                            labels=names(application_md2), prop = T, combined = F,
                            cex.axis=.45, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

marginplot(application_md2[c(1,4)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)
marginplot(application_md2[c(3,4)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)

md.pattern(application_md3)
application_md1_miss = aggr(application_md3, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                            labels=names(application_md1), prop = T, combined = F,
                            cex.axis=.45, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

marginplot(application_md3[c(1,2)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)
marginplot(application_md3[c(3,4)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)

md.pattern(application_md4)
application_md1_miss = aggr(application_md4, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                            labels=names(application_md4), prop = T, combined = F,
                            cex.axis=.45, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

marginplot(application_md4[c(1,2)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)
marginplot(application_md4[c(3,4)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)

md.pattern(application_md5)
application_md1_miss = aggr(application_md5, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                            labels=names(application_md5), prop = T, combined = F,
                            cex.axis=.34, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

marginplot(application_md5[c(1,2)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)
marginplot(application_md5[c(3,4)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)

md.pattern(application_md6)
application_md1_miss = aggr(application_md6, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                            labels=names(application_md6), prop = T, combined = F,
                            cex.axis=.3, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

marginplot(application_md6[c(1,2)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)
marginplot(application_md6[c(3,4)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)

md.pattern(application_md7)
application_md1_miss = aggr(application_md7, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                            labels=names(application_md7), prop = T, combined = F,
                            cex.axis=.28, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

marginplot(application_md7[c(3,4)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)
marginplot(application_md7[c(1,2)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)

md.pattern(application_md8)
application_md1_miss = aggr(application_md8, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                            labels=names(application_md8), prop = T, combined = F,
                            cex.axis=.28, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

marginplot(application_md8[c(1,2)], col = mdc(1:2), cex.numbers = 0.5, pch = 19)


#Imputing the missing values through the MICE imputation technique
imp1 <- mice(application_md1, seed = 23109)
imp2 <- mice(application_md2, seed = 23109)
imp3 <- mice(application_md3, seed = 23109)
imp4 <- mice(application_md4, seed = 23109)
imp5 <- mice(application_md5, seed = 23109)
imp6 <- mice(application_md6, seed = 23109)
imp7 <- mice(application_md7, seed = 23109)
imp8 <- mice(application_md8, seed = 23109)

completeData <- complete(imp1)

application$AMT_ANNUITY <- completeData$AMT_ANNUITY
application$AMT_GOODS_PRICE <- completeData$AMT_GOODS_PRICE
application$OWN_CAR_AGE <- completeData$OWN_CAR_AGE
application$CNT_FAM_MEMBERS <- completeData$CNT_FAM_MEMBERS

completeData1 <- complete(imp2)
application$EXT_SOURCE_1 <- completeData1$EXT_SOURCE_1
application$EXT_SOURCE_2 <- completeData1$EXT_SOURCE_2
application$EXT_SOURCE_3 <- completeData$EXT_SOURCE_3
application$APARTMENTS_MEDI <- completeData$APARTMENTS_MEDI

completeData4 <- complete(imp6)
application$NONLIVINGAREA_MEDI <- completeData4$NONLIVINGAREA_MEDI
application$OBS_30_CNT_SOCIAL_CIRCLE <- completeData4$OBS_30_CNT_SOCIAL_CIRCLE
application$DEF_30_CNT_SOCIAL_CIRCLE <- completeData4$DEF_30_CNT_SOCIAL_CIRCLE
application$OBS_60_CNT_SOCIAL_CIRCLE <- completeData4$OBS_60_CNT_SOCIAL_CIRCLE

#Writing final application dataset with no missing values after imputing to CSV

write.csv(application,'application.csv', row.names = FALSE)

#Importing final feature rfom Python file (Feature Engineering). 

summ <- final_feature %>%  summarise_each(funs(100*mean(is.na(.))))
write.csv(summ,'sumna.csv', row.names = FALSE)

final_feature$LANDAREA_MEDI <- application$LANDAREA_MEDI
final_feature$LIVINGAPARTMENTS_MEDI <- application$LIVINGAPARTMENTS_MEDI
final_feature$LIVINGAREA_MEDI <- application$LIVINGAREA_MEDI
final_feature$NONLIVINGAPARTMENTS_MEDI <- application$NONLIVINGAPARTMENTS_MEDI

final_feature$NONLIVINGAREA_MEDI <- application$NONLIVINGAREA_MEDI

final_feature$OBS_30_CNT_SOCIAL_CIRCLE <- application$OBS_30_CNT_SOCIAL_CIRCLE
final_feature$DEF_30_CNT_SOCIAL_CIRCLE <- application$DEF_30_CNT_SOCIAL_CIRCLE
final_feature$OBS_60_CNT_SOCIAL_CIRCLE <- application$OBS_60_CNT_SOCIAL_CIRCLE

final_feature$AMT_REQ_CREDIT_BUREAU_HOUR <- application$AMT_REQ_CREDIT_BUREAU_HOUR


#Checking for missing values in EXT_SOURCE_3 
EXT_SOURCE_3 <- application_md$EXT_SOURCE_3

final_feature <- cbind(final_feature, EXT_SOURCE_3)

#Imputing missing values with mean
final_feature = transform(final_feature, EXT_SOURCE_3 = ifelse(is.na(EXT_SOURCE_3), mean(EXT_SOURCE_3, na.rm=TRUE), EXT_SOURCE_3))

write.csv(summ,'sumna.csv', row.names = FALSE)

#Removing Unecessary IDs
final_feature <- select(final_feature,-contains("_id"))

sumna <- read.csv("sumna.csv", header=TRUE, sep=",")

#Adding values with high NA to separate dataset and removing from overall feature engineered dataset 
high_na <- sumna[sumna$NA. > 65,]

cols_to_drop <- intersect(colnames(final_feature), high_na$X)

#dropping high NA columns from overall final feature dataset
final_feature <- final_feature[ , -which(names(final_feature) %in% c(cols_to_drop))]

summ1 <- final_feature %>%  summarise_each(funs(100*mean(is.na(.))))

write.csv(summ1,'sumna1.csv', row.names = FALSE)

drops <- c("MIN.bureau.AMT_CREDIT_MAX_OVERDUE.","MAX.bureau.AMT_CREDIT_MAX_OVERDUE.", "MEAN.bureau.AMT_CREDIT_MAX_OVERDUE.")
final_feature <- final_feature[ , !(colnames(final_feature) %in% drops)]

final_feature <- na.omit(final_feature)

set.seed(123)
sample_feature <- final_feature[sample(nrow(final_feature),10000),]

view(sample_feature)

#Writing sample feature and final feature to CSV for Running Machine Learning files in R markdown

write.csv(sample_feature,'sample_feature.csv', row.names = FALSE)

write.csv(final_feature,'final_feature.csv', row.names = FALSE)
