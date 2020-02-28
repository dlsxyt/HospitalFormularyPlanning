setwd("/Users/xx/Desktop/rotman datathon/")
library(varhandle)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(data.table)
data = read.csv("/Users/xx/Desktop/rotman datathon/download.csv",header = T)
head(data)
dim(data)
plot(data$readmitted, xlab = "readmit", ylab="freq", title = "Frequency of readmitted")
na.omit(data)

# all the medicines that we have
medcines = colnames(data[,seq(15,36)])

# factor up down and steady to numeric
data[,seq(15,36)] = lapply(data[,seq(15,36)], factor, levels =c("No","Down","Up", "Steady"))
data[,seq(15,36)] = lapply(data[,seq(15,36)], as.numeric)
data[,c(3,4,5)] = lapply(data[,c(3,4,5)], factor)

# count number of 1, if = 22, use no medicine.
data$used = rowSums(data[,seq(15,36)])
data$used = rowSums(data[,seq(15,36)])
# filter out data with no extra medicine at all
data = data %>% filter(data$used != 22)
data = data[, seq(1,37)]


# number of medicine used
med = data[,seq(15,36)]
NumMed <- rowSums(med!= 1)
data = cbind(data, NumMed)

# remove those with only Insulin or Metformin
data[,"metformin"] = ifelse(data[,"metformin"] == 1, 0,1)
data[,"insulin"] = ifelse(data[,"insulin"] == 1, 0,1)
data = data %>% filter(
(data$NumMed - data[,"insulin"] - data[,"metformin"] != 0) & (data$NumMed - data[,"insulin"] != 0) &
  (data$NumMed - data[,"metformin"] != 0)
  )  

data = data[, !(colnames(data) %in% c("metformin", "insulin"))]

data$readmitted = factor(data$readmitted, labels = c('<30','NO','>30'))
data$number_diagnoses = factor(data$number_diagnoses)
data[,seq(15,34)] = lapply(data[,seq(15,34)], factor, levels=c(1,2,3,4))
#########clean data ends##############

###############Prepare dummy variable#################
for (i in 15:34){
  contrasts(data[,i])
}
###########Fitting the Ologit model###############
m = polr(formula = data$readmitted ~ ., data = data[,-c(1,2,35,36)], Hess = TRUE)

summary(update(m, method = "probit", Hess = TRUE), digits = 3)


ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

ciL <- ctable[,1] - 2* ctable[,3]
ciU <- ctable[,1] + 2* ctable[,3]

result = exp(cbind(OR = ctable[,1], "2.5%" = ciL, "97.5%"=ciU))
########### model cof ends#######

####### some medicine is having too few obs so exclude from recommendations########
percet_used = function(x){
(dim(data)[1] - sum(data[,x] == 1))/dim(data)[1] # number of patient used tolazamide
}

small_num_used = rbind("tolazamide" = percet_used("tolazamide"),
      "metformin.rosiglitazone" = percet_used("metformin.rosiglitazone"),
      "metformin.pioglitazone" = percet_used("metformin.pioglitazone"),
      "acetohexamide" = percet_used("acetohexamide"),
      "chlorpropamide" = percet_used("chlorpropamide"),
      "glyburide.metformin" = percet_used("glyburide.metformin")
      )

########
sum(data[,"nateglinide"] == 1)

data$nateglinide
###### oder by cof######
result[result[,1]>1,]

data$nateglinide

write.csv(data, "2019 MMA Datathon Data Structure1.csv")
write.csv(small_num_used, "small_num_used.csv")

