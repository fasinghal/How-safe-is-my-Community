#read the data and replace all the missing value indicators ("?") to NA
communities <- read.csv("D:/SEM3 MS/R for DS P2/communities.data", header=FALSE, na.strings="?")

names(communities)<-c("state","county","community","communityname","fold","population","householdsize",
                      "racepctblack","racePctWhite","racePctAsian","racePctHisp","agePct12t21",
                      "agePct12t29","agePct16t24","agePct65up","numbUrban","pctUrban","medIncome",
                      "pctWWage","pctWFarmSelf","pctWInvInc","pctWSocSec","pctWPubAsst","pctWRetire",
                      "medFamInc","perCapInc","whitePerCap","blackPerCap","indianPerCap","AsianPerCap",
                      "OtherPerCap","HispPerCap","NumUnderPov","PctPopUnderPov","PctLess9thGrade",
                      "PctNotHSGrad","PctBSorMore","PctUnemployed","PctEmploy","PctEmplManu",
                      "PctEmplProfServ","PctOccupManu","PctOccupMgmtProf","MalePctDivorce",
                      "MalePctNevMarr","FemalePctDiv","TotalPctDiv","PersPerFam","PctFam2Par",
                      "PctKids2Par","PctYoungKids2Par","PctTeen2Par","PctWorkMomYoungKids",
                      "PctWorkMom","NumIlleg","PctIlleg","NumImmig","PctImmigRecent","PctImmigRec5",
                      "PctImmigRec8","PctImmigRec10","PctRecentImmig","PctRecImmig5","PctRecImmig8",
                      "PctRecImmig10","PctSpeakEnglOnly","PctNotSpeakEnglWell","PctLargHouseFam",
                      "PctLargHouseOccup","PersPerOccupHous","PersPerOwnOccHous","PersPerRentOccHous",
                      "PctPersOwnOccup","PctPersDenseHous","PctHousLess3BR","MedNumBR","HousVacant",
                      "PctHousOccup","PctHousOwnOcc","PctVacantBoarded","PctVacMore6Mos","MedYrHousBuilt",
                      "PctHousNoPhone","PctWOFullPlumb","OwnOccLowQuart","OwnOccMedVal","OwnOccHiQuart",
                      "RentLowQ","RentMedian","RentHighQ","MedRent","MedRentPctHousInc","MedOwnCostPctInc",
                      "MedOwnCostPctIncNoMtg","NumInShelters","NumStreet","PctForeignBorn","PctBornSameState",
                      "PctSameHouse85","PctSameCity85","PctSameState85","LemasSwornFT","LemasSwFTPerPop",
                      "LemasSwFTFieldOps","LemasSwFTFieldPerPop","LemasTotalReq","LemasTotReqPerPop",
                      "PolicReqPerOffic","PolicPerPop","RacialMatchCommPol","PctPolicWhite","PctPolicBlack",
                      "PctPolicHisp","PctPolicAsian","PctPolicMinor","OfficAssgnDrugUnits","NumKindsDrugsSeiz",
                      "PolicAveOTWorked","LandArea","PopDens","PctUsePubTrans","PolicCars","PolicOperBudg",
                      "LemasPctPolicOnPatr","LemasGangUnitDeploy","LemasPctOfficDrugUn","PolicBudgPerPop","ViolentCrimesPerPop")
myData<- communities
summary(myData)

dim(myData)
# 1994 rows and 128 Attributes

#cleaning the data
#As per the dataset description, first five attributes are not counted as predictive, lets remove those

myData$state<-NULL
myData$county<-NULL
myData$community<-NULL
myData$communityname<-NULL
myData$fold<-NULL

#Let's check the NA count per attribute
NACounts <- apply(X = myData, MARGIN = 2, FUN = function(x) sum(is.na(x)))
summary(myData$OtherPerCap) # this has one NA. Lets keep it and omit 1 record containing NA

NACounts[NACounts>1]
largeNaAttr<-names(NACounts[NACounts>1])

dim(myData)  # 1994 x 123
length(largeNaAttr) # 22 attributes with large NAs

#lets remove these fields
for(i in 1: length(largeNaAttr)){
  myData[,largeNaAttr[i]] <-NULL
}

dim(myData) # 1994 x 101

#remove single record of OtherPerCap containing NA
myData<-na.omit(myData) 
dim(myData) # 1993 x 101 #

summary(myData)
NACounts <- apply(X = myData, MARGIN = 2, FUN = function(x) sum(is.na(x)))
NACounts[NACounts>0]  # all values are normalized already, are numeric and dont have NAs

#lets build initial Linear regression model

#0.75 * 1993 
index_train<-sample(1993, size = 1495) # ~75% split
trainData<- myData[index_train, ]
testData<- myData[-index_train,]


all.model <- lm(data = trainData, formula = ViolentCrimesPerPop~.)
glm.model1 <- glm(data = trainData, formula = ViolentCrimesPerPop~.)

summary(all.model)
anov<-anova(all.model)

#many predictors seem useless 
#(a larger (insignificant) F test-value suggests that changes in the predictor are not associated 
#with changes in the response)
#lets remove those


#lets see the distribution of F-Stat values to get an idea of cutoff F stat
summary(anov$`Pr(>F)`)
boxplot(anov$`Pr(>F)`)

hist(anov$`Pr(>F)`, freq = FALSE)
lines(density(na.omit(anov$`Pr(>F)`)), col="red") # BiModal ? hm

#lets keep the cutoff at third quartile Q3 = 0.4707

insig <-anov[anov$`Pr(>F)` >0.4707,]

#there's one weired NA, lets remove that and order in descresing F-Stat
insig <-na.omit(insig)
insig<-insig[order(-insig$`Pr(>F)`),]

insigAttr<-rownames(insig)
paste(insigAttr, collapse = " - ")

formula <- ViolentCrimesPerPop~. -OwnOccHiQuart - whitePerCap - PctNotSpeakEnglWell - PctHousOccup - PctImmigRecent - LandArea - NumImmig - PctSameHouse85 - PctImmigRec5 - agePct16t24 - PctBSorMore - RentHighQ - PctTeen2Par - perCapInc - MedYrHousBuilt - PctLargHouseOccup - PctUnemployed - PctLargHouseFam - PctImmigRec8 - agePct12t21 - MedRentPctHousInc - NumUnderPov - PctHousNoPhone - PctSpeakEnglOnly - PctSameState85

glm.model2<-glm(formula = formula, data = trainData) 
summary(glm.model2) # better than glm.model1 AIC -1690

#lets try Step AIC on improved model - glm.model2
library("MASS", quietly = TRUE)
AIC1<- stepAIC(object = glm.model2, direction = "both")

AICFormula<-ViolentCrimesPerPop ~ racepctblack + agePct12t29 + pctUrban + 
  medIncome + pctWWage + pctWFarmSelf + pctWInvInc + pctWRetire + 
  indianPerCap + AsianPerCap + OtherPerCap + PctPopUnderPov + 
  PctLess9thGrade + PctEmploy + PctEmplManu + PctOccupManu + 
  PctOccupMgmtProf + MalePctDivorce + MalePctNevMarr + TotalPctDiv + 
  PersPerFam + PctKids2Par + PctYoungKids2Par + PctWorkMom + 
  NumIlleg + PctIlleg + PersPerOccupHous + PersPerRentOccHous + 
  PctPersOwnOccup + PctPersDenseHous + HousVacant + PctHousOwnOcc + 
  PctVacantBoarded + PctVacMore6Mos + RentLowQ + MedRent + 
  MedOwnCostPctInc + MedOwnCostPctIncNoMtg + NumStreet + PctUsePubTrans

aic.glm.model3<-glm(formula = AICFormula, data=trainData)
summary(aic.glm.model3) # huge improvement in AIC -> -1756.5

#Lets RUN Step AIC on ALL Data for model discovery
ALL.AIC<-stepAIC(object = all.model, direction = "both")

aic.all.formula<-ViolentCrimesPerPop ~ population + racepctblack + agePct12t21 + 
  agePct16t24 + numbUrban + pctUrban + pctWWage + pctWFarmSelf + 
  pctWInvInc + pctWSocSec + whitePerCap + indianPerCap + AsianPerCap + 
  PctPopUnderPov + PctEmploy + PctEmplManu + PctOccupManu + 
  PctOccupMgmtProf + MalePctDivorce + MalePctNevMarr + TotalPctDiv + 
  PctKids2Par + PctWorkMomYoungKids + PctWorkMom + NumIlleg + 
  PctIlleg + NumImmig + PctNotSpeakEnglWell + PctLargHouseOccup + 
  PersPerOccupHous + PersPerRentOccHous + PctPersOwnOccup + 
  PctPersDenseHous + HousVacant + PctHousOccup + PctHousOwnOcc + 
  PctVacantBoarded + PctVacMore6Mos + RentLowQ + RentHighQ + 
  MedRent + MedRentPctHousInc + MedOwnCostPctInc + MedOwnCostPctIncNoMtg + 
  NumInShelters + NumStreet + PctForeignBorn + PctUsePubTrans

aic.all.model<-glm(formula = aic.all.formula, data = trainData)
summary(aic.all.model) # much better AIC = -1764.8

#Lets try BIC approach

ALL.BIC <- stepAIC(object = all.model, direction = "both", k = log(length(trainData$ViolentCrimesPerPop)))
bic.formula<-ViolentCrimesPerPop ~ racepctblack + pctUrban + pctWSocSec + 
  PctEmploy + MalePctDivorce + PctKids2Par + PctWorkMom + PctIlleg + 
  PctNotSpeakEnglWell + PctPersDenseHous + HousVacant + PctHousOccup + 
  RentLowQ + MedRent + MedOwnCostPctIncNoMtg + NumStreet + 
  PctForeignBorn

bic.all.model<-glm(formula = bic.formula, data=trainData)
summary(bic.all.model) 

#we have three contender models
#aic.glm.model3
#aic.all.model
#bic.all.model

#lets do a cross validation on total data
library(boot, quietly = TRUE)

#generate the GLM object on whole data for cv.glm to work
aic.glm.model3<-lm(formula = AICFormula, data=myData)
cv.aic.glm3 <-cv.glm(data = myData,glmfit = aic.glm.model3, K = 10)
#prediction error = 0.01782328
#0.01782328/mean(myData$ViolentCrimesPerPop)

aic.all.model <- lm(formula = aic.all.formula, data = myData)
cv.aic.all<-cv.glm(data = myData,glmfit = aic.all.model, K = 10)
aic.all.prd<- predict(aic.all.model, newdata = testData)
#worse off
#sqrt(mean(aic.all.model$residuals^2))/mean(myData$ViolentCrimesPerPop) ~ 54% :X

bic.all.model<-glm(formula = bic.formula, data = myData)
bic.glm<-cv.glm(data = myData,glmfit = bic.all.model, K = 10)
#worse off performance to AIC ALL model
#0.01823776/mean(myData$ViolentCrimesPerPop)

#gotta
##PCR KNN Lasso Ridge 

#lets train a NN first
library(neuralnet, quietly = TRUE)
nn1<-neuralnet(aic.all.formula, data = trainData,hidden = c(66, 44), linear.output = TRUE) # removing the y col
saveRDS(nn1, "./nn1.rds")
nn1 <- readRDS("./nn1.rds")

testData.nn<-testData[,c("population","racepctblack","agePct12t21",
"agePct16t24","numbUrban","pctUrban","pctWWage","pctWFarmSelf",
"pctWInvInc","pctWSocSec","whitePerCap","indianPerCap","AsianPerCap",
"PctPopUnderPov","PctEmploy","PctEmplManu","PctOccupManu",
"PctOccupMgmtProf","MalePctDivorce","MalePctNevMarr","TotalPctDiv",
"PctKids2Par","PctWorkMomYoungKids","PctWorkMom","NumIlleg",
"PctIlleg","NumImmig","PctNotSpeakEnglWell","PctLargHouseOccup",
"PersPerOccupHous","PersPerRentOccHous","PctPersOwnOccup",
"PctPersDenseHous","HousVacant","PctHousOccup","PctHousOwnOcc",
"PctVacantBoarded","PctVacMore6Mos","RentLowQ","RentHighQ",
"MedRent","MedRentPctHousInc","MedOwnCostPctInc","MedOwnCostPctIncNoMtg",
"NumInShelters","NumStreet","PctForeignBorn","PctUsePubTrans")]

nn1.pr<-compute(x = nn1, covariate = testData.nn)
MSE.nn1 <-sum((testData[,101]-nn1.pr$net.result)^2)/nrow(testData.nn) #
MSE.nn1*100 # ~  6.933821579%


##2nd Model
nn2<-neuralnet(aic.all.formula, data = trainData,hidden = c(66, 44, 29), linear.output = TRUE) # removing the y col
saveRDS(nn2, "./nn2.rds")
nn2 <- readRDS("./nn2.rds")


nn2.pr<-compute(x = nn2, covariate = testData.nn)
MSE.nn2 <-sum((testData[,101]-nn2.pr$net.result)^2)/nrow(testData.nn) #
MSE.nn2*100 # ~5.637280655%

# lets continue the 2/3 story

nn3<-neuralnet(aic.all.formula, data = trainData,hidden = c(66, 44, 29, 19), linear.output = TRUE) # removing the y col
saveRDS(nn3, "./nn3.rds")
nn3 <- readRDS("./nn3.rds")

saveRDS(nnprop_with, "./nnprop_with.rds")

nn3.pr<-compute(x = nn3, covariate = testData.nn)
MSE.nn3<-sum((testData[,101]-nn3.pr$net.result)^2)/nrow(testData.nn) #
MSE.nn3*100 #~ 5.378675508 % 


#lets change the algorithm to resilient backpropagation with weight backtracking
# advantages -
# First, training with Rprop is often faster than training with back propagation. 
# Second, Rprop is one of the fastest weight update mechanisms

nn.rprop_with<-neuralnet(aic.all.formula, data = trainData,hidden = c(66, 44, 29, 19), linear.output = TRUE, algorithm = "rprop+" ) # removing the y col
saveRDS(nn.rprop_with, "./nn.rprop_with.rds")
nn.rprop_with <- readRDS("./nn.rprop_with.rds")

nn.rprop_with.pr<-compute(x = nn.rprop_with, covariate = testData.nn)
MSE.rprop_with<-sum((testData[,101]-nn.rprop_with.pr$net.result)^2)/nrow(testData.nn) #
MSE.rprop_with*100 # ~4.45 yay!

#without weight backprp

nn.rprop_without<-neuralnet(aic.all.formula, data = trainData,hidden = c(66, 44, 29, 19), linear.output = TRUE, algorithm = "rprop-" ) # removing the y col
saveRDS(nn.rprop_without, "./nn.rprop_without.rds")
nn.rprop_without<- readRDS("./nn.rprop_without.rds")

nn.rprop_without.pr<-compute(x = nn.rprop_without, covariate = testData.nn)
MSE.rprop_without<-sum((testData[,101]-nn.rprop_without.pr$net.result)^2)/nrow(testData.nn) #
MSE.rprop_without*100 #~ 4.060703571 best so far

# lets try sag algorithm

nn.sag<-neuralnet(aic.all.formula, data = trainData,hidden = c(66, 44, 29, 19), linear.output = TRUE, algorithm = "sag" ) # removing the y col
saveRDS(nn.sag, "./nn.sag.rds")
nn.sag <- readRDS("./nn.sag.rds")

nn.sag.pr<-compute(x = nn.sag, covariate = testData.nn)
MSE.sag<-sum((testData[,101]-nn.sag.pr$net.result)^2)/nrow(testData.nn) #
MSE.sag*100 # 5.504 % meh!


#lets compare
#nnTst<-testData[,101]
#nnTpr<-nn.pr$net.result
#alldata.nn<-c(nnTst,nnTpr)
#range=c(0,1)

#plot(testData[,101],nn.pr$net.result, col="red",main='Real vs predicted NN',pch=18,cex=0.7, xlim = range, ylim=range)
#abline(0,1,lwd=2)
#plot(testData[,101], aic.all.prd, col="blue",main='Real vs predicted LR',pch=18,cex=0.7,xlim=range, ylim = range)
#abline(0,1,lwd=2)

#works best for split at seed(1) for rprop-, hidden  c(66, 44, 29, 19)
#lets automate()
#running for backprop, seed 1 : 25, Hidden c(66, 44)
pb <- progress_bar$new(total = 26)
pb$tick()
xval<-1:25
yval<-rep(0,25)
for(i in 1:25 ){
  
  set.seed(i)
  index_train<-sample(1993, size = 1495) # ~75% split
  trainData<- myData[index_train, ]
  testData<- myData[-index_train,]
  testData.nn<-testData[,c("population","racepctblack","agePct12t21",
                           "agePct16t24","numbUrban","pctUrban","pctWWage","pctWFarmSelf",
                           "pctWInvInc","pctWSocSec","whitePerCap","indianPerCap","AsianPerCap",
                           "PctPopUnderPov","PctEmploy","PctEmplManu","PctOccupManu",
                           "PctOccupMgmtProf","MalePctDivorce","MalePctNevMarr","TotalPctDiv",
                           "PctKids2Par","PctWorkMomYoungKids","PctWorkMom","NumIlleg",
                           "PctIlleg","NumImmig","PctNotSpeakEnglWell","PctLargHouseOccup",
                           "PersPerOccupHous","PersPerRentOccHous","PctPersOwnOccup",
                           "PctPersDenseHous","HousVacant","PctHousOccup","PctHousOwnOcc",
                           "PctVacantBoarded","PctVacMore6Mos","RentLowQ","RentHighQ",
                           "MedRent","MedRentPctHousInc","MedOwnCostPctInc","MedOwnCostPctIncNoMtg",
                           "NumInShelters","NumStreet","PctForeignBorn","PctUsePubTrans")]
  nn.rprop_without<-neuralnet(aic.all.formula, data = trainData,hidden = c(66, 44, 29, 19), linear.output = TRUE, algorithm = "rprop-") 
  nn.rprop_without.pr<-compute(x = nn.rprop_without, covariate = testData.nn)
  MSE.rprop_without<-sum((testData[,101]-nn.rprop_without.pr$net.result)^2)/nrow(testData.nn) #
  MSE.rprop_without<-MSE.rprop_without*100
  pb$tick()
  xval[i]<-i
  yval[i]<-MSE.rprop_without
  
}
#yval<-readRDS(file = "./yval1to20.rds")
plot(1:20,yval,type = "b", col="red")
