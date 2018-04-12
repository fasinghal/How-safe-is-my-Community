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
NACounts[NACounts>0]
largeNaAttr<-names(NACounts[NACounts>0])

dim(myData)  # 1994 x 123
length(largeNaAttr) # 23 attributes with large NAs

#lets remove these fields
for(i in 1: length(largeNaAttr)){
  myData[,largeNaAttr[i]] <-NULL
}

dim(myData) # 1994 x 100

summary(myData) # all values are normalized already, are numeric and dont have NAs

#lets build initial Linear regression model

#0.75 * 1994
index_train<-sample(1994, size = 1496) # 75% split
trainData<- myData[index_train, ]
testData<- myData[-index_train,]
lm.model <- lm(data = trainData, formula = ViolentCrimesPerPop~.)

summary(lm.model)
anov<-anova(lm.model)
insigAttr <-rownames(anov[anov$`Pr(>F)` >0.9,])

#many predictors seem useless
#lets remove those

formula <- ViolentCrimesPerPop~. -HispPerCap -PctOccupManu -OwnOccLowQuart -RentHighQ

lm.model2<-glm(formula = formula, data = trainData)
summary(lm.model2) # too many variables -ve AIC! :S
