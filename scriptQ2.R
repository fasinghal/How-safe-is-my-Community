#Question 2
#Submitted by 
#Falak Singhal (fxs161530@utdallas.edu)
#Shruti Govind bidada (sgb160130@utdallas.edu)

myData <-read.csv("D:/SEM3 MS/R for DS P2/agaricus-lepiota.data", header=FALSE,na.strings="?")
colnames(myData)<-c("class","shape","capsurface","capcolor","bruises","odor","gillattachment","gillspacing","gillsize","gillcolor","stalkshape","stalkroot","stalksurfaceabovering","stalksurfacebelowring","stalkcolorabovering","stalkcolorbelowring","veiltype","veilcolor","ringnumber","ringtype","sporeprintcolor","population","habitat")
summary(myData)

#convert everything to a factor
for(i in 1:23){
  myData[,i]<-as.factor(myData[,i])
}

#Checking the distribution of data within each attribute
for(i in 1:16){
  barplot(summary(myData[,i]), xlab = colnames(myData)[i])
}

#Check the NA counts in each attribute
NACounts <- apply(X = myData, MARGIN = 2, FUN = function(x) sum(is.na(x)))
NACounts

#Lets remove some useless fields
myData$stalkroot<-NULL
myData$veiltype<-NULL

#Convex Hull is a special type of Canonical Surface, we can merge shape = c and x into x
levels(myData$shape) <- c("b", "x", "f", "k", "s", "x")

#cap-surface = grooves=g,scaly=y surfaces can be classified under ridge surfaces as r
levels(myData$capsurface) <-c("f", "r", "s", "r")

#stalk-color-above-ring = yellow=y and Buff=b are the shades of same yellow color, can be merged as buff =b
levels(myData$stalkcolorabovering)=c("b" ,"c", "e" ,"g", "n", "o", "p" ,"w", "b")


# veil-color = yellow = y  can be discarded
myData<-myData[myData$veilcolor!='y',]
myData$veilcolor<-droplevels(myData$veilcolor)


summary(myData)

myDataClean<-as.data.frame(myData)

#create Model Matrix 
MushroomMM<-data.frame(model.matrix(~., myDataClean))

index_train<-sample(8124, size = 6093) # ~75% split
trainData.MM<- MushroomMM[index_train, -1] # removing the intercept col
testData.MM<- MushroomMM[-index_train,-1] 


library(neuralnet)

mm.nn.formula <-formula(classp ~ shapex + shapef +shapek + shapes + capsurfacer + 
                        capsurfaces + capcolorc + capcolore + 
                        capcolorg + capcolorn + capcolorp + 
                        capcolorr + capcoloru + capcolorw + 
                        capcolory + bruisest + odorc + 
                        odorf + odorl + odorm + 
                        odorn + odorp + odors + 
                        odory + gillattachmentf + gillspacingw + 
                        gillsizen + gillcolore + gillcolorg + 
                        gillcolorh + gillcolork + gillcolorn + 
                        gillcoloro + gillcolorp + gillcolorr + 
                        gillcoloru + gillcolorw + gillcolory + 
                        stalkshapet + stalksurfaceaboveringk + stalksurfaceaboverings + 
                        stalksurfaceaboveringy + stalksurfacebelowringk + stalksurfacebelowrings + 
                        stalksurfacebelowringy + stalkcoloraboveringc + stalkcoloraboveringe + 
                        stalkcoloraboveringg + stalkcoloraboveringn + stalkcoloraboveringo + 
                        stalkcoloraboveringp + stalkcoloraboveringw + stalkcolorbelowringc + 
                        stalkcolorbelowringe + stalkcolorbelowringg + stalkcolorbelowringn + 
                        stalkcolorbelowringo + stalkcolorbelowringp + stalkcolorbelowringw + 
                        stalkcolorbelowringy + veilcoloro + veilcolorw + 
                        ringnumbero + ringnumbert + ringtypef + 
                        ringtypel + ringtypen + ringtypep + 
                        sporeprintcolorh + sporeprintcolork + sporeprintcolorn + 
                        sporeprintcoloro + sporeprintcolorr + sporeprintcoloru + 
                        sporeprintcolorw + sporeprintcolory + populationc + 
                        populationn + populations + populationv + 
                        populationy + habitatg + habitatl + 
                        habitatm + habitatp + habitatu + 
                        habitatw)

dim(trainData.MM)
Mushroom.NN1<-neuralnet(formula =mm.nn.formula, data = trainData.MM,hidden = c(58),linear.output = FALSE )

#save the model
saveRDS(Mushroom.NN1, "./MushroomNN1.rds")
Mushroom.NN1<-readRDS("./MushroomNN1.rds")

nn.pr <-compute(x = Mushroom.NN1,covariate = testData.MM[,-1]) # remove the dependent var (classp = indicator of poisonous)

pr.result <- nn.pr$net.result
pr.result <-ifelse(pr.result>0.5, yes = "poisonous",no = "edible") # convert to poisonous = 1, edible = 0
actual.val <- ifelse(testData.MM$classp==1,yes = "poisonous",no = "edible")

result<-table(actual = actual.val, predicted = pr.result)
 
#wrt edible mushroom
precision <- result[1,1]/(result[1,1]+result[2,1])
recall <- result[1,1] /(result[1,1]+result[1,2])
F.Measure <- 2* (precision*recall) / (precision+recall)

#results
result
precision
recall
F.Measure