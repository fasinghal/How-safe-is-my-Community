library("dplyr", quietly = TRUE)
library("boot", quietly = TRUE)
library("tidyr", quietly = TRUE)
library("MASS", quietly = TRUE)

Teams <- read.csv("./data/Teams.csv")
RegularSeasonDetailedResults <- read.csv("./data/RegularSeasonDetailedResults.csv")
TourneyCompactResults <- read.csv("./data/TourneyCompactResults.csv")
seasonData<-RegularSeasonDetailedResults
dim(seasonData)
seasonData<-as.data.frame(seasonData)
namesAll<-names(seasonData)
namesWin<-namesAll[grepl("^W",namesAll)]
namesLoose<-namesAll[grepl("^L",namesAll)]
WinTeamData<-seasonData
WinTeamData[,namesLoose]<-NULL
WinTeamData<-WinTeamData[order(WinTeamData[,"Wteam"]),]
WinTeamData <- subset(WinTeamData, select = -c(Wloc))
LoseTeamData<-seasonData
LoseTeamData[,namesWin]<-NULL
LoseTeamData<-LoseTeamData[order(LoseTeamData[,"Lteam"]),]
colnames(WinTeamData)<- gsub('W', '', colnames(WinTeamData), fixed=TRUE)
colnames(LoseTeamData)<-gsub('L', '', colnames(LoseTeamData), fixed=TRUE)
Data<-rbind(WinTeamData, LoseTeamData)
agg = function(){
for(season in 1:nrow(Teams))
{
teamId = Teams[season, "Team_Id"]
Team = Data[Data$team == teamId,]
  if(nrow(Team) > 0){
    Team = aggregate(. ~ Season, Team, mean)
    if(season == 1) {
      finalData <<- Team
    } 
    else {
      finalData <<- bind_rows(finalData, Team)
    }
  }
}
}
agg()
finalData$Daynum = NULL
finalData$score = NULL
finalData$Numot = NULL
TourneyCompactResults = TourneyCompactResults[TourneyCompactResults$Season > 2002,]
Tcr = TourneyCompactResults
Tcr$Daynum = NULL
Tcr$Wloc = NULL
Tcr$Numot = NULL
TcrnamesAll<-names(Tcr)
TcrnamesWin<-TcrnamesAll[grepl("^W",TcrnamesAll)]
TcrnamesLoose<-TcrnamesAll[grepl("^L",TcrnamesAll)]
TcrWinTeamData<-Tcr
TcrWinTeamData[,TcrnamesLoose]<-NULL
TcrWinTeamData<-TcrWinTeamData[order(TcrWinTeamData[,"Wteam"]),]
TcrLoseTeamData<-Tcr
TcrLoseTeamData[,TcrnamesWin]<-NULL
TcrLoseTeamData<-TcrLoseTeamData[order(TcrLoseTeamData[,"Lteam"]),]
colnames(TcrWinTeamData)<- gsub('W', '', colnames(TcrWinTeamData), fixed=TRUE)
colnames(TcrLoseTeamData)<-gsub('L', '', colnames(TcrLoseTeamData), fixed=TRUE)
TcrData<-rbind(TcrWinTeamData, TcrLoseTeamData)
regressionData = merge(finalData, TcrData, by = c("Season", "team"))
####################################################################
Data<-finalData
kmData<-as.data.frame(Data)
kmData$Season<-NULL
kmData$team<-NULL

# Lets do a PCA and try reducing the dimentionality of the dataset.
pca_nba = prcomp(kmData, scale. = TRUE, center = TRUE)
biplot(pca_nba,cex=c(1/3,1/2), scale=0)

# Let's plot the PVE
plot(pca_nba)

# Percentage Variance Explained Plot 
pca_nba.var =pca_nba$sdev ^2
pve=pca_nba.var/sum(pca_nba.var)
plot(cumsum (pve), xlab=" Principal Component ", ylab ="
     Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b', col ="red")
abline(v = 9, lty =2,col="blue")

# lets us keep 9 Principal Components that explain ~97% 
# of the variability in data
KMDataPCA <- pca_nba$x[,1:9]

#elbow method on PCA data
k.max <- 15
t.wss<-NULL
t.wss <- sapply(1:k.max, function(k){kmeans(KMDataPCA,k)$tot.withinss})
plot(1:k.max, t.wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow Method on PCA of NBA Data", col="red")
abline(v = 3, lty =2,col="blue")
# with K means on PCA data, 3 clusters seems to be enough

# Applying K Means Elbow Method directly on to the NBA data
k.max <- 15
t.wss<-NULL
t.wss <- sapply(1:k.max, function(k){kmeans(kmData,k)$tot.withinss})
plot(1:k.max, t.wss,
type="b", pch = 19, frame = FALSE,
xlab="Number of clusters K",
ylab="Total within-clusters sum of squares",
main="Elbow Method to determine optimal # of Clusters")
abline(v = 3, lty =2,col="red")
km<-kmeans(kmData,3)
km

#H-Clustering 
#hclust_comp<-hclust(dist(kmData), method = "complete")
#plot(hclust_comp, main="Complete Linkage" ,cex=.9,labels = FALSE)
hclust_ward<-hclust(dist(kmData), method="ward.D2")
plot(hclust_ward, main="Ward.D2 Linkage" ,cex=.9,labels = FALSE)
kmData$ClusterID<-km$cluster
ResultP2<-kmData

Cluster1_R2<-ResultP2[ResultP2$ClusterID==1,]
Cluster2_R2<-ResultP2[ResultP2$ClusterID==2,]
Cluster3_R2<-ResultP2[ResultP2$ClusterID==3,]

summary(Cluster1_R2)
summary(Cluster2_R2)
summary(Cluster3_R2)

#################################################################################
regData = regressionData
regData$team = NULL
regData$Season = NULL
attach(regData)
names(regData)
formula=score~. -fgm3 -fta -fga -dr -ast -stl

#MUST keep this glm else cv.glm wont work
model1 = glm(formula = formula, data=regData)  
summary(model1)

#Cross Validation 
set.seed(42)
cv.error <-NULL
for(i in 5:20){
cv.error[i] = cv.glm(regData, model1, K=i)$delta[1]
}

#Plot the CV Error
x<-5:20
plot(x, cv.error[5:20], type = "b", main = "K-Fold Cross Validation", xlab="K", ylab="Cross Validation Error")
axis(1, at=x,labels=x)

modelAll <- lm(score~., data=regData)
AIC<-stepAIC(object =modelAll, data=regData, direction = "both")
#Step:  AIC=8774.83
#score ~ fgm + fga3 + ftm + fta + or + to + blk + pf
formulaAIC<-score ~ fgm + fga3 + ftm + fta + or + to + blk + pf
modelAIC<-lm(score ~ fgm + fga3 + ftm + fta + or + to + blk + pf, data=regData)
summary(modelAIC)
anova(model1,modelAIC)

#Residual Plot
plot(modelAIC$fitted.values, modelAIC$residuals, main="Residual Plot", xlab="Fillted Values", ylab="Residuals")
abline(lm(modelAIC$residuals~modelAIC$fitted.values), col="blue") #no trend=>good

#Quantile Plot for residuals
qqnorm(resid(modelAIC), main="Normal QQ plot")
qqline(resid(modelAIC), col="red")

#Cross Validation on Final Model
modelAIC<-glm(score ~ fgm + fga3 + ftm + fta + or + to + blk + pf, data=regData)
cv.error <-NULL
set.seed(42)
for(i in 5:20){
cv.error[i] = cv.glm(regData, modelAIC, K=i)$delta[1]
}
x<-5:20
plot(x, cv.error[5:20], type = "b", main = "K-Fold Cross Validation", xlab="K", ylab="Cross Validation Error", col="red")
axis(1, at=x,labels=x)
