shroom <-read.csv("D:/SEM3 MS/R for DS P2/agaricus-lepiota.data", header=FALSE,na.strings="?")
colnames(shroom)<-c("class","shape","cap-surface","cap-color","bruises","odor","gill-attachment","gill-spacing","gill-size","gill-color","stalk-shape","stalk-root","stalk-surface-above-ring","stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring","veil-type","veil-color","ring-number","ring-type","spore-print-color","population","habitat")

for(i in 1:16){
  barplot(summary(shroom[,i]), xlab = colnames(shroom)[i])
}

NACounts <- apply(X = shroom, MARGIN = 2, FUN = function(x) sum(is.na(x)))
NACounts

