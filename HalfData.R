#set working directory
setwd("A:/dissertationstick/analysis")
#read in the data
road<-read.csv("HalfData.csv", header=TRUE)
#load all the libraries required
library("MASS")
library("tibble")
library("ggplot2")
library("ISLR")
require("tree")
library("caret")
library("rpart")
library("adabag")
library("plyr")
library("corrgram")
library("tree")
library("randomForest")
library("ipred")
library("gbm")
library("ROCR")
library("fastAdaboost")

#set seed:the results don't change if code run by someone else
set.seed(123)
samp_sizev= floor(0.75*nrow(road))

train_indv= sample(seq_len(nrow(road)), size= samp_sizev)
train_data<-road[train_indv,]
test_data<-road[-train_indv,]
train_data$police_officer<-as.factor(train_data$police_officer)
test_data$police_officer<-as.factor(test_data$police_officer)


####adaboost

newada<-adaboost(police_officer~., data=train_data, nIter=100)

train_adata<-train_data
test_adata<-test_data
train_adata$police_officer<-as.factor(train_adata$police_officer)
test_adata$police_officer<-as.factor(test_adata$police_officer)

train_bdata<-train_data
test_bdata<-test_data
train_bdata$police_officer<-as.character(train_bdata$police_officer)
test_bdata$police_officer<-as.character(test_bdata$police_officer)
###BAGGING
 ####BAGGING
  baggmodel50<-bagging(police_officer~., data=train_data,nbagg=50, coob=TRUE)
  baggmodel100<-bagging(police_officer~., data=train_data,nbagg=100, coob=TRUE)
  baggmodel150<-bagging(police_officer~., data=train_data,nbagg=150, coob=TRUE)
  baggmodel200<-bagging(police_officer~., data=train_data,nbagg=200, coob=TRUE)
  baggmodel250<-bagging(police_officer~., data=train_data,nbagg=250, coob=TRUE)
  baggmodel300<-bagging(police_officer~., data=train_data,nbagg=300, coob=TRUE)
  baggmodel350<-bagging(police_officer~., data=train_data,nbagg=350, coob=TRUE)


  baggmodel325<-bagging(police_officer~., data=train_data,nbagg=325, coob=TRUE)
  baggmodel275<-bagging(police_officer~., data=train_data,nbagg=275, coob=TRUE)
  baggmodel225<-bagging(police_officer~., data=train_data,nbagg=225, coob=TRUE)

  OOB.BAG<-c(baggmodel50$err,baggmodel100$err,baggmodel150$err,baggmodel200$err,baggmodel250$err,baggmodel300$err,
  baggmodel350$err)  
ntre<-c(seq(from=50, to=350, by=50))
  plot(ntre, OOB.BAG, xlab="Number of Iterations", ylab="OOB Error", type="l")
  
#####Random Forest

  RF52<-randomForest(police_officer~., data=train_data, ntree=500, mtry=2 )
  RF54<-randomForest(police_officer~., data=train_data, ntree=500, mtry=4)
  RF510<-randomForest(police_officer~., data=train_data, ntree=500, mtry=10)
RF15<-randomForest(police_officer~., data=train_data, ntree=1000, mtry=15)
  plot(RF4$err.rate[,1], xlab="Number of trees", ylab="OOB Error", 
ylim=c(0.20,0.32),type="l", main="OOB error for Random Forest")
  lines(RF2$err.rate[,1], col="red")
  lines(RF8$err.rate[,1], col="blue")
  legend(800, 0.28,legend=c(2,4,10), col=c("red", "black","blue"),lty=1,
box.lty=0,title="mtry")
  #legend
  
#####ADABAG


adaboost1<-boosting(police_officer~.,data=train_data, boos=TRUE, mfinal=3,coeflearn="Freund")
					  
adaboost2<-boosting(police_officer~.,data=train_adata, boos=FALSE, mfinal=500,coeflearn="Freund")



					  
####GRADIENTBOOSTING

boost0011<-gbm(police_officer~., data=train_bdata, distribution = "bernoulli",n.trees=1500,shrinkage=0.001, interaction.depth = 1,bag.fraction=0.3)
boost00112<-gbm(police_officer~., data=train_bdata, distribution = "bernoulli",n.trees=1500,shrinkage=0.001, interaction.depth = 2, bag.fraction=0.3)
boost00114<-gbm(police_officer~., data=train_bdata, distribution = "bernoulli",n.trees=1500,shrinkage=0.001, interaction.depth = 4,bag.fraction=0.3)	
#par(mfrow=c(1,3))

#boost0011.oob<-boost0011$oobag.improve
#boost00112.oob<-(boost00112$oobag.improve)
#boost00114.oob<-boost00114$oobag.improve
gbm.perf(boost0011, plot.it=TRUE, oobag.curve=TRUE, method="OOB",overlay=FALSE)
gbm.perf(boost00112, plot.it=TRUE, oobag.curve=TRUE, method="OOB", overlay=FALSE)
gbm.perf(boost00114, plot.it=TRUE, oobag.curve=TRUE, method="OOB", overlay=FALSE)




#boost0011cv<-gbm(police_officer~., data=train_bdata, distribution = "bernoulli",n.trees=1500,shrinkage=0.001, interaction.depth = 1,bag.fraction=0.3, cv.folds=2)




boost011<-gbm(police_officer~., data=train_bdata, distribution = "bernoulli",n.trees=1500, shrinkage=0.01, interaction.depth = 1)
boost0112<-gbm(police_officer~., data=train_bdata, distribution = "bernoulli",n.trees=1500,shrinkage=0.01, interaction.depth = 2)
boost0114<-gbm(police_officer~., data=train_bdata, distribution = "bernoulli",n.trees=1500,shrinkage=0.01, interaction.depth = 4)	

gbm.perf(boost011, plot.it=TRUE, oobag.curve=TRUE, method="OOB",overlay=FALSE)
gbm.perf(boost0112, plot.it=TRUE, oobag.curve=TRUE, method="OOB", overlay=FALSE)
gbm.perf(boost0114, plot.it=TRUE, oobag.curve=TRUE, method="OOB", overlay=FALSE)


boost11<-gbm(police_officer~., data=train_bdata, distribution = "bernoulli",n.trees=1500,shrinkage=0.1, interaction.depth = 1)
boost112<-gbm(police_officer~., data=train_bdata, distribution = "bernoulli",n.trees=1500,shrinkage=0.1, interaction.depth = 2)
boost114<-gbm(police_officer~., data=train_bdata, distribution = "bernoulli",n.trees=1500,shrinkage=0.1, interaction.depth = 4)

gbm.perf(boost11, plot.it=TRUE, oobag.curve=TRUE, method="OOB",overlay=FALSE)
gbm.perf(boost112, plot.it=TRUE, oobag.curve=TRUE, method="OOB", overlay=FALSE)
gbm.perf(boost114, plot.it=TRUE, oobag.curve=TRUE, method="OOB", overlay=FALSE)
par(mfrow=c(1,3))
gbm.perf(boost11, method="OOB")
gbm.perf(boost112, method="OOB")
gbm.perf(boost114, method="OOB")

par(mfrow=c(1,3))
gbm.perf(boost0011, method="OOB")
gbm.perf(boost00112, method="OOB")
gbm.perf(boost00114, method="OOB")

par(mfrow=c(1,3))
gbm.perf(boost011, method="OOB")
gbm.perf(boost0112, method="OOB")
gbm.perf(boost0114, method="OOB")



modelroad<-glm(police_officer ~ ., data=train_data, family= "binomial")
summary(modelroad)
anov<-anova(modelroad, test="Chisq")

varname<-colnames(road[1:20])
vimlog<-data.frame(varname, varImp(modelroad))
colnames(vimlog)<-c("Variable", "Importance")
vimlog$Variable<-factor(vimlog$Variable, levels=vimlog$Variable[order(vimlog$Importance)])

theme_set(theme_bw())

# Plot
ggplot(vimlog, aes(x=Overall, y=varname)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Overall, 
                   xend=Overall, 
                   y=0, 
                   yend=varname)) + 
  labs(title="Lollipop Chart", 
       subtitle="Variable importance" 
       ) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

library(scales)
theme_set(theme_classic())

# Plot
ggplot(vimlog, aes(x=Variable, y=Importance)) + 
  geom_point(col="tomato2", size=3)+  # Draw points
  geom_segment(aes(x=Variable, 
                   xend=Variable, 
                   y=min(Importance), 
                   yend=max(Importance)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Variable importance for logistic regression") +  
  coord_flip()

library("ggcorrplot")
corr<-round(cor(road),1)




pmodelroad<-predict(modelroad, newdata=test_data, response="response")
pmodelroad<-ifelse(pmodelroad>0.5,"1", "0")
table(pmodelroad,test_data$police_officer)
mean(pmodelroad==test_data$police_officer)#accuracy

#ROC/AUC plot
library("ROCR")
pmodelroad1<-predict(modelroad, newdata=test_data, response="response")

pr<-prediction(pmodelroad1, test_data$police_officer)
prf<-performance(pr, measure = "tpr", x.measure="fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

roc<-performance(pr, "tpr", "fpr")

plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0,1), y=c(0,1), col="black", lwd=1)

auc=performance(pr, "auc")
auc=unlist(auc@y.values)


###step selection
roadstep<-stepAIC(modelroad, trace=FALSE)

steppred<-predict(roadstep, newdata=test_data, response="class")
steppred<-ifelse(steppred>0.5,"1", "0")
table(steppred,test_data$police_officer)
mean(steppred==test_data$police_officer)
			 
####TREES
treeroad<-tree(police_officer~.,data=train_data)
plot(treeroad)
text(treeroad, pretty=0) 

roadpred<-predict(treeroad, test_data, type="class")
table(roadpred, test_data$police_officer)
mean(roadpred==test_data$police_officer)



###ROC/AUC for the DT
ptree<-predict(treeroad, newdata=test_data, type="vector")
rocpred<-prediction(ptree[,2], test_data$police_officer)
 plot(performance(rocpred, "tpr", "fpr"))

auc <- performance(rocpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

text(0.7, 0.2, labels="ROC curve (AUC=0.63)")
####set a diff seed to see if the tree changes
set.seed(24)
samp_size= floor(0.75*nrow(road))

train_ind1= sample(seq_len(nrow(road)), size= samp_size)
train_data1<-road[train_ind1,]
test_data1<-road[-train_ind1,]
train_data1$police_officer<-as.factor(train_data1$police_officer)
test_data1$police_officer<-as.factor(test_data1$police_officer)

tree1road<-tree(police_officer~., data=train_data1)

tree5road<-tree(police_officer~. - Police_Force, data=train_data)

######Bagged tree model 
baggmodel300<-bagging(police_officer~., data=train_data,nbagg=300, coob=TRUE)

predbag<- predict(baggmodel300, newdata=test_data, type="class")
table(predbag, test_data$police_officer)
mean(predbag==test_data$police_officer)
confusionMatrix(predbag, test_data$police_officer)
####Roc AUC for bagged trees
test.bagprob = predict(baggmodel300, newdata=test_data, type="prob")
bagpred = prediction(test.bagprob[,2], test_data$police_officer)
bagperf = performance(bagpred, "tpr", "fpr")

plot(bagperf)
plot(bagperf, col=2, add=TRUE)
plot(perf, col=1, add=TRUE)
legend(0.6, 0.6, c(ctree, bagging), 1:2)
auc.curve = performance(bagpred, "auc")
auc.curve <- auc.curve@y.values[[1]]
auc.curve
text(0.7, 0.2, labels="ROC curve (AUC=0.71)")





vimbag<-data.frame(read.csv("varbag.csv", header=TRUE))
vimbag$Variable<-factor(vimbag$Variable, levels=vimbag$Variable[order(vimbag$Importance)])

theme_set(theme_classic())
ggplot(vimbag, aes(x=Variable, y=Importance)) + 
  geom_point(col="tomato2", size=3)+  # Draw points
  geom_segment(aes(x=Variable, 
                   xend=Variable, 
                   y=min(Importance), 
                   yend=max(Importance)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Variable importance for bagged trees") +  
  coord_flip()
###RANDOMFOREST results 

RF14<-randomForest(police_officer~., data=train_data, ntree=1000, mtry=4)




vimRF<-data.frame(read.csv("RFvar.csv", header=TRUE))
vimRF$Variable<-factor(vimRF$Variable, levels=vimRF$Variable[order(vimRF$Importance)])
theme_set(theme_classic())
ggplot(vimRF, aes(x=Variable, y=Importance)) + 
  geom_point(col="tomato2", size=3)+  # Draw points
  geom_segment(aes(x=Variable, 
                   xend=Variable, 
                   y=min(Importance), 
                   yend=max(Importance)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Variable importance for Radnom Forest") +  
  coord_flip()


RF14pred<-predict(RF14, newdata=test_data, type="response")
confusionMatrix(RF14pred, test_data$police_officer)

#####Logistic regression as factors

