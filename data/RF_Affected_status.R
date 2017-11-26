library(randomForest)
library(pROC)
set.seed(42)

subfeatures<- read.csv('/wdata/rotating_students/yonghuang/data/subfeatures(merged).csv')
new_features <- read.csv('/wdata/rotating_students/yonghuang/data/sub_features.csv')
update_feature.scale <- read.csv('/wdata/rotating_students/yonghuang/data/scaled_features.csv')
rownames(update_feature.scale) <- update_feature.scale[,1]
update_feature.scale <- update_feature.scale[,-1]

prediction_label <-matrix(0,nrow = nrow(update_feature.scale),ncol = 1)
test_label <- as.character(new_features$Affected.status)

### get Euclidean distances between facial landmarks
update_feature.scale_distance = t(apply(update_feature.scale,1,function(x) as.numeric(dist(matrix(x,12,2,byrow=T)))))
### randomforest sampling
rf = randomForest(y=factor(new_features$Affected.status),x=update_feature.scale_distance,strata=as.factor(new_features$Affected.status),sampsize=c(283,283),ntree=5000)
face = lm(rf$votes[,2]~new_features$Affected.status)

prediction_label <-as.character(rf$predicted)

accuracy <- sum(prediction_label == test_label) / length(test_label)

male <- new_features[grep("Male",new_features[,2]),]
female <- new_features[grep("Female",new_features[,2]),]
affected_m <- male[grep("TRUE",male[,4]),]
unaffected_m <-male[grep("FALSE",male[,4]),]
affected_fm <- female[grep("TRUE",female[,4]),]
unaffected_fm <- female[grep("FALSE",female[,4]),]

new_features[row.names(affected_m),5] <- "affected_m"
new_features[row.names(unaffected_m),5] <- "unaffected_m"
new_features[row.names(affected_fm),5] <- "affected_fm"
new_features[row.names(unaffected_fm),5] <- "unaffected_fm"


sex_status <- factor(new_features[,5], levels = c("unaffected_fm","affected_fm","unaffected_m","affected_m"))


plot.roc(new_features$Affected.status,rf$votes[,2],percent=TRUE,partial.auc=c(100, 90),print.auc=TRUE,print.auc.pattern="Corrected pAUC (100-90%% SP):\n%.1f%%", print.auc.col="#1c61b6",auc.polygon=TRUE, auc.polygon.col="#1c61b6",max.auc.polygon=TRUE, max.auc.polygon.col="#1c61b622", main="Partial AUC (pAUC)")

#boxplot(rf$votes[,2]~sex_status)

png('/wdata/rotating_students/yonghuang/data/Affected_Status_test.png',1520,1200)

boxplot(rf$votes[,2]~sex_status,col = c("#FFA07A","#FF6347","#FF7F50", "#FF0000"),main= "Probability of affected status from different sex group",ylab = "Probability of affected status")

#dev.off()


############################################################################## Count and plot number of disorders
rownames(update_feature.scale) <- update_feature.scale[,1]
update_feature.scale <- update_feature.scale[,-1]
rownames(subfeatures) <-subfeatures[,1]
subfeatures[,1] <-NULL
for(i in 1:nrow(subfeatures)){
  if(is.na(subfeatures[i,1])){
    subfeatures[i,1]<-FALSE
  }
  if(is.na(subfeatures[i,2])){
    subfeatures[i,2]<-FALSE
  }
  if(is.na(subfeatures[i,3])){
    subfeatures[i,3]<-FALSE
  }
  if(is.na(subfeatures[i,4])){
    subfeatures[i,4]<-FALSE
  }
}
count <- matrix(0,ncol= 1,nrow= 772)
for(m in 1:nrow(subfeatures)){
  count[m,1]<-sum(subfeatures[m,] == TRUE)
}
count <-cbind(rownames(subfeatures),count[,1])
rownames(count)<-count[,1]
count <- data.frame(count[,-1])
prediction_label <-matrix(0,nrow = nrow(update_feature.scale),ncol = 1)
test_label <- as.character(new_features$Affected.status)

plot(count[,1],rf$votes[,"TRUE"],xlab= "Number of disease" , ylab = "Probability of Autism",main = "Probability of autism to number of disorders",col=rainbow(5))
################################################################ labeling the facial features with probability of autism
facial_feature <- matrix(0,nrow = 12,ncol = 1)
l=1
facial_feature[l,1] <- as.character(gsub("_x","",colnames(update_feature.scale)[1]))
for(l in 1:11) {
  facial_feature[l+1,1] <- as.character(gsub("_x","",colnames(update_feature.scale)[2*l+1]))
}
Euclidean_distance <- matrix(0,nrow =  66,ncol = 1)
i = 1
k=1
for (i in 1:(nrow(facial_feature)-1)){
  for(j in (i+1):12){
    Euclidean_distance[k,1] <-  paste(facial_feature[i,1],"to",facial_feature[j,1])
    k <-k+1
  }
}
Euclidean_distance_importance <- cbind(Euclidean_distance,rf$importance)
colnames(Euclidean_distance_importance) <- c("Euclidean_distance",colnames(Euclidean_distance_importance)[2])
subfeatures <-cbind(rf$votes[,"TRUE"],subfeatures)

########################################################################## Linear regression model on subfeatures
library(ggsignif)
library(ggplot2)
fit <- lm(formula = Epilepsy.status~Autism.reported.+Language.impair+ID,data = subfeatures) # rf$votes[,"TRUE"]+
fit <- lm(formula = rf$votes[,"TRUE"]~fit$residuals)
fit_rf <- lm(formula = Epilepsy.status~rf$votes[,"TRUE"]+Autism.reported.+Language.impair+ID,data = subfeatures)
fit1 <- lm(formula = Autism.reported.~rf$votes[,"TRUE"]+Epilepsy.status+Language.impair+ID,data = subfeatures)
fit2<- lm(formula = Language.impair~rf$votes[,"TRUE"]+Autism.reported.+Epilepsy.status+ID,data = subfeatures)
fit3<- lm(formula = ID~rf$votes[,"TRUE"]+Autism.reported.+Language.impair+Epilepsy.status,data = subfeatures)
tmp = fit$residuals

summary(fit)
summary(fit1)
summary(fit2)
summary(fit3)
tmp <- data.frame(tmp)
tmp[grep("TRUE",subfeatures[,3]),2]<-"linearM_result_T"
tmp[grep("FALSE",subfeatures[,3]),2]<-"linearM_result_F"
rfvotes <-data.frame(rf$votes[,2])
rfvotes[grep("TRUE",subfeatures[,3]),2] <-"True(rfvotes)"
rfvotes[grep("FALSE",subfeatures[,3]),2] <-"False(rfvotes)"
t<-matrix(0,ncol =2,nrow=1544)
rfvotes[,1] <-tmp[,2]
tmp[,2] <- rf$votes[,2]
rfvotes <-stack(rfvotes)
tmp <-stack(tmp)
feature <- cbind(rfvotes,tmp)
feature <-feature[,-2]
feature <-feature[,-3]
feature$factor<- factor(feature[,1], levels = c("False(rfvotes)","True(rfvotes)","linearM_result_F","linearM_result_T"))
bp <- ggplot(feature, aes(x=feature$factor, y=feature[,2],fill = feature$factor)) +labs(x="Epilepsy status",y=" ") +geom_boxplot() 
bp <- bp+geom_signif(comparisons = list(c("False(rfvotes)", "True(rfvotes)"),c("linearM_result_F","linearM_result_T")),map_signif_level=TRUE)
bp+scale_fill_manual(values=c("#FF0000","#FF0000","#FFA07A", "#FFA07A"),name = "")

#boxplot(tmp~subfeatures$Epilepsy.status,ylab = "Residual", xlab = "Epilepsy status")
#boxplot(rf$votes[,2]~subfeatures$Epilepsy.status,ylab = "Residual", xlab = "Epilepsy status")
