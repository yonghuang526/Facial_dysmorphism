library(LiblineaR)
set.seed(42)

new_features <- read.csv('/wdata/rotating_students/yonghuang/data/sub_features.csv')
update_feature.scale <- read.csv('/wdata/rotating_students/yonghuang/data/scaled_features.csv')

# Number of folds/rounds
nfolds = 10
# Generate random index
idx = sample(1:nfolds,nrow(update_feature.scale),replace=TRUE)
rownames(update_feature.scale) <- update_feature.scale[,1]
update_feature.scale <- update_feature.scale[,-1]

accuracy <-matrix(0,nrow = 10,ncol = 1)

prediction_set <- matrix(0,nrow = nrow(update_feature.scale),ncol = 2)

#Initialize the label for later accuracy calculation
prediction_label <-matrix(0,nrow = nrow(update_feature.scale),ncol = 1)
test_label <- as.character(new_features$Affected.status)
### get Euclidean distances between facial landmarks
update_feature.scale_distance = t(apply(update_feature.scale,1,function(x) as.numeric(dist(matrix(x,12,2,byrow=T)))))



for(fold in 1:nfolds){
  val.idx = which(idx == fold)
  
  update_feature.scale.val = update_feature.scale_distance[val.idx,]
  update_feature.scale.train = update_feature.scale_distance[-val.idx,]
  
  new_features.sex.train = new_features[match(row.names(update_feature.scale.train),new_features[,1]),]
  new_features.sex.val = new_features[match(row.names(update_feature.scale.val),new_features[,1]),]
  
  #test_label <- as.character(new_features.sex.val$Affected.status)
  
  m.svm <- LiblineaR(data=update_feature.scale.train, target=new_features.sex.train$Affected.status,cost = 100,type = 6) #type=0 corresponds to the common SVM formulation.
  
  prediction_set[val.idx,1] <- predict(m.svm,newx = update_feature.scale.val,proba = TRUE)$probabilities[,1]
  prediction_set[val.idx,2] <- predict(m.svm,newx = update_feature.scale.val,proba = TRUE)$probabilities[,2]
  
#  prediction_set[val.idx,1] <- predict(m.svm,newx = update_feature.scale.val,proba = TRUE,decisionValues = TRUE)$decisionValues[,1]
#  prediction_set[val.idx,2] <- predict(m.svm,newx = update_feature.scale.val,proba = TRUE,decisionValues = TRUE)$decisionValues[,2]

  prediction_label[val.idx,1] <- as.character(prediction_set[val.idx,1] < 0.5264888)
#  prediction_label[val.idx,1] <- as.character(predict(m.svm,newx = update_feature.scale.val)$predictions)
  
  #prediction <-as.character(predict(m.svm,newx = update_feature.scale.val)$predictions)
  #accuracy[fold] <- sum(prediction == test_label) / length(test_label)
  
}
#The confidence interval of accuracy 
#m <-mean(accuracy)
#s <-sd(accuracy)
#error <- qnorm(0.975)*s/sqrt(10)

#left <- m-error
#right <- m+error

table(new_features$Affected.status,prediction_label)
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


prediction_label <-factor(prediction_label)


#Building the linear model to test the importance of sex and age to the probability of affected status
sub_features <- matrix(0,nrow = 772,ncol = 4)

colnames(sub_features) <- c("Probability_Affected","Affected_Status","Sex","Age")
sub_features[,1]<-prediction_set[,2]
sub_features[,2]<-as.numeric(prediction_label)
sub_features[,3]<-new_features$Sex
sub_features[,4]<-new_features$Age
sub_features <-data.frame(sub_features)

fit <- lm(formula = Probability_Affected~Affected_Status+Sex+Age,data = sub_features)
fit2 <-lm(formula = Probability_Affected~Age,data = sub_features)
summary(fit)
summary(fit2)



#Scatter Plot
library(RColorBrewer)
display.brewer.all()
cols<-brewer.pal(n=4,name="Set1")
cols_t1<-cols[factor(new_features$Affected.status)]

library(pROC)
plot.roc(test_label,prediction_set[,2],percent=TRUE,partial.auc=c(100, 90),print.auc=TRUE,print.auc.pattern="Corrected pAUC (100-90%% SP):\n%.1f%%", print.auc.col="#1c61b6",auc.polygon=TRUE, auc.polygon.col="#1c61b6",max.auc.polygon=TRUE, max.auc.polygon.col="#1c61b622", main="Partial AUC (pAUC)")

rocobj1 <- plot.roc(new_features$Affected.status,prediction_set[,2], main="Statistical comparison", percent=TRUE, col="#1c61b6")

coords(rocobj1,"b",ret=c("threshold", "specificity", "sensitivity"),as.list=FALSE, drop=TRUE, best.method=c( "youden","closest.topleft"),best.weights=c(1, 0.3))

##################################################
png('/wdata/rotating_students/yonghuang/data/Probability(Affected_status)_Age.png',1440,900)

plot(new_features$Age,prediction_set[,2], main = "Relationship of age to affected probability",xlab = "Age",ylab = "Probability of Affected",col = cols_t1,cex=1.5,cex.axis=1.5,pch=19,cex.lab=1.5)
legend("topright",c("True","False"),col = levels(as.factor(cols_t1)), cex=1.7,box.lty=2, box.lwd=2,pt.bg=levels(as.factor(cols_t1)), pch=21:21)
abline(fit2, lwd = 2)

dev.off()

png('/wdata/rotating_students/yonghuang/data/Affected_status_test(SVM).png',1520,1200)

boxplot(prediction_set[,2]~sex_status)

dev.off()