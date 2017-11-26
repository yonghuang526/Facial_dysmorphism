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
colnames(prediction_set) <-c("Male","Female")
#Initialize the label for later accuracy calculation
prediction_label <-matrix(0,nrow = nrow(update_feature.scale),ncol = 1)
#test_label <- as.character(new_features$Sex)
### get Euclidean distances between facial landmarks
update_feature.scale_distance = t(apply(update_feature.scale,1,function(x) as.numeric(dist(matrix(x,12,2,byrow=T)))))



for(fold in 1:nfolds){
  val.idx = which(idx == fold)
  
  update_feature.scale.val = update_feature.scale_distance[val.idx,]
  update_feature.scale.train = update_feature.scale_distance[-val.idx,]
  
  new_features.sex.train = new_features[match(row.names(update_feature.scale.train),new_features[,1]),]
  new_features.sex.val = new_features[match(row.names(update_feature.scale.val),new_features[,1]),]
  
  test_label <- as.character(new_features.sex.val$Sex)
  
  m.svm <- LiblineaR(data=update_feature.scale.train, target=new_features.sex.train$Sex,cost = 100,type = 6) #type=0 corresponds to the common SVM formulation.
  
  prediction_set[val.idx,'Male'] <- predict(m.svm,newx = update_feature.scale.val,proba = TRUE)$probabilities[,'Male']
  prediction_set[val.idx,'Female'] <- predict(m.svm,newx = update_feature.scale.val,proba = TRUE)$probabilities[,'Female']

#  prediction_set[val.idx,1] <- predict(m.svm,newx = update_feature.scale.val,proba = TRUE,decisionValues = TRUE)$decisionValues[,1]
#  prediction_set[val.idx,2] <- predict(m.svm,newx = update_feature.scale.val,proba = TRUE,decisionValues = TRUE)$decisionValues[,2]
  
  prediction_label[val.idx,1] <- as.character(predict(m.svm,newx = update_feature.scale.val)$predictions)
  
#  plot(predict(m.svm,newx = update_feature.scale.val,proba = TRUE,decisionValues = TRUE)$decisionValues[,1],col = ifelse(test = predict(m.svm,newx = update_feature.scale.val)$predictions == 'Male',yes = 'blue',no='red'))
#  abline(h=0,lty=2)
  
  prediction <-as.character(predict(m.svm,newx = update_feature.scale.val)$predictions)
  accuracy[fold] <- sum(prediction == test_label) / length(test_label)
  
}

m <-mean(accuracy)
s <-sd(accuracy)
error <- qnorm(0.975)*s/sqrt(10)

left <- m-error
right <- m+error

#accuracy <- sum(prediction_label == test_label) / length(test_label)

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


#Building the linear model to test the importance of sex and age to the probability of maleness
sub_features <- matrix(0,nrow = 772,ncol = 3)

colnames(sub_features) <- c("Probability_maleness","Sex","Age")
sub_features[,1]<-prediction_set[,1]
sub_features[,2]<-as.numeric(prediction_label)
sub_features[,3]<-new_features$Age
sub_features <-data.frame(sub_features)

fit <- lm(formula = Probability_maleness~Sex+Age,data = sub_features)
summary(fit)

#Scatter Plot
library(RColorBrewer)
display.brewer.all()
cols<-brewer.pal(n=4,name="Set1")
cols_t1<-cols[prediction_label]


png('/wdata/rotating_students/yonghuang/data/Probability(maleness)_Age.png',1440,900)

plot(new_features$Age,prediction_set[,"Male"], main= "Relationship of Masculinity to Age",xlab = "Age",ylab = "Probability_Maleness",col = cols_t1,cex.axis=1.5,cex.main=1.5,pch=19,cex.lab=1.5)
legend("topright",c("Male","Female"),col = cols_t1, cex=1.7,box.lty=2, box.lwd=2,pt.bg=cols_t1, pch=21:21)

dev.off()

png('/wdata/rotating_students/yonghuang/data/maleness-test(SVM).png',1240,900)

boxplot(prediction_set[,"Male"]~sex_status,ylab = "Facial Masculinity",col=c("#FFA07A","#FF7F50","#FF6347","#FF0000"),par(cex.lab=1.5),par(cex.axis=1.5),par(cex.main = 1.5))

dev.off()



#plot(predict(m.svm,newx = update_feature.scale.val,proba = TRUE,decisionValues = TRUE)$decisionValues[,1],col = ifelse(test = predict(m.svm,newx = update_feature.scale.val)$predictions == 'Male',yes = 'blue',no='red'))
#abline(h=0,lty=2)
