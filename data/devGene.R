library(readtext)
library(randomForest)
devGene <- read.csv("/Users/Robinhuang/Documents/OneDrive/ML/QUANTITATIVE_data.csv")
new_features <- read.csv('/Users/Robinhuang/Sites/Facial_dysmorphism/data/sub_features.csv')


updated_devGene <- devGene[which(!is.na(devGene[,5])),]
updated_devGene <- updated_devGene[which(!is.na(updated_devGene[,4])),]
updated_devGene <- updated_devGene[which(!is.na(updated_devGene[,3])),]
rownames(new_features) <- new_features[,1]
affected_status <- matrix(0,nrow = nrow(updated_devGene),ncol = 1)
for(i in 1:nrow(updated_devGene)){
  rname <- paste("^",updated_devGene$Participant_ID[i],"$",sep = "")
  affected_status[i,1] = new_features[grep(rname,row.names(new_features)),4]
}

updated_devGene[,30]<- affected_status[,1]

male <- updated_devGene[grep("1",updated_devGene$Male),]
female <- updated_devGene[grep("0",updated_devGene$Male),]
affected_m <- male[grep("1",male[,30]),]
unaffected_m <-male[grep("0",male[,30]),]
affected_fm <- female[grep("1",female[,30]),]
unaffected_fm <- female[grep("0",female[,30]),]

updated_devGene[row.names(affected_m),31] <- "affected_m"
updated_devGene[row.names(unaffected_m),31] <- "unaffected_m"
updated_devGene[row.names(affected_fm),31] <- "affected_fm"
updated_devGene[row.names(unaffected_fm),31] <- "unaffected_fm"

sex_status <- factor(updated_devGene[,31], levels = c("unaffected_fm","affected_fm","unaffected_m","affected_m"))

write.csv(sex_status,"/Users/Robinhuang/Sites/Facial_dysmorphism/data/Saved_object/sex_status.csv")
#####Linear model 
fit <- lm(formula = updated_devGene$Head.Circumference..cm.~updated_devGene$Age.at.Collection)
fit_fr <- lm(formula = updated_devGene$Index.Ring~updated_devGene$Age.at.Collection)

### get Euclidean distances between facial landmarks
update_feature.scale_distance = t(apply(updated_devGene[,6:29],1,function(x) as.numeric(dist(matrix(x,12,2,byrow=T)))))
### randomforest sampling
rf = randomForest(y=factor(updated_devGene$Male),x=update_feature.scale_distance,strata=as.factor(new_features$Affected.status),sampsize=c(400,1),ntree=5000)

#### Linear model
fit_rf <- lm(formula = rf$votes[,2]~updated_devGene$Age.at.Collection)

##### Female group
affected_fm_H_corrected <- data.frame(fit$residuals[grep("1",female[,30])])
unaffected_fm_H_corrected <- data.frame(fit$residuals[grep("0",female[,30])])

affected_fm_F_corrected <- data.frame(fit_fr$residuals[grep("1",female[,30])])
unaffected_fm_F_corrected <- data.frame(fit_fr$residuals[grep("0",female[,30])])

affected_fm_proba_corrected <- data.frame(fit_rf$residuals[grep("1",female[,30])])
unaffected_fm_proba_corrected <- data.frame(fit_rf$residuals[grep("0",female[,30])])

A_female_group <- matrix(0,nrow =58 ,ncol = 3)
Ua_female_group <- matrix(0,nrow =222 ,ncol = 3)
A_female_group[,1] <-affected_fm_H_corrected[,1]
Ua_female_group[,1] <-unaffected_fm_H_corrected[,1]
A_female_group[,2] <-affected_fm_F_corrected[,1]
Ua_female_group[,2] <-unaffected_fm_F_corrected[,1]
A_female_group[,3] <-affected_fm_proba_corrected[,1]
Ua_female_group[,3] <-unaffected_fm_proba_corrected[,1]

colnames(A_female_group) <- c("head_circum","finger_ratio","facial_proba")
colnames(Ua_female_group) <- c("head_circum","finger_ratio","facial_proba")

write.csv(A_female_group,"/Users/Robinhuang/Sites/Facial_dysmorphism/data/Saved_object/A_female_group.csv")
write.csv(Ua_female_group,"/Users/Robinhuang/Sites/Facial_dysmorphism/data/Saved_object/Ua_female_group.csv")
###--T test
t.test(unaffected_fm_H_corrected,affected_fm_H_corrected,alternative = "less")
t.test(unaffected_fm_F_corrected,affected_fm_F_corrected,alternative = "less")
t.test(unaffected_fm_proba_corrected,affected_fm_proba_corrected,alternative = "less")
###--Wilcox test
wilcox.test(unaffected_fm_H_corrected[,1],affected_fm_H_corrected[,1],alternative = "less")
wilcox.test(unaffected_fm_F_corrected[,1],affected_fm_F_corrected[,1],alternative = "less")
wilcox.test(unaffected_fm_proba_corrected[,1],affected_fm_proba_corrected[,1],alternative = "less")

#### Male group
affected_m_H_corrected <- data.frame(fit$residuals[grep("1",male[,30])])
unaffected_m_H_corrected <- data.frame(fit$residuals[grep("0",male[,30])])

affected_m_F_corrected <- data.frame(fit_fr$residuals[grep("1",male[,30])])
unaffected_m_F_corrected <- data.frame(fit_fr$residuals[grep("0",male[,30])])

affected_m_proba_corrected <- data.frame(fit_rf$residuals[grep("1",male[,30])])
unaffected_m_proba_corrected <- data.frame(fit_rf$residuals[grep("0",male[,30])])

A_male_group <- matrix(0,nrow =146 ,ncol = 3)
Ua_male_group <- matrix(0,nrow =144 ,ncol = 3)
A_male_group[,1] <-affected_m_H_corrected[,1]
Ua_male_group[,1] <-unaffected_m_H_corrected[,1]
A_male_group[,2] <-affected_m_F_corrected[,1]
Ua_male_group[,2] <-unaffected_m_F_corrected[,1]
A_male_group[,3] <-affected_m_proba_corrected[,1]
Ua_male_group[,3] <-unaffected_m_proba_corrected[,1]

colnames(A_male_group) <- c("head_circum","finger_ratio","facial_proba")
colnames(Ua_male_group) <- c("head_circum","finger_ratio","facial_proba")

write.csv(A_male_group,"/Users/Robinhuang/Sites/Facial_dysmorphism/data/Saved_object/A_male_group.csv")
write.csv(Ua_male_group,"/Users/Robinhuang/Sites/Facial_dysmorphism/data/Saved_object/Ua_male_group.csv")

###--T test
t.test(unaffected_m_H_corrected,affected_m_H_corrected,alternative = "less")
t.test(unaffected_m_F_corrected,affected_m_F_corrected,alternative = "less")
t.test(unaffected_m_proba_corrected,affected_m_proba_corrected,alternative = "less")
###--Wilcox test
wilcox.test(unaffected_m_H_corrected[,1],affected_m_H_corrected[,1],alternative = "less")
wilcox.test(unaffected_m_F_corrected[,1],affected_m_F_corrected[,1],alternative = "less")
wilcox.test(unaffected_m_proba_corrected[,1],affected_m_proba_corrected[,1],alternative = "less")

### Original 
par(mfrow=c(1,3))
boxplot(updated_devGene$Index.Ring~sex_status)
boxplot(updated_devGene$Head.Circumference..cm.~sex_status)
boxplot(rf$votes[,2]~sex_status)

### Age effect excluded 
boxplot(fit$residuals~sex_status)
boxplot(fit_fr$residuals~sex_status)
boxplot(fit_rf$residuals~sex_status)

### scaled

par(mfrow=c(1,3))
boxplot(scale(fit_fr$residuals)~sex_status, ylim = c(-3,4),ylab = "Scaled Masculinity",main = "Finger ratios",cex.lab=1.5)
boxplot(scale(fit$residuals)~sex_status, main = "Head circumference")
boxplot(scale(fit_rf$residuals)~sex_status,ylim = c(-3,4), main = "Facial dysmorphism")

save(fit_fr, file = "/Users/Robinhuang/Sites/Facial_dysmorphism/data/Saved_object/finger_model.rda")
save(fit, file = "/Users/Robinhuang/Sites/Facial_dysmorphism/data/Saved_object/head_model.rda")
save(fit_rf, file = "/Users/Robinhuang/Sites/Facial_dysmorphism/data/Saved_object/facial_model.rda")

