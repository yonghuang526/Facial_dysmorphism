library(randomForest)

#new_features <- read.csv('/wdata/rotating_students/yonghuang/data/sub_features.csv')
#update_feature.scale <- read.csv('/wdata/rotating_students/yonghuang/data/scaled_features.csv')
gene_group <- read.csv("/Users/Robinhuang/Documents/OneDrive/ML/two_batches_id.csv")
new_features <- read.csv('/Users/Robinhuang/Sites/Facial_dysmorphism/data/sub_features.csv')
update_feature.scale <- read.csv('/Users/Robinhuang/Sites/Facial_dysmorphism/data/scaled_features.csv')

rownames(update_feature.scale) <- update_feature.scale[,1]
update_feature.scale <- update_feature.scale[,-1]

### get Euclidean distances between facial landmarks
update_feature.scale_distance = t(apply(update_feature.scale,1,function(x) as.numeric(dist(matrix(x,12,2,byrow=T)))))
### randomforest sampling
#trainset = new_features[grep("FALSE",new_features$Affected.status),]
rf = randomForest(y=factor(new_features$Sex),x=update_feature.scale_distance,ntree=5000,strata=as.factor(new_features$Affected.status),sampsize=c(400,1))
face = lm(rf$votes[,2]~new_features$Sex)
new_features[,5] <- rf$votes[,2]

new_features$ID <- as.character(new_features$ID)
new_features$Sex <- as.character(new_features$Sex)


male <- new_features[grep("Male",new_features[,2]),]
female <- new_features[grep("Female",new_features[,2]),]
affected_m <- male[grep("TRUE",male[,4]),]
unaffected_m <-male[grep("FALSE",male[,4]),]
affected_fm <- female[grep("TRUE",female[,4]),]
unaffected_fm <- female[grep("FALSE",female[,4]),]

new_features[row.names(affected_m),6] <- "affected_m"
new_features[row.names(unaffected_m),6] <- "unaffected_m"
new_features[row.names(affected_fm),6] <- "affected_fm"
new_features[row.names(unaffected_fm),6] <- "unaffected_fm"


devgene_info <- data.frame(matrix(ncol = 6,nrow = 184))

for (i in 1:nrow(gene_group)){
  devgene_info[i,] = new_features[grep(gene_group$X103_1[i],new_features$ID),]
}

devgene_info <- devgene_info[-which(is.na(devgene_info)),]


sex_status <- factor(new_features[,6], levels = c("unaffected_fm","affected_fm","unaffected_m","affected_m"))
sex_status_dev <- factor(devgene_info[,6], levels = c("unaffected_fm","affected_fm","unaffected_m","affected_m"))

colnames(devgene_info) <- c("ID","SEX","AGE","Affected_status","proba_M","Label")
##png('/wdata/rotating_students/yonghuang/data/maleness-test.png',1520,1200)

boxplot(rf$votes[,2]~sex_status)
boxplot(devgene_info[,5]~sex_status_dev)

write.csv(devgene_info,"/Users/Robinhuang/Sites/Facial_dysmorphism/data/devgene_info.csv")
##dev.off()
