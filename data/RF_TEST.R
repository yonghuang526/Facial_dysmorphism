library(randomForest)

new_features <- read.csv('/wdata/rotating_students/yonghuang/data/sub_features.csv')
update_feature.scale <- read.csv('/wdata/rotating_students/yonghuang/data/scaled_features.csv')
rownames(update_feature.scale) <- update_feature.scale[,1]
update_feature.scale <- update_feature.scale[,-1]

### get Euclidean distances between facial landmarks
update_feature.scale_distance = t(apply(update_feature.scale,1,function(x) as.numeric(dist(matrix(x,12,2,byrow=T)))))
### randomforest sampling
rf = randomForest(y=factor(new_features$Sex),x=update_feature.scale_distance,ntree=5000)
face = lm(rf$votes[,2]~new_features$Sex)

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

png('/wdata/rotating_students/yonghuang/data/maleness-test.png',1520,1200)

boxplot(rf$votes[,2]~sex_status)

dev.off()
