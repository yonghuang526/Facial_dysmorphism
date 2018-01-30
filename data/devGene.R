library(readtext)

devGene <- read.csv("/Users/Robinhuang/Documents/OneDrive/ML/QUANTITATIVE_data.csv")
new_features <- read.csv('/Users/Robinhuang/Sites/Facial_dysmorphism/data/sub_features.csv')


updated_devGene <- devGene[which(!is.na(devGene[,5])),]
updated_devGene <- updated_devGene[which(!is.na(updated_devGene[,4])),]
rownames(new_features) <- new_features[,1]
affected_status <- matrix(0,nrow = 572,ncol = 1)
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

boxplot(updated_devGene$Head.Circumference..cm.~sex_status)
