data = read.csv('/wdata/devGenes/Kevin - Gait Analyses/DevGenesDatabases_2017-08-29_1147.csv')
feature = read.csv('/wdata/devGenes/CURRENT DATA /kevin_facial_point_extraction/features.csv')
Gait_Model= read.csv('/wdata/devGenes/Kevin - Gait Analyses/9.19.16 Data to Kevin for Gait Model.csv')
matchdata = data[match(row.names(feature),row.names(data)),]

subdata = data[match(feature[,1],data[,1]),]
na_data = subdata[-NA,]

subdata_new = subdata[-match(na_data[,1],subdata[,1]),]

number_of_affected <- c(0);
number_of_unaffected <- c(0);

for(i in 1:nrow(subdata_new)){
  for(j in 5:ncol(subdata_new)){
    if(!is.na(subdata_new[i,j])){
      if(subdata_new[i,j] == 'TRUE'|subdata_new[i,j] == 'True'){
        subdata_new[i,4] <-TRUE;
      }
    }
  }
}

for(k in 1:nrow(subdata_new)){
  if(is.na(subdata_new[k,4])){
    subdata_new[k,4] <-FALSE;
    number_of_unaffected <-number_of_unaffected+1;
  }
  else{
    number_of_affected <- number_of_affected+1;
  }
}
#################################################

for(m in 1:nrow(subdata_new)){

  if(is.na(subdata_new[m,5])){
    subdata_new[m,5]<-""
  }
  if(is.na(subdata_new[m,6])){
    subdata_new[m,6]<-""
  }
  if(is.na(subdata_new[m,7])){
    subdata_new[m,7]<-""
  }
  if(is.na(subdata_new[m,8])){
    subdata_new[m,8]<-""
  }
  if(is.na(subdata_new[m,9])){
    subdata_new[m,9]<-""
  }
  if(is.na(subdata_new[m,10])){
    subdata_new[m,10]<-""
  }
  if(is.na(subdata_new[m,11])){
    subdata_new[m,11]<-""
  }
  if(is.na(subdata_new[m,12])){
    subdata_new[m,12]<-""
  }
}

subfeature<-cbind(data.frame(paste(subdata_new[,5],subdata_new[,9],sep = "",collapse = NULL)),data.frame(paste(subdata_new[,6],subdata_new[,10],sep = "",collapse = NULL)))
subfeature<-cbind(subfeature,data.frame(paste(subdata_new[,7],subdata_new[,11],sep = "",collapse = NULL)))
subfeature<-cbind(subfeature,data.frame(paste(subdata_new[,8],subdata_new[,12],sep = "",collapse = NULL)))
subfeature <- cbind(subdata_new[,1],subfeature)
rownames(subfeature) <-subfeature[,1]
subfeature[,1] <-NULL
names(subfeature) <- c("Autism(reported)","Epilepsy status","Language-impair","ID" ) 


write.csv(subfeature,file = "/wdata/rotating_students/yonghuang/data/subfeatures(merged).csv")
################################################

new_features <- data.frame(subdata_new[,1],subdata_new[,2],subdata_new[,3],subdata_new[,4],stringsAsFactors=FALSE)
names(new_features) <- c("ID","Sex","Age","Affected status")

new_features[,3] <- as.character(new_features[,3])

new_features[152,3] <-NA
new_features[222,3] <-NA
new_features[498,3] <-NA 



library("lubridate")
for(m in 1:nrow(new_features)){
  if(!is.na(new_features[m,3])){
     new_features[m,3]<-2017-year(new_features[m,3])
  }
}

write.csv(new_features, file = "/wdata/rotating_students/yonghuang/data/sub_features.csv",row.names=FALSE)


update_feature <-feature[match(subdata_new[,1],feature[,1]),]
rownames(update_feature) <-update_feature[,1]
update_feature <- update_feature[,-1]
dimension <- update_feature[,1:2]
scale = as.vector(t(dimension[match(row.names(update_feature),row.names(dimension)),]))

update_feature <- update_feature[,-1]
update_feature <- update_feature[,-1]
  
update_feature.scale  = sapply(1:nrow(update_feature), function(n) update_feature[n,] / scale[(2*n):(2*(n-1) + 1)])
# transpose back to initial format
update_feature.scale= t(update_feature.scale)
rownames(update_feature.scale) = rownames(update_feature)

write.csv(update_feature.scale, file = "/wdata/rotating_students/yonghuang/data/scaled_features.csv")

