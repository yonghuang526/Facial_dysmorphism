require(EBImage)
setwd("~/Documents/OneDrive/ML/TrueCase")

images <- list.files()
imgname <- 1
for(i in 1:length(images)){
  # Try-catch is necessary since some images
  # may not work.
  result <- tryCatch({
    # Image name
    imgname[i] <- images[i]
    
    
  },
  # Error function
  error = function(e){print(e)})
  
}
Truename <- data.frame(imgname)
setwd("~/Documents/OneDrive/ML/FalseCase")

images2 <- list.files()
imgname2 <- 1
for(k in 1:length(images2)){
  # Try-catch is necessary since some images
  # may not work.
  result2 <- tryCatch({
    # Image name
    imgname2[k] <- images2[k]
    
    
  },
  # Error function
  error = function(e){print(e)})
  
}
Falsename <- data.frame(imgname2)

Truename[,1] <- gsub(".jpg","",Truename[,1])
Truename[,2] <- 1:283
Falsename[,1] <- gsub(".jpg","",Falsename[,1])
Falsename[,2] <- 1:489
dataname <- matrix(0,ncol = 2, nrow = 772)
dataname[1:283,1] <-Truename[,1]
dataname[1:283,2] <-Truename[,2]

dataname[284:772,1] <- Falsename[,1]
dataname[284:772,2] <- Falsename[,2]

#dataname <- Truename
#row.names(dataname) <- dataname[,2]

dataname[1:283,3] <- "T"
dataname[284:772,3] <- "F"
set.seed(100)
idx = sample(1:10,nrow(dataname),replace =TRUE)
nfold = 10

setwd("~/Documents/OneDrive/ML/Folder")
picture_name <- matrix(0,ncol = 1,nrow = 772)
len = 1
for(fold in 1:nfold){
  val.idx_T = Truename[which(idx == fold),2]
  val.idx_T = val.idx_T[1:which(is.na(val.idx_T))[1]-1]
  picture = Truename[which(idx == fold),1]
  picture_name[len:(len+length(picture[1:which(is.na(picture))[1]-1])-1),1] = picture[1:which(is.na(picture))[1]-1]
  #write.table(val.idx_T,paste0(fold,"st_val_fold_T.csv"),row.names = FALSE,col.names = FALSE)
  len = as.integer(len+length(picture[1:which(is.na(picture))[1]-1]))
  
  val.idx_F = Falsename[which(idx == fold),2]
  val.idx_F = val.idx_F[1:which(is.na(val.idx_F))[1]-1]
  picture = Falsename[which(idx == fold),1]
  picture_name[len:(len+length(picture[1:which(is.na(picture))[1]-1])-1),1] = picture[1:which(is.na(picture))[1]-1]
  len = as.integer(len+length(picture[1:which(is.na(picture))[1]-1]))
  #write.table(val.idx_F,paste0(fold,"st_val_fold_F.csv"),row.names = FALSE,col.names = FALSE)
}
colnames(picture_name) <- "image_name"
write.csv(picture_name,"~/Documents/OneDrive/ML/picture_name.csv")
for(fold in 1:nfold){
  train.idx_T = Truename[which(idx != fold),2]
  train.idx_T = train.idx_T[1:which(is.na(train.idx_T))[1]-1]
  #write.table(train.idx_T,paste0(fold,"st_train_fold_T.csv"),row.names = FALSE,col.names = FALSE)

  train.idx_F = Falsename[which(idx != fold),2]
  train.idx_F = train.idx_F[1:which(is.na(train.idx_F))[1]-1]
  #write.table(train.idx_F,paste0(fold,"st_train_fold_F.csv"),row.names = FALSE,col.names = FALSE)
}