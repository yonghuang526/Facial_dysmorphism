#------------------------------------------------------------------#
# Comparison between human annotation and computational approaches #
#------------------------------------------------------------------#

#load human
human = read.csv('../input/human_anno_V2.csv',header= TRUE,row.names = 1) # 99 x 34

#dlib anno
dlib = read.csv('../input/dlib_sample.csv',header=TRUE,row.names = 1)
# subset dlib based on keypoints found in human anno
# dlib is providing more face points that we have for manual annotation
sub.dlib = dlib[match(row.names(human),row.names(dlib)),] # 99 x 34

#re-order rows to match dlib order
#sub.dlib1= sub.dlib[match(row.names(dlib),row.names(sub.dlib)),] 
#human = human[match(row.names(human),row.names(sub.dlib)),]
# extract picture height and width columns
dimensions = sub.dlib[,1:2] #(X-Y-axis)

sub.dlib2 = sub.dlib[,match(colnames(human),colnames(sub.dlib))]

row.names(dimensions) = row.names(sub.dlib2)
# flatten dimensions table in a vector, where element i is the width, and i+1 is the corresponding height.
scales = as.vector(t(dimensions[match(row.names(human),row.names(dimensions)),]))

#scaling for human annotations
# each picture X coordinate is divided by width, and Y is divided by height.
human.scale  = sapply(1:nrow(human), function(i) human[i,] / scales[(2*i):(2*(i-1) + 1)])
# transpose back to initial format
human.scale = t(human.scale)
#scaling for computer annotations
dlib.scale  = sapply(1:nrow(sub.dlib2), function(i) sub.dlib2[i,] / scales[(2*i):(2*(i-1) + 1)])
# transpose back to initial format
dlib.scale = t(dlib.scale)


png('../doc/human_dlib_cor_V2.png',1520,1200)

# split output screen based on matrix view (7 rows and 5 columns)
par(mfrow = c(7,5))

# correlation for all face points
for(i in 1:ncol(dlib.scale)){
  plot(human.scale[,i],dlib.scale[,i],xlab = 'human anno',ylab='dlib anno',main=colnames(human.scale)[i])
  # add correlation line (diagonal 1)
  abline(a=0,b=1,col='red',lty=2)

  
  fit<-lm(as.numeric(human.scale[,i]) ~ as.numeric(dlib.scale[,i]))
  cf <- round(coef(fit), 2) 
  eq <- paste0("dlib = ", cf[1],ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " human")
  mtext(eq, 3, line=-2,cex = 1,font = 2)
  
  abline(lm(fit),col = "green",lty =2)
}

dev.off()


