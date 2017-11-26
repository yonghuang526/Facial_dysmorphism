#----------------------------------------------------#
# Source converting DLIB output in data.frame format #
#----------------------------------------------------#

convert2R <- function(inputfile){
  # read DLIB output file
  x = read.table(inputfile,as.is = TRUE,sep='\t')
  # filter duplicated lines
  idx = which(duplicated(x[,1]))
  # if there is at least a duplicated line
  if(length(idx) > 0){
    # get duplicated IDs
    tmp= apply(x[,c(3,20)],2,function(i)gsub(pattern = '\\(|\\)|\\,',replacement = '',i))
    tmp2 = apply(tmp,2,function(i) unlist(lapply(strsplit(i,split=' '),function(x)as.numeric(x)[1])))
    # compute jaw left and right distance, in order to keep the smallest one among the duplicates
    tmp3 = tmp2[,2]-tmp2[,1]
    # loop over duplicates
    filt = idx
    for(i in 1:length(idx)){
      if(tmp3[idx[i]] > tmp3[idx[i]-1]) filt[i] = idx[i] - 1
    }
    #duplicates removal
    new.x = x[-filt,]
    x = new.x
  }
  # process row names
  rowN = x[,1]
  z = gsub(pattern = '.*\\/',replacement='',rowN)
  z =  gsub(pattern = '\\..*', replacement = '', z)
  x = x[,-1]
  if(any(is.na(x[,ncol(x)]))) x = x[,-ncol(x)]
  row.names(x) = z
  
  tmp= apply(x[,-1],2,function(i)gsub(pattern = '\\(|\\)|\\,',replacement = '',i))
  tmp2 = lapply(1:ncol(tmp),function(i) matrix(unlist(lapply(strsplit(tmp[,i],split=' '),as.numeric)),ncol=2,byrow=TRUE))
  #remove number of channels column
  tmp3 = matrix(unlist(lapply(strsplit(gsub(pattern = '\\(|\\)|\\,',replacement = '',x[,1]),split=' '),as.numeric)),ncol=3,byrow=TRUE)
  tmp3 = tmp3[,-3]
  
  #merge
  tmp4 = do.call(cbind,tmp2)
  tmp = cbind(tmp3, tmp4)
  row.names(tmp) = row.names(x)
  
  #colnames
  #keypoint index (based on people left and right)
  
  idx = c('height','width','top_left_jaw_x','top_left_jaw_y','top_left_jaw_2_x','top_left_jaw_2_y','mid_left_jaw_x','mid_left_jaw_y',
          'mid_left_jaw_2_x','mid_left_jaw_2_y','mid_left_jaw_3_x','mid_left_jaw_3_y','bot_left_jaw_x','bot_left_jaw_y',
          'top_left_chin_x','top_left_chin_y','mid_left_chin_x','mid_left_chin_y','bot_chin_x','bot_chin_y',
          'mid_right_chin_x','mid_right_chin_y','top_right_chin_x','top_right_chin_y',
          'bot_right_jaw_x','bot_right_jaw_y','mid_right_jaw_3_x','mid_right_jaw_3_y','mid_right_jaw_2_x','mid_right_jaw_2_y',
          'mid_right_jaw_x','mid_right_jaw_y','top_right_jaw_2_x','top_right_jaw_2_y','top_right_jaw_x','top_right_jaw_y',
          'outer_left_eyebrow_x','outer_left_eyebrow_y','mid_left_eyebrow_x','mid_left_eyebrow_y',
          'mid_left_eyebrow_2_x','mid_left_eyebrow_2_y','mid_left_eyebrow_3_x','mid_left_eyebrow_3_y','inner_left_eyebrow_x','inner_left_eyebrow_y',
          'inner_right_eyebrow_x','inner_right_eyebrow_y','mid_right_eyebrow_3_x','mid_right_eyebrow_3_y','mid_right_eyebrow_2_x','mid_right_eyebrow_2_y',
          'mid_right_eyebrow_x','mid_right_eyebrow_y','outer_right_eyebrow_x','outer_right_eyebrow_y','top_nose_x','top_nose_y','top_nose_2_x','top_nose_2_y',
          'mid_nose_x','mid_nose_y','nose_tip_x','nose_tip_y','bot_left_nose_x','bot_left_nose_y','bot_left_nose_2_x','bot_left_nose_2_y',
          'bot_mid_nose_x','bot_mid_nose_y','bot_right_nose_2_x','bot_right_nose_2_y','bot_right_nose_x','bot_right_nose_y','outer_left_eye_corner_x','outer_left_eye_corner_y',
          'top_left_eye_x','top_left_eye_y','top_left_eye_2_x','top_left_eye_2_y','inner_left_eye_corner_x','inner_left_eye_corner_y',
          'bot_left_eye_x','bot_left_eye_y','bot_left_eye_2_x','bot_left_eye_2_y','inner_right_eye_corner_x','inner_right_eye_corner_y',
          'top_right_eye_2_x','top_right_eye_2_y','top_right_eye_x','top_right_eye_y','outer_right_eye_corner_x','outer_right_eye_corner_y',
          'bot_right_eye_x','bot_right_eye_y','bot_right_eye_2_x','bot_right_eye_2_y','left_mouth_corner_x','left_mouth_corner_y',
          'top_left_lip_x','top_left_lip_y','top_left_lip_2_x','top_left_lip_2_y','top_center_lip_x','top_center_lip_y',
          'top_right_lip_2_x','top_right_lip_2_y','top_right_lip_x','top_right_lip_y','right_mouth_corner_x','right_mouth_corner_y',
          'bot_right_lip_x','bot_right_lip_y','bot_right_lip_2_x','bot_right_lip_2_y','bot_center_lip_x','bot_center_lip_y',
          'bot_left_lip_2_x','bot_left_lip_2_y','bot_left_lip_x','bot_left_lip_y','mid_left_upper_lip_x','mid_left_upper_lip_y',
          'mid_left_upper_lip_2_x','mid_left_upper_lip_2_y','mid_center_upper_lip_x','mid_center_upper_lip_y','mid_right_upper_lip_2_x','mid_right_upper_lip_2_y',
          'mid_right_upper_lip_x','mid_right_upper_lip_y','mid_right_lower_lip_x','mid_right_lower_lip_y','mid_left_lower_lip_x','mid_left_lower_lip_y')
  
  
  colnames(tmp) = idx
  row.names(tmp) = z
  return(tmp)
}


#example
inputfile = '../input/dlib_output_090216.txt'
data = convert2R(inputfile = inputfile)
write.csv(data[,c('height','width','outer_left_eyebrow_x','outer_left_eyebrow_y','inner_left_eyebrow_x','inner_left_eyebrow_y','inner_right_eyebrow_x','inner_right_eyebrow_y',
                  'outer_right_eyebrow_x','outer_right_eyebrow_y','top_nose_x','top_nose_y','nose_tip_x','nose_tip_y','outer_left_eye_corner_x','outer_left_eye_corner_y',
                  'inner_left_eye_corner_x','inner_left_eye_corner_y','inner_right_eye_corner_x','inner_right_eye_corner_y','outer_right_eye_corner_x','outer_right_eye_corner_y',
                  'left_mouth_corner_x','left_mouth_corner_y','right_mouth_corner_x','right_mouth_corner_y')],file='../input/dlib_output_090216.csv')
