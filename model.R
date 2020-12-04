library("OpenImageR")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
utils <- load("utilsFisher.RData")


face_recognition <- function(image, objs=NULL){
  data =  matrix(0, 1, 200*180*3)
  Im = readImage(image)
  red = as.vector(Im[,,1]) 
  green = as.vector(Im[,,2])
  blue = as.vector(Im[,,3])
  data[1, ] = t(c(red, green, blue))
  
  tes =  scale(data, center=attr(train.scaled,"scaled:center"),
               scale=attr(train.scaled,"scaled:scale"))
  new_train = t(t(Eigenfaces)%*%t(tes))
  
  test.new_fisher = as.data.frame(new_train%*%fish$eigenvector_fisher)
  
  res = knn(train.fisher, test.new_fisher, train.labels, "manhattan", 1)
  res = as.numeric(res)
  return(res)
}


##############################################################################
# EXAMPLE   To use the function:                                             #
# This file (model.R) and (utilsFisher.RData) must be in the same directory  #
#                                                                            #
#  After loading (utilsFisher.RData). Go to the folder with all images       #
#                                                                            #    
#  PARAMETERS:                                                               #
#     image is the image to recognize: example("13DT.jpg") of dim 200x180x3  #
#                                                                            #
# this_dir is the directory with the images to test. Go there and list files #
##############################################################################

this_dir <- "..."
setwd(this_dir)

files <- list.files()  

try = 45
fc <- face_recognition(files[try])
fc
files[try]




#############################################################################
#     To test more than only one image at same time                         #
#     Warning all files inside the dir must be the images to test .jpg      #
#############################################################################

files <- list.files()
classification = rep(1,length(files))
j=1
for (i in 1:length(classification)){
  print(files[i])
  classification[j] <- face_recognition(files[i])
  j=j+1
}
classification

