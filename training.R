library("OpenImageR")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Training folder with 150 images (25 people, 6 images each one)
setwd("Training")
files <- list.files()
# when testing if we have impostors I use only 100 images for the model and then I make sure files[101:150] return 0
# files = files[1:100]

# Explore the images
pic1 <- readImage(files[1])
imageShow(pic1)
dim(pic1)

#  Reshaping the matrices into vectors.  Each of the face images into a 1 x 108.000 vector
# data = matrix of reshaped images          [NumberImages, Vectors]
data = matrix(0, length(files), 200*180*3)  # [150,       108.000]
for (i in 1:length(files)){ 
  Im = readImage(files[i])   
  red = as.vector(Im[,,1])   # each vector is of size 200*180px = 36.000
  green = as.vector(Im[,,2])
  blue = as.vector(Im[,,3])
  data[i,] = t(c(red, green, blue))
}

# Label each picture with the corresponding name
filename <- as.data.frame(files)
filename$files <- sub(".jpg", "", filename$files)  # Eliminate .jpg 
eliminate = c("AT", "BT", "CT", "DT", "ET", "FT")
for (i in 1:length(eliminate)){
  filename$files <- sub(eliminate[i], "", filename$files)
}
data <- cbind(filename, data) # First column with labels
classes <- unique(filename$files)

# Random order of rows
set.seed(1)
rand <- sample(nrow(data))
data <- data[rand,]


# Compute a "mean face," which is the average for each pixel across all of the faces.
# Display the mean face as a photo in the original size.
average_face = colMeans(data[,-1])
avg_Im = array(average_face ,dim(Im))
imageShow(avg_Im)

PCA <- function(set, var_explained = 0.95){
  PCA = NULL
  # Scale data set
  scaled = scale(set, center = TRUE, scale = TRUE)
  PCA$mean = colMeans(set)
  
  # Covariance matrix
  Sigma_ = scaled%*%t(scaled)/(nrow(scaled)-1)
  Eigen = eigen(Sigma_)
  Eigenvalues = Eigen$values
  Cummulative_Var = cumsum(Eigenvalues)/sum(Eigenvalues)
  
  #     PCA + KNN    ---> min(which(Cummulative_Var > var_explained))
  #PCA$D_VarianceExplained = min(which(Cummulative_Var > var_explained))
  
  # PCA + FDA + KNN  ---> 24
  PCA$D_VarianceExplained = 24
  PCA$P_EigenVectors = Eigen$vectors[, 1:PCA$D_VarianceExplained]
  return (PCA)
}

knn = function(data, test, labels, distance, k){
  res = 0
  
  dmatrix=dist(rbind(test,data), method = distance, diag = TRUE, upper = TRUE)
  
  dmatrix=as.matrix(dmatrix)
  
  dmatrix=dmatrix[1,2:(nrow(data)+1)]  # 125
  
  ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE)
  
  labels_sel=labels[ordenados$ix[1:k]]
  
  uniqv <- unique(labels_sel)
  
  threshold = max(dmatrix) * 0.258
  
  if (min(dmatrix) > threshold){
    res = 0
  }else{
    res = uniqv[which.max(tabulate(match(labels_sel, uniqv)))] 
    #If they are two values with the same frecuency, it returs the first
  }
  return (res)
}


########################################################
###################     FISHER      ####################
########################################################

Fisher <- function(set, labeled, classes, var_explained = 0.95){
  fisher = NULL
  
  mean.train = colMeans(set)
  
  means = sapply(levels(factor(labeled[, 1])), 
                 FUN = function(classes){
                   colMeans(set[labeled[,1]==classes, 1:24])
                 })
  
  # calcular matriz SW y Sb
  Sb = matrix(0, 24, 24)
  Sw = matrix(0, 24, 24)
  
  #  between-class matrix
  for (b in 1:25) {
    Sb = Sb + (table(labeled[,1])[1])*(means[,b]-mean.train)%*%t(means[,b]-mean.train)
  }  
  
  # within-class matrix
  for (w in 1:25) {
    Sw = Sw + cov(set[labeled[,1]==as.character(w), 1:ncol(set)])*
      (table(labeled[,1])[1]-1)
  }
  
  t = solve(Sw)%*%Sb
  eigen_fisher = eigen(t)
  eigenvalues_fisher = eigen_fisher$values
  Cummulative_Var = cumsum(eigenvalues_fisher)/sum(eigenvalues_fisher)
  fisher$var_explained = min(which(Cummulative_Var> var_explained))
  fisher$eigenvector_fisher = eigen_fisher$vectors[ , 1:fisher$var_explained]
  return (fisher)
}



distances = c("euclidean", "manhattan", "canberra", "maximum", "binary")
k = c(1, 2, 3, 4, 5, 6)
result = data.frame(matrix(rep(0,length(distances)*6),nrow = 6, ncol = 5))
rownames(result) <- k
colnames(result) <- distances

variance=c(0.95,0.96,0.97,0.98,0.99)
save_results = c()

n = nrow(data)
nfolds = 6
folds = cut(1:n, breaks=nfolds, labels=FALSE)

model = rep(1,25) # 25 faces at test
for (var in 1:length(variance)){
  for (i in 1:6){
    test <- data[which(folds==i), -1]  # 25 pics
    train <- data[which(folds!=i), ] # 125 pics
    # indices 
    test.labels <- data[which(folds==i), 1]
    train.labels <- data[which(folds!=i), 1]
    
    train.scaled = scale(train[,-1], center = T, scale = T)
    test.scaled = scale(test, center=attr(train.scaled,"scaled:center"),
                        scale=attr(train.scaled,"scaled:scale"))
    # Performing pca on the training set
    PCA.train <- PCA(train.scaled)
    Eigenfaces <- t(train.scaled)%*%PCA.train$P_EigenVectors
    # Project the training and test sets on the Eigenspace of n principal components
    new_train = t(t(Eigenfaces)%*%t(train.scaled)) # 125 * 24
    new_test = t(t(Eigenfaces)%*%t(test.scaled)) 
    labeled = cbind(train.labels, new_train)
    
    min = PCA.train$D_VarianceExplained # 24
    
    fish = Fisher(new_train, labeled, classes, variance[var])
    
    train.fisher = new_train%*%fish$eigenvector_fisher
    train.fisher = as.data.frame((train.fisher))
    test.fisher = as.data.frame(new_test%*%fish$eigenvector_fisher)
    
    Img = array(train.scaled[i,], dim(Im)) 
    imageShow(Img)
    
    accuracy_dist = rep(0, length(distances))
    for (j in 1:length(distances)){
      distance = distances[j]
      print(distance)
      for (row in 1:nrow(test.fisher)){
        lab = NULL
        lab = knn(train.fisher, test.fisher[row,], train.labels, distance, k[i])
        model[row] = lab
      }
      print("Real Labels")
      print(test.labels)
      print("Model Labels")
      print(model)
      ccr.model = sum(test.labels == model)/nrow(test)
      accuracy_dist[j] = ccr.model
      cat("keeped var=", fish$var_explained," for", variance[var],"% k=",i," Accuracy ", accuracy_dist, "\n")
    }
    result[i, ] = accuracy_dist
  }
  print(result)
  save_results[[var]] = result
  print("Final de una Var")
}
save_results
# manhattan maximum euclidean
# k=1, k=2
# 0.95 0.96 0.97









########################################################
#########   Saving data for the model  #################
########################################################

train <- data # 150 pics
# indices 
train.labels <- data[, 1] #len 150
train.scaled <- scale(train[, -1], center = T, scale = T)
# Performing PCA on the set
PCA.train <- PCA(train.scaled)
Eigenfaces <- t(train.scaled)%*%PCA.train$P_EigenVectors
new_train <- t(t(Eigenfaces)%*%t(train.scaled)) # 150 * 24

labeled <- cbind(train.labels, new_train)

fish <- Fisher(new_train, labeled, classes, 0.96)

train.fisher <- new_train%*%fish$eigenvector_fisher

save(train.labels, train.scaled, train.fisher, fish, knn, PCA, Eigenfaces, file="utilsFisher.RData")








###########################################################
##################    Fisherfaces    ######################
###########################################################

fisherface = Eigenfaces%*%fish$eigenvector_fisher
avg_Im = array(fisherface[,13] ,dim(Im)) # 19
imageShow(avg_Im)

# Saving first 13 fisherfaces bases for the report
u=1
names = c("1.png","2.png","3.png","4.png","5.png","6.png","7.png","8.png","9.png","10.png","11.png","12.png","13.png")
for (d in 1:13){
  avg_Im = array(fisherface[,d] ,dim(Im)) # 24
  imageShow(avg_Im)
  rstudioapi::savePlotAsImage(names[u],width=200,height=180)
  u=u+1
}

###########################################################
##################    Eigenfaces     ######################
###########################################################

avg_Im = array(Eigenfaces[,4] ,dim(Im)) # 13
imageShow(avg_Im)

u=1
names = c("1.png","2.png","3.png","4.png","5.png","6.png","7.png","8.png","9.png","10.png","11.png","12.png","13.png",
          "14.png","15.png","16.png","17.png","18.png","19.png","20.png","21.png","22.png","23.png", "24.png")
for (d in 1:24){
  avg_Im = array(Eigenfaces[,d] ,dim(Im)) # 24
  imageShow(avg_Im)
  rstudioapi::savePlotAsImage(names[u],width=200,height=180)
  u=u+1
}

