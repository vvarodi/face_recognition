# Eigenfaces and Fisherfaces for face recognition
## Table of Contents:
1. [Introduction](https://github.com/vvarodi/face_recognition/1.#Introduction) 
    * [Report](https://github.com/vvarodi/face_recognition/report.pdf)
2. [Content](https://github.com/vvarodi/face_recognition/2.#Content) 
3. [Eigenfaces and Fisherfaces obtained](https://github.com/vvarodi/face_recognition/3.#Eigenfaces#and#Fisherfaces#obtained)


## 1. Introduction
<img align="right" src="https://github.com/vvarodi/face_recognition/blob/main/Images/all_25_members.jpg" width="250" >For this project, an image dataset was provided in order to perform a classification task by building a statistical model. This face recognition model must be able to tell us if a given image belongs to this dataset or not. If the face image is a member of the dataset, the model must return the corresponding label associated with that face. On the other hand, a 0 is returned when the image does not belong to any of our individuals, we have implemented thresholds to recognize this impostor. The Training dataset is a subsample of 25 individuals of the [original dataset](http://cmp.felk.cvut.cz/~spacelib/faces/faces94.html).

The statistical model was build using different techniques:
* Principal Component Analysis (PCA)
* K-Nearest Neighbours (KNN) 
* Fisher Discriminant Analysis (FDA)

Combining algorithms, two different models were obtained:
1. PCA + KNN 
2. PCA + FDA + KNN 

In both models, cross validation and parameters optimization were performed.

To see the **results**, **conclusions** and **procedure** to understand how it was done, you can see the [report](https://github.com/vvarodi/face_recognition/report.pdf).

## 2. Content
    * [Images]    Images used in the report / README
    * [Training]  Training Dataset provided
    * [RData]     Utils needed in model.R, load before using face_recognition() function
    * report.pdf  Results, conclusions and procedure   
    
    * training.R  Source code
    * model.R     Final model, face_recognition() final function

## 3. Eigenfaces and Fisherfaces obtained
<img align="left" src="https://github.com/vvarodi/face_recognition/blob/main/Images/24eigenfaces.png" width="450" >
<img  src="https://github.com/vvarodi/face_recognition/blob/main/Images/13fisherfaces.png" width="450" >





## TO DO:
- Work in the documentation
- Provide the Training images
- Provide the real full dataset, and images to test our faces and another impostors
