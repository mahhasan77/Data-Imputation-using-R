##Assignment 2: Data imputation##
##Student ID: 20047638
##User ID: ZM21AAS@herts.ac.uk

#Importing Necessary Libraries
library(Amelia)
library(VIM)
library(impute)
library(missMethods)

# Data set Selected is IRIS 
data("iris")
iris

#Applying MCAR mechanism
# Variable with missing values, setting 15% to NA at random
iris1_imp<- iris; mis_level<- 0.15
data1 <- as.matrix(iris1_imp)

# Setting 15% of Sepal Length values as NA
set.seed(1234)
x1<- sample(1:length(data1[,1]), round(length(data1[,1])*mis_level), replace=F)
iris1_imp[x1, 1]<- NA
iris1_imp
iris1_imp$Sepal.Length
missmap(iris1_imp)

#mean value imputation
iris1_mean <- iris1_imp
iris1_mean$Sepal.Length[is.na(iris1_imp$Sepal.Length)] <- round(mean(iris1_imp$Sepal.Length, na.rm = TRUE), 2)
iris1_mean$Sepal.Length
#Root mean square error for mean imputation
iris1_mean[x1,1]
mean_res_mcar <-sqrt(sum((iris[x1,1]-iris1_mean[x1,1])^2) /length(x1)) # RMSE
round(mean_res_mcar, 2)
mean_res_mcar

# k-nearest neighbors(KNN) Imputation (with k= 12)
iris1_knn <- kNN(iris1_imp, variable = c("Sepal.Length") ,k=12)
iris1_knn$Sepal.Length
#Root mean square error for KNN imputation
iris1_knn[x1,1]
knn_res_mcar <-sqrt(sum((iris[x1,1]-iris1_knn[x1,1])^2) /length(x1)) # RMSE
round(knn_res_mcar, 2)
knn_res_mcar

# Amelia(number of imputed datasets m=5)
iris1_am <- amelia(iris1_imp, m=5, noms = "Species")
iris1_amelia_imp=(iris1_am$imputations$imp1+iris1_am$imputations$imp2+iris1_am$imputations$imp3
            +iris1_am$imputations$imp4+iris1_am$imputations$imp5)/5
iris1_amelia_imp
#Root mean square error for Amelia imputation
amelia_res_mcar =sqrt(sum((iris[x1,1]-iris1_amelia_imp[x1,1])^2) /length(x1)) #RMSE
round(amelia_res_mcar, 2)
amelia_res_mcar

#Applying MAR Mechanism
#setting 30% values to NA for Sepal length using Petal Length
iris2_imp <- iris
#setting the range of Petal length
set.seed(1234)
x = which(iris2_imp$Petal.Length > 2 & iris2_imp$Petal.Length < 4.5) 
x2 = sample(x, size =round(length(x)*0.3))
x2
# Setting Corresponding Sepal Length values to NA
iris2_imp[x2,"Sepal.Length"] <-NA
iris2_imp$Sepal.Length[x2]
missmap(iris2_imp)

#mean value imputation
iris2_mean <- iris2_imp
iris2_mean$Sepal.Length[is.na(iris2_imp$Sepal.Length)] <- round(mean(iris2_imp$Sepal.Length, na.rm = TRUE),2)
iris2_mean$Sepal.Length
#Root mean square error for mean imputation
iris2_imp[x2,1]
iris2_mean[x2,1]
mean_rms_mar <-sqrt(sum((iris[x2,1]-iris2_mean[x2,1])^2) /length(x2)) # RMSE
round(mean_rms_mar,2)
mean_rms_mar

# k-nearest neighbors(KNN) Imputation (with k=12)
iris2_knn <- kNN(iris2_imp, variable = c("Sepal.Length") ,k=12)
iris2_knn$Sepal.Length
#Root mean square error for KNN imputation
iris2_imp[x2,1]
iris2_knn[x2,1]
knn_rms_mar<-sqrt(sum((iris[x2,1]-iris2_knn[x2,1])^2) /length(x2)) # RMSE
round(knn_rms_mar, 2)
knn_rms_mar

# Amelia(number of imputed datasets m=5)
iris2_am <- amelia(iris2_imp, m=5, noms = "Species")
iris2_amelia_imp=(iris2_am$imputations$imp1+iris2_am$imputations$imp2+iris2_am$imputations$imp3
                  +iris2_am$imputations$imp4+iris2_am$imputations$imp5)/5
iris2_amelia_imp
#Root mean square error for Amelia imputation
amelia_rms_mar =sqrt(sum((iris[x2,1]-iris2_amelia_imp[x2,1])^2) /length(x2)) #RMSE
round(amelia_rms_mar,2)
amelia_rms_mar

