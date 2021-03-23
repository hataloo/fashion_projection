##Need to run data_preparation before.
library(factoextra)
total_number_of_images <- length(train_labels)
#Rearrange the data so its in the "correct" orientation when shown using image()

for(i in 1:total_number_of_images){
  flipped_train_images[i,,] <- t(apply(train_images[i,,],2,rev))/255
}
train_vectors <- flipped_train_images
dim(train_vectors) <- c(length(train_labels), image_size^2)

train_vectors_centered <- train_vectors - colMeans(train_vectors)

pca <- prcomp(train_vectors_centered, scale = F)

fviz_eig(pca)


#Change of basis to PCA-basis. Use first X columns to only
#consider the first X principal components.
train_coeff_pca <- train_vectors_centered %*% pca$rotation

library(RcppCNPy)
npySave("eigencoeffdata/train_coeff_pca.npy", train_coeff_pca)
npySave("eigencoeffdata/train_coeff_pca_cut.npy", train_coeff_pca[,1:80])
