library(Splinets)
library(pbapply)
library(tictoc)
library(pracma)

order <- 4
number_of_knots <- image_size*8
knots <- seq(from = 1, to = image_size^2, length.out = number_of_knots)

total_number_of_images <- length(train_labels)

class_means <- array(dim = c(length(class_names), image_size^2))


tic("Calculating and projecting means")

for (class_index in 1:length(class_names)){
  class_means[class_index,] <- apply((train_images[train_labels == (class_index-1),,]), c(2), mean)  
}
data_values <- cbind(seq(image_size^2), t(class_means))
class_means_splines <- project(data_values, knots, order)
toc()

#Rearrange the data so its in the "correct" orientation when shown using image()
number_to_project <- total_number_of_images
labels_projected <- train_labels[1:number_to_project]

flipped_train_images <- train_images[1:number_to_project,,]/255
for(i in 1:number_to_project){
  flipped_train_images[i,,] <- t(apply(flipped_train_images[i,,],2,rev))
}

data_values <- flipped_train_images
dim(data_values) <- c(number_to_project, image_size^2)

tic("Projecting the data onto Splines")
train_vector_splines <- project(cbind(seq(image_size^2), t(data_values)), knots, order) 
toc()

#number_of_knots - order -1 is the number of coefficients the Splines have.
number_of_eigenvalues_to_save <- min(80, number_of_knots - order -1)
eigen_coeffs_array <- array(dim = c(number_to_project, length(class_names), number_of_eigenvalues_to_save))
sigma <- array(dim = c(length(class_names), number_of_knots - order -1, number_of_knots - order -1))
spect <- list()
eigen_class_sp <- list()

for(projection_index in 1:length(class_names)){
  sigma[projection_index,,] <- cov(train_vector_splines$coeff[labels_projected == (projection_index-1), ] - class_means_splines$coeff[projection_index,])
  spect[[projection_index]] <- eigen(sigma[projection_index,,], symmetric = T)
  
  eigen_class_sp[[projection_index]] <- lincomb(train_vector_splines$basis, t(spect$vectors))
  temp <- (train_vector_splines$coeff - class_means_splines$coeff[projection_index,]) %*% spect[[projection_index]]$vectors
  eigen_coeffs_array[,projection_index,] <- temp[,1:number_of_eigenvalues_to_save]
}

eigen_coeffs_matrix <- matrix(eigen_coeffs_array, nrow = number_to_project, ncol = length(class_names)*number_of_eigenvalues_to_save)
#The coefficients are saved as a long vector for every sample
#From:     vector_1  vector_2
# class_1:
# class_2
# class_3:
#To: 
# class_1,vector_1:
# class_2,vector_1:
# class_3,vector_1:
# class_1,vector_1:
# class_2,vector_2:
# class_3,vector_2:

save(eigen_coeffs_matrix, file = "eigencoeffdata/total_eigen_coeffs_matrix.RData")
library(RcppCNPy)
npySave("eigencoeffdata/total_eigen_coeffs_matrix.npy", eigen_coeffs_matrix)
npySave("eigencoeffdata/train_labels.npy", train_labels)

all_distances <- array(dim = c(length(train_labels), length(class_names)))
for(sample_index in 1:length(train_labels)){
  for(projection_index in 1:length(class_names)){
    all_distances[sample_index,projection_index] <- sum(Reshape(train_images[sample_index,,],28^2,1) - (class_means[projection_index,]))
  } 
}
npySave("eigencoeffdata/all_distances.npy", all_distances)
