##Need to run data_preparation before.

###PCA centered around every class
number_of_components_to_save <- 80
pca_coeffs_array <- array(dim = c(number_of_images, length(class_names), number_of_components_to_save))
normalized_pca_coeffs_array <- array(dim = c(number_of_images, length(class_names), number_of_components_to_save))
test_pca_coeffs_array <- array(dim = c(length(test_labels), length(class_names), number_of_components_to_save))
test_normalized_pca_coeffs_array <- array(dim = c(length(test_labels), length(class_names), number_of_components_to_save))

pca_sigma_cw <- array(dim = c(length(class_names), image_size^2, image_size^2))
pca_spect_cw <- list()
pb <- timerProgressBar(min = 0, max = length(class_names))
for(projection_index in 1:length(class_names)){
  pca_sigma_cw[projection_index,,] <- cov(sweep(train_vectors[train_labels == (projection_index-1),],2,class_means[projection_index,]))
  pca_spect_cw[[projection_index]] <- eigen(pca_sigma_cw[projection_index,,], symmetric = TRUE)
  setTimerProgressBar(pb, value = projection_index)
}
pb <- timerProgressBar(min = 0, max = length(class_names))
for(projection_index in 1:length(class_names)){
  pca_coeffs_array[,projection_index,] <- (sweep(train_vectors, 2, class_means[projection_index,]) %*% pca_spect_cw[[projection_index]]$vectors)[,1:number_of_components_to_save]
  test_pca_coeffs_array[,projection_index,] <- (sweep(test_vectors, 2, class_means[projection_index,]) %*% pca_spect_cw[[projection_index]]$vectors)[,1:number_of_components_to_save]
  for(component_index in 1:number_of_components_to_save){
    normalized_pca_coeffs_array[,projection_index,component_index] <- pca_coeffs_array[,projection_index, component_index]/sqrt(pca_spect_cw[[projection_index]]$values[component_index])
    test_normalized_pca_coeffs_array[,projection_index,component_index] <- test_pca_coeffs_array[,projection_index, component_index]/sqrt(pca_spect_cw[[projection_index]]$values[component_index])
  }
  setTimerProgressBar(pb, value = projection_index)
}

#Verify that the coefficients have mean ~0 and var ~1,
#hist should hopefully resemble a Gaussian but not necessary.
class_index <- 10
component_index <- 1
hist(normalized_pca_coeffs_array[train_labels == (class_index-1),class_index,component_index], breaks = 100)
mean(normalized_pca_coeffs_array[train_labels == (class_index-1),class_index,component_index])
var(normalized_pca_coeffs_array[train_labels == (class_index-1),class_index,component_index])

image(1:28, 1:28, matrix(pca_spect_cw[[8]]$vectors[,1], ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")
image(1:28, 1:28, matrix(class_means[6,], ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")
image(1:28, 1:28, matrix(colMeans(train_vectors[train_labels == 5,]), ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")




number_of_components_in_matrix <- 80
principal_coeffs_matrix <- matrix(pca_coeffs_array, nrow = number_of_images, ncol = length(class_names)*number_of_components_in_matrix)
normalized_pca_coeffs_matrix <- matrix(normalized_pca_coeffs_array, nrow = number_of_images, ncol = length(class_names)*number_of_components_in_matrix)
test_pca_coeffs_matrix <- matrix(test_pca_coeffs_array, nrow = length(test_labels), ncol = length(class_names)*number_of_components_in_matrix)
test_normalized_pca_coeffs_matrix <- matrix(test_normalized_pca_coeffs_array, nrow = length(test_labels), ncol = length(class_names)*number_of_components_in_matrix)



npySave("principalcoeffdata/pca_coeffs_matrix.npy", pca_coeffs_matrix_all)
npySave("principalcoeffdata/test_pca_coeffs_matrix.npy", test_pca_coeffs_matrix)
npySave("principalcoeffdata/normalized_pca_coeffs_matrix.npy", normalized_pca_coeffs_matrix)
npySave("principalcoeffdata/test_normalized_pca_coeffs_matrix.npy", test_normalized_pca_coeffs_matrix)
