##Need to run data_preparation before.

###PCA centered around entire dataset

number_of_components_to_save <- 100
pca_coeffs_matrix_all <- array(dim = c(number_of_images, number_of_components_to_save))
normalized_pca_coeffs_matrix_all <- array(dim = c(number_of_images, number_of_components_to_save))

test_pca_coeffs_matrix_all <- array(dim = c(length(test_labels), number_of_components_to_save))
test_normalized_pca_coeffs_matrix_all <- array(dim = c(length(test_labels), number_of_components_to_save))

dataset_mean <- colMeans(train_vectors)
pca_sigma_all <- cov(sweep(train_vectors,2, dataset_mean))
pca_spect_all <- eigen(pca_sigma_all, symmetric = T)

pca_coeffs_matrix_all <- ((sweep(train_vectors,2,dataset_mean)) %*% pca_spect_all$vectors)[,1:number_of_components_to_save]
test_pca_coeffs_matrix_all <- ((sweep(test_vectors,2,dataset_mean)) %*% pca_spect_all$vectors)[,1:number_of_components_to_save]
for(component_index in 1:number_of_components_to_save){
  normalized_pca_coeffs_matrix_all[,component_index] <- pca_coeffs_matrix_all[,component_index]/sqrt(pca_spect_all$values[component_index])
  test_normalized_pca_coeffs_matrix_all[,component_index] <- test_pca_coeffs_matrix_all[,component_index]/sqrt(pca_spect_all$values[component_index])
}
npySave("principalcoeffdata/pca_coeffs_matrix_entire_dataset.npy", pca_coeffs_matrix_all)
npySave("principalcoeffdata/test_pca_coeffs_matrix_entire_dataset.npy", test_pca_coeffs_matrix_all)
npySave("principalcoeffdata/normalized_pca_coeffs_matrix_entire_dataset.npy", normalized_pca_coeffs_matrix_all)
npySave("principalcoeffdata/test_normalized_pca_coeffs_matrix_entire_dataset.npy", test_normalized_pca_coeffs_matrix_all)
