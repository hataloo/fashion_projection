project_class_onto_splines <- function(train_vectors, knots, order,sample_size = dim(train_vectors)[1]){
  indices <- 1:sample_size
  data_values <- cbind(seq(dim(train_vectors)[2]), t(train_vectors[1:sample_size,]))
  class_projection <- project(data_values, knots, order)
  return(class_projection)
}

eigen_decompose_class <- function(class_projection, eigen_projection_size){
  sigma <- cov(class_projection$coeff[1:eigen_projection_size,])
  spect <- eigen(sigma, symmetric = T)
  
  eigen_class_sp <- lincomb(class_projection$basis, t(spect$vectors))
  eigen_coeffs <- class_projection$coeff %*% spect$vectors
  eigen_info <- list("sigma" = sigma, "spect" = spect, "eigen_class_sp" = eigen_class_sp, "coeffs" = eigen_coeffs)
  return(eigen_info)
}

eigen_decompose_class_non_centered <- function(non_centered_class_projection, mean_class_projection, eigen_projection_size, projection_index){
  sigma <- cov(non_centered_class_projection$coeff[1:eigen_projection_size,] - mean_class_projection$coeff[projection_index,])
  spect <- eigen(sigma, symmetric = T)
  
  eigen_class_sp <- lincomb(non_centered_class_projection$basis, t(spect$vectors))
  eigen_coeffs <- non_centered_class_projection$coeff %*% spect$vectors
  eigen_info <- list("sigma" = sigma, "spect" = spect, "eigen_class_sp" = eigen_class_sp, "coeffs" = eigen_coeffs)
  return(eigen_info)
}

get_eigenfunctions <- function(eigen_class_sp, eigen_length){
  return(subsample(eigen_class_sp, 1:eigen_length))
}

eigen_project_sample <- function(sample_spline_projection_coeffs, eigenfunctions, spect, eigen_length){
  sample_eigen_coeffs <- sample_spline_projection_coeffs %*% spect$vectors
  eigen_projection <- lincomb(eigenfunctions, t(sample_eigen_coeffs[1:eigen_length]))
  eigen_projection_info <- list("coeffs" = sample_eigen_coeffs[1:eigen_length], "proj" = eigen_projection)
  return(eigen_projection_info)
}

spline_and_eigen_project_sample <- function(centered_sample_vector, knots, order,eigenfunctions, spect, eigen_length){
  data_values <- cbind(seq(length(centered_sample_vector)), centered_sample_vector)
  spline_sample_projection <- project(data_values, knots, order)
  sample_eigen_coeffs <- spline_sample_projection$coeff %*% spect$vectors
  
  eigen_projection <- lincomb(eigenfunctions, t(sample_eigen_coeffs[1:eigen_length]))
  eigen_projection_info <- list("coeffs" = sample_eigen_coeffs[1:eigen_length], "proj" = eigen_projection)
  return(eigen_projection_info)
}

spline_and_eigen_project_samples <- function(centered_sample_vectors, knots, order, eigenfunctions, spect, eigen_length){
  data_values <- cbind(seq(dim(centered_sample_vectors)[2]), t(centered_sample_vectors))
  splines_samples_projections <- project(data_values, knots, order)
  sample_eigen_coeffs <- splines_samples_projections$coeff %*% spect$vectors
  eigen_projections <- lincomb(eigenfunctions, (sample_eigen_coeffs[,1:eigen_length]))
  eigen_projection_info <- list("coeffs" = sample_eigen_coeffs[,1:eigen_length], "proj" = eigen_projections)
  return(eigen_projection_info)
}

eigen_project_samples_from_coeffs <- function(centered_sample_coeffs, spect, eigenfunctions ,eigen_length){
  sample_eigen_coeffs <- centered_sample_coeffs %*% spect$vectors
  eigen_projections <- lincomb(eigenfunctions, (sample_eigen_coeffs[,1:eigen_length]))
  eigen_projection_info <- list("coeffs" = sample_eigen_coeffs[,1:eigen_length], "proj" = eigen_projections)
  return(eigen_projection_info)
}


get_max_eigen_coeff <- function(sample_spline_projection_coeffs, eigen_class_sp, spect,eigen_length){
  eigenfunctions <- get_eigenfunctions(eigen_class_sp, eigen_length)
  projection_info <- eigen_project_sample(sample_spline_projection_coeffs, eigenfunctions, spect, eigen_length)
  eigen_coeffs <- projection_info$coeffs
  return(max(eigen_coeffs))
}
