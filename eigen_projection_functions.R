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

get_eigenfunctions <- function(eigen_class_sp, eigen_length){
  return(subsample(eigen_class_sp, 1:eigen_length))
}

eigen_project_sample <- function(sample_spline_projection_coeffs, eigenfunctions, spect, eigen_length){
  sample_eigen_coeffs <- sample_spline_projection_coeffs %*% spect$vectors
  eigen_projection <- lincomb(eigenfunctions, t(sample_eigen_coeffs[1:eigen_length]))
  eigen_projection_info <- list("coeffs" = sample_eigen_coeffs, "proj" = eigen_projection)
  return(eigen_projection_info)
}


get_max_eigen_coeff <- function(sample_spline_projection_coeffs, eigen_class_sp, spect,eigen_length){
  eigenfunctions <- get_eigenfunctions(eigen_class_sp, eigen_length)
  projection_info <- eigen_project_sample(sample_spline_projection_coeffs, eigenfunctions, spect, eigen_length)
  eigen_coeffs <- projection_info$coeffs
  return(max(eigen_coeffs))
}
