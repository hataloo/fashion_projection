
#Run data_preparation.R and eigen_projection_preparation.R prior to running this file.

sample_class <- match("Sneaker", class_names)
sample_index <- 10

projection_class <- match("Sneaker", class_names)

eigen_length <- c(3,10,25,60)

sample_eigen_visualization <- function(sample_class, sample_index, projection_class, eigen_length, info){
  train_images_by_class <- info[["train_images"]]
  class_names <- info[["class_names"]]
  train_vectors_by_class <- info[["train_vectors"]]
  eigen_class_sp <- info[["eigen_class_sp"]]
  class_projection <- info[["class_projection"]]
  
  
  
  plotpath <- sprintf("./plots/class_projections/%sOnto%s_%d.pdf",class_names[sample_class],class_names[projection_class],sample_index)
  
  layoutmat <- matrix(c(1,1,2,3), nrow = 2,ncol = 2, byrow = TRUE)
  for(i in 1:(length(eigen_length)+1)){layoutmat <- rbind(layoutmat, c(i+3,i+3))}
  layout(layoutmat, heights = c(1, rep(1,each = 1+length(eigen_length))))
  
  par(mar = c(0,0,0,0))
  plot.new()
  text(0.5,0.5,sprintf("%s projected onto %s", class_names[sample_class], class_names[projection_class]), cex = 2)
  par(mar = c(3,1,3,1))
  
  image(1:28, 1:28, train_images_by_class[[sample_class]][sample_index,,], col = gray((0:255/(255))), xaxt = "n", yaxt = "n", main = class_names[sample_class])
  image(1:28, 1:28, train_images_by_class[[projection_class]][sample_index,,], col = gray((0:255/(255))), xaxt = "n", yaxt = "n", main = class_names[projection_class])
  
  plot(seq(image_size^2), train_vectors_by_class[[sample_class]][sample_index,], type = "l", ylab = "", xlab = "", main = sprintf("Original data for %s",class_names[sample_class] ))
  par(mar = c(3,1,3,1))
  for (j in 1:length(eigen_length)){
    eigenfunctions <- get_eigenfunctions(eigen_class_sp[[j]], eigen_length[j])
    eigen_projection_info <- eigen_project_sample(class_projection[[sample_class]]$coeff[sample_index,], eigenfunctions, spect[[j]], eigen_length[j])
    plot(eigen_projection_info[["proj"]], type = "l",
         main = sprintf("Projected onto %s with %d eigenvectors", 
                        class_names[projection_class], eigen_length[j]))
  }
}

sample_eigen_visualization(sample_class, 15, projection_class, eigen_length, 
                           info = list("train_images" = train_images_by_class, 
                                      "class_names" = class_names,
                                      "eigen_class_sp" = eigen_class_sp,
                                      "train_vectors" = train_vectors_by_class,
                                      "class_projection" = class_projection))

max_eigen_coeff <- vector(mode = "list", length(class_names))
for (class_index in 1:length(class_names)){
  max_eigen_coeff[[class_index]] <- get_max_eigen_coeff(class_projection[[sample_class]]$coeff[sample_index,], eigen_class_sp[[class_index]], spect[[class_index]],
                                                         eigen_length = 30)
}
max_class_index <- which.max(max_eigen_coeff)
max_class <- class_names[max_class_index]
cat(sprintf("Predicted class: %s\nActual class: %s",max_class, class_names[sample_class]))
