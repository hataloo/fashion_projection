separate_data_by_class <- function(train_images, train_labels, number_of_classes){
  train_images_by_class <- list()
  for (i in 0:(number_of_classes-1)){
    images <- train_images[train_labels == i,,]/255
    for (j in 1:(dim(images)[1])){
      images[j,,] <- t(apply(images[j,,],2,rev))
    }
    train_images_by_class[[i+1]] <- images
  }
  return(train_images_by_class)
}

class_images_into_vectors <- function(train_images_by_class){
  train_vectors_by_class <- list()
  for (i in 0:(length(train_images_by_class)-1)){
    images <- train_images_by_class[[i+1]]
    train_vectors_by_class[[i+1]] <- matrix(images, dim(images)[1], prod(dim(images)[2:3]))
  }
  return(train_vectors_by_class)
}

center_images <- function(train_images_by_class, class_image_means){
  train_images_by_class_centered <- list()
  for(class_index in 1:length(train_images_by_class)){
    train_images_by_class_centered[[class_index]] <- apply(train_images_by_class[[class_index]], c(1),
                                                                                  function(x) x - class_image_means[[class_index]])
    train_images_by_class_centered[[class_index]] <- t(train_images_by_class_centered[[class_index]])
    dim(train_images_by_class_centered[[class_index]]) <- c(dim(train_images_by_class[[class_index]])[1], dim(class_image_means[[class_index]]))
  }
  return(train_images_by_class_centered)
}

center_vectors <- function(train_vectors_by_class, class_vector_means){
  train_vectors_by_class_centered <- list()
  for(class_index in 1:length(train_images_by_class)){
    train_vectors_by_class_centered[[class_index]] <- apply(train_vectors_by_class[[class_index]], c(1),
                                                                                  function(x) x - class_image_means[[class_index]])
    train_vectors_by_class_centered[[class_index]] <- t(train_vectors_by_class_centered[[class_index]])
  }
  return(train_vectors_by_class_centered)
}

center_vectors_around_all <- function(train_vectors_by_class, class_vector_means){
  train_vectors_by_class_centered <- list()
  for(class_index in 1:length(train_images_by_class)){
    inner_list <- list()
    for(projection_index in 1:length(class_vector_means)){
      inner_list[[projection_index]] <- apply(train_vectors_by_class[[class_index]], c(1),
                                                              function(x) x - class_image_means[[projection_index]])
      inner_list[[projection_index]] <- t(train_vectors_by_class_centered[[projection_index]])
    }
    train_vectors_by_class_centered[[class_index]] <- inner_list
  }
  return(train_vectors_by_class_centered)
}