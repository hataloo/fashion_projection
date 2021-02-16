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
