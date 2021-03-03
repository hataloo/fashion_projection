source("data_preparation_functions.R")
source("eigen_projection_functions.R")
library(pbapply)
library(keras)
#Run eigen_projection_preparation before running this.

eigen_length <- 5


train_eigen_coeffs_by_class <- array(dim = c(length(class_names),sample_size, length(class_names), eigen_length))
#The dimensions are: [class_index, sample_index, projection_index, eigen_vector_index]
pb <- timerProgressBar(min = 1, max = length(class_names)^2, style = 3)

for (class_index in 1:length(class_names)){
  for (projection_index in 1:length(class_names)){
    centered_sample_coeffs <- non_centered_class_projection[[class_index]]$coeff - 
                              mean_projection$coeff[projection_index,]
    eigenfunctions <- get_eigenfunctions(eigen_class_sp[[projection_index]], eigen_length)
    eigen_info <- eigen_project_samples_from_coeffs(centered_sample_coeffs, spect[[projection_index]],eigenfunctions, eigen_length)
    train_eigen_coeffs_by_class[class_index, ,projection_index, ] <- eigen_info$coeffs
    setTimerProgressBar(pb, getTimerProgressBar(pb)+1)
  }
}

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 32, input_shape = c(length(class_names)*eigen_length)) %>%
  layer_activation('relu') %>%
  layer_dense(units = length(class_names)) %>%
  layer_activation('softmax')

model %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

data <- array(train_eigen_coeffs_by_class, dim = c(sample_size*length(class_names), eigen_length*length(class_names)))
data_labels <- matrix(nrow = sample_size*length(class_names),ncol =  1)
for(i in 1:length(class_names)){
  data_labels[((i-1)*sample_size+1):((i)*sample_size)] <- i-1
}
one_hot_labels <- to_categorical(data_labels, num_classes = 10)

model %>% fit(data, one_hot_labels, epochs = 10, batch_size = 32)
