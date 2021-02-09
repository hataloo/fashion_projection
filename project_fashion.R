#Have to run the following commands prior:
#Please note that this will install the Python
#package onto your computer.
#I only use tensorflow and keras to access the
#fashion MNIST dataset. There are most likely
#alternative ways to load this dataset into R,
#however I found this to be an easy solution.
#install.packages("tensorflow")
#library(tensorflow)
#install_tensorflow()
#To test:
#tf$constant("Hello Tensorflow")


library(keras)
library(Splinets)

#Code below builds uses Tensorflow tutorial found at:
#https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_classification/


#Will download approx 200MB
fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train

class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

number_of_images <- dim(train_labels)
grid_size <- 4 #Use even numbers preferably.
number_of_images_to_show <- grid_size^2/2
indices <- sample.int(number_of_images,number_of_images_to_show)

par(mfcol = c(grid_size,grid_size))
par(mar = c(0.7, 0.3, 1.5, 0), xaxs = "i", yaxs = "i")
imgs <- list()

#Plots grid of clothing and their corresponding pixel values as a regular plot.
for (i in 1:(number_of_images_to_show)){
  img <- train_images[indices[i],,] / 255
  img <- t(apply(img, 2, rev))
  
  if (i == 1){imgs = list(img)} 
  else{imgs[[i]] <- img}
  
  name <- paste(class_names[train_labels[indices[i]]+1])
  image(1:28, 1:28, img, col = gray((0:255/(255))), xaxt = "n", yaxt = "n",
        main = name)
  plot(as.vector(img), type = "l", main = name, xaxt = "n", yaxt = "n")
}

#Plots same pixel value plots as above but wider.
par(mfcol = c(grid_size,1))
for (i in 1:(grid_size)) {
  name <- paste(class_names[train_labels[indices[i]]+1])
  img_flattened <- as.vector(imgs[[i]])
  plot(img_flattened, type = "l", main = name, xaxt = "n", yaxt = "n", ylab = "")
}

#Projects the pixel values onto a Spline on uniform knots.
knots <- seq(from = -10, to = 28^2+10, length.out = 28*2)
order = 4
for (i in 1:(grid_size)){
  name <- paste(class_names[train_labels[indices[i]]+1])
  data_values <- cbind(seq(28^2),as.vector(imgs[[i]]))
  imgproj <- project(data_values, knots, order)
  name <- paste(class_names[train_labels[indices[i]]+1])
  plot(imgproj$sp, type = "l", xaxt = "n", yaxt = "n", main = name)
}

#Shows the clothing item, a plot of its pixel values and a projection onto
#a spline.
layout(matrix(c(1,2,3,3,4,4), nrow = 3, ncol = 2, byrow = TRUE))
for (i in 1:(grid_size/2)){
  img = imgs[[i]]
  img_flattened <- as.vector(img)
  name <- paste(class_names[train_labels[indices[i]]+1])
  image(1:28, 1:28, img, col = gray((0:255/(255))), xaxt = "n", yaxt = "n",
        main = name)
  image(1:28, 1:28, img, col = gray((0:255/(255))), xaxt = "n", yaxt = "n",
        main = name)
  plot(img_flattened, type = "l", main = name, xaxt = "n", yaxt = "n", ylab = "")
  imgproj <- project(data_values, knots, order)
  plot(imgproj$sp, type = "l", xaxt = "n", yaxt = "n", main = name)
}

examine_new_sample <- function(knots, order, train_images, class_names){
  
  i = sample.int(dim(train_images)[1],1)
  layout(matrix(c(1,2,3,3,4,4), nrow = 3, ncol = 2, byrow = TRUE))
  img <- train_images[i,,] / 255
  img <- t(apply(img, 2, rev))
  img_flattened <- as.vector(img)
  name <- paste(class_names[train_labels[i]+1])
  image(1:28, 1:28, img, col = gray((0:255/(255))), xaxt = "n", yaxt = "n",
        main = name)
  image(1:28, 1:28, img, col = gray((0:255/(255))), xaxt = "n", yaxt = "n",
        main = name)
  plot(img_flattened, type = "l", main = name, xaxt = "n", yaxt = "n", ylab = "")
  data_values <- cbind(seq(length(img)),as.vector(img))
  imgproj <- project(data_values, knots, order)
  plot(imgproj$sp, type = "l", xaxt = "n", yaxt = "n", main = name)
}

