library(ggplot2)
library(reshape2)

class_image_means <- list()
class_vector_means <- list()
for(class_index in 1:length(class_names)){
  class_image_means[[class_index]] <- apply(train_images_by_class[[class_index]], c(2,3), mean)
  class_vector_means[[class_index]] <- apply(train_vectors_by_class[[class_index]], 2, mean)

  imagepath <- sprintf("./plots/data_visualisation/mean/%s_image_mean.pdf",gsub("/","-",class_names[class_index]),sample_index)
  vectorpath <- sprintf("./plots/data_visualisation/mean/%s_vector_mean.pdf",gsub("/","-",class_names[class_index]),sample_index)
  pdf(imagepath, width = 3, height = 3)
  image(1:28, 1:28, class_image_means[[class_index]], col = gray((0:255/(255))), xaxt = "n", yaxt = "n", xlab = "", ylab = "", asp = 1, bty = "n")
  dev.off()
  p <- ggplot(data.frame(pos = seq(image_size^2), value = class_vector_means[[class_index]]),
              aes(x = pos, y= value)) + geom_area() + 
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  show(p)
  ggsave(vectorpath, width = 18, height = 3)
}

class_vector_means_df <- as.data.frame(do.call(cbind, class_vector_means))
class_vector_means_df <- data.frame(Position = seq(28^2), values = class_vector_means)
colnames(class_vector_means_df)[2:11] <- class_names
class_vector_means_df <- melt(class_vector_means_df, id.vars = "Position", value.name = "Value", variable.name = "Class")

p <- ggplot(data = class_vector_means_df,
            aes(x = Position, y = class_names)) + geom_line()
show(p)


d <- data.frame(y = unlist(class_vector_means),
                x = seq(28^2))
ggplot(d, aes(x = x, y = y)) + geom_line()

class_vector_named <- list()
for(i in 1:3){
  class_vector_named[[class_names[i]]] <- class_vector_means[[i]]
}

dat <- class_vector_named
dat <- lapply(dat, function(x) cbind(x = seq_along(x), y = x))

list.names <- names(dat)
lns <- sapply(dat, nrow)
dat <- as.data.frame(do.call("rbind",dat))
dat$Class <- rep(list.names, lns)

ggplot(dat, aes(x = x,y = y, colour = Class)) + geom_area()
