#Facial Recognition - Computational Statistics
library(imager)
filename <- 'yalefaces/subject14.gif'
file <- system.file(filename,package='imager')
im <- load.image(file)
