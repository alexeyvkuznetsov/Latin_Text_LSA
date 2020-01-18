install.packages("scatterplot3d") # Install
library("scatterplot3d") # load

data(iris)
head(iris)

#scatterplot3d(x, y=NULL, z=NULL)

scatterplot3d(iris[,1:3])


scatterplot3d(iris[,1:3], angle = 55)
