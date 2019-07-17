install.packages("ggplot2")
install.packages("ggvoronoi")
library(ggvoronoi)
x <- sample(1:100,100)
y <- sample(1:100,100)
points1 <- data.frame(x, y, distance = sqrt((x-100)^2 + (y-100)^2))
ggplot(points1,aes(x,y)) +
  geom_voronoi(aes(fill=distance),color="#4dffb8",size=.125) +
  scale_fill_gradient(low="#4dffb8",high="black",guide=F)+
  theme_void()