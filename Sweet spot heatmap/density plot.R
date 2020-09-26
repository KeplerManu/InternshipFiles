#instructions
# https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2/


install.packages("tidyverse")

# Library
library(tidyverse)
library(RColorBrewer)
library(lattice)
library(ggplot2)

# Data
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data <- rbind(a,b,c)


# Basic scatterplot
ggplot(data, aes(x=x, y=y) ) +
  geom_point()

ggplot(data, aes(x=x, y=y) ) +
  geom_bin2d() +
  theme_bw()

# Number of bins in each direction?
ggplot(data, aes(x=x, y=y) ) +
  geom_bin2d(bins = 70) +
  theme_bw()

ggplot(data, aes(x=x, y=y) ) +
  geom_hex() +
  theme_bw()

# Number of bins in each direction?
ggplot(data, aes(x=x, y=y) ) +
  geom_hex(bins = 70) +
  theme_bw()

# Show the contour only
ggplot(data, aes(x=x, y=y) ) +
  geom_density_2d()

# Show the area only
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")

# Area + contour
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")

# Using raster
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

# Call the palette with a number
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

# The direction argument allows to reverse the palette
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

# You can also call the palette using a name.
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

