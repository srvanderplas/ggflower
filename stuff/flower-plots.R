library(tidyverse)
#Have the user create or use a tidy data set that will have the

dframe <- data.frame(
  expand.grid(
  x = seq(0, 2*pi, by = 0.01),
  class = 1:8
  )
)

dframe$y = dnorm(dframe$x, mean = (dframe$class-.5)*pi/4, sd = pi/12) #Restrict the values to be between 0 and 2pi range here
dframe$y = dframe$y * ((dframe$class != 5) * 0.5 + 0.5) #This will scale the petal size for the different sizes.

dframe %>% ggplot(aes(x = x)) +
  geom_line(aes(y = y, colour = factor(class)))

dframe %>% ggplot(aes(x = x)) +
  geom_line(aes(y = y, colour = factor(class))) +
  coord_polar() +
  theme_bw()

dframe %>% ggplot(aes(x = x)) +
  geom_polygon(aes(y = y, fill = factor(class)), alpha = 0.5) +
  geom_line(aes(y = y, colour = factor(class))) +
  coord_polar() +
  theme_bw()
