library(tidyverse)
library(ggforce)
set.seed(123456)

n <- 25

data <- expand_grid(x = seq_len(n), y = seq_len(n)) %>%
  mutate(x = x - as.numeric(y %% 2 == 0) / 2,
         y = y - as.numeric(x %% 2 == 0) / 2)


data %>%
  mutate(center = sqrt((x-(n/2+0.25))^2 +(y-(n/2+0.375))^2),
         center = center + rnorm(n(), sd = 3)) %>%
  ggplot(aes(x, y, fill = center)) +
  geom_voronoi_tile() +
  geom_voronoi_segment() +
  coord_fixed() +
  scale_fill_gradientn(colors = hcl.colors(256, "Temps")) +
  theme_void()

##ggsave("filename", height = 10, width = 10, dpi = 320)
