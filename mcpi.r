
library(tidyverse)
library(gganimate)


# monte carlo simulation of pi ------------------------------------------------

# number of points
n <- 10000

# run simulation
pts <- 
  data.frame(
    frame = 1:n,
    x = runif(n, min = -1, max = 1),
    y = runif(n, min = -1, max = 1)) %>%
  mutate(
    h = sqrt(x ^ 2 + y ^ 2),
    inside = h <= 1,
    inside2 = case_when(inside ~ 1, T ~ 0),
    nInside = cumsum(inside2),
    ratio = format(4 * nInside / frame, digits = 3),
    desc = paste0('Point: ', frame, '\nIn:', nInside, '\nOut: ', (frame - nInside), '\nRatio: ', ratio))

ratio <- 4 * nrow(filter(pts, inside)) / n
ratio

# plot results
plt <- 
  pts %>%
  ggplot(aes(x = x, y = y, color = inside, fill = inside)) +
  geom_point(size = 1, alpha = 1, show.legend = F) +
  annotate("path",
    x = cos(seq(0, 2 * pi, length.out = 100)),
    y = sin(seq(0, 2 * pi, length.out = 100))) +
  coord_fixed() +
  labs(
    title = paste0("Monte Carlo simulation of pi (n = ", format(n, big.mark = ",", scientific = F), ")"),
    subtitle = paste("Ratio of inside/outside =", format(ratio, digits = 10)),
    x = NULL, y = NULL)
plt




# r normal distribution for fun ----

n <- 10000

pts_norm <- 
  data.frame(
    x = rnorm(n, mean = 0, sd = 1),
    y = rnorm(n, mean = 0, sd = 1)) %>%
  mutate(h = sqrt(x ^ 2 + y ^ 2)) %>%
  mutate(inside = h <= 1)

ratio_norm <- 4 * nrow(filter(pts2, inside)) / n
ratio_norm

plt_norm <- 
  pts_norm %>%
  ggplot(aes(x = x, y = y, color = inside, fill = inside)) +
  geom_point(size = 1, alpha = .5, show.legend = F) +
  annotate("path",
           x = cos(seq(0, 2 * pi, length.out = 100)),
           y = sin(seq(0, 2 * pi, length.out = 100))) +
  coord_fixed() +
  labs(
    title = paste("Monte Carlo simulation using rnorm, n =", format(n, big.mark = ",", scientific = F)),
    subtitle = paste("Ratio of inside/outside =", format(ratio2, digits = 10)))
plt_norm



# make animation ----------------------------------------------------------

# make animation (slow!)
plt_norm <- 
  pts %>%
  ggplot(aes(x, y, color = inside, fill = inside)) +
  annotate(
    "path",
    x = cos(seq(0, 2 * pi, length.out = 100)),
    y = sin(seq(0, 2 * pi, length.out = 100))) +
  geom_point(size = 2, alpha = 1, show.legend = F) +
  geom_text(aes(x = 0, y = 0, label = desc), inherit.aes = F) +
  coord_fixed(clip = 'off') +
  labs(
    title = paste0("Monte Carlo simulation of pi (n = ", format(n, big.mark = ",", scientific = F), ")"),
    x = NULL,  y = NULL) +
  transition_states(frame) +
  enter_appear() +
  shadow_mark(exclude_layer = 3)



