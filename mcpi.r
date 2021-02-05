
library(tidyverse)
library(gifski)
library(gganimate)


# monte carlo simulation of pi ------------------------------------------------

# number of points
n <- 10000

# run simulation
pts <- 
  tibble(
    point = 1:n,
    x = runif(n, min = -1, max = 1),
    y = runif(n, min = -1, max = 1)) %>%
  mutate(
    h = sqrt(x ^ 2 + y ^ 2),
    inside = (h <= 1),
    n_inside = cumsum(inside),
    ratio = format(4 * n_inside / point, digits = 4))

ratio <- 4 * nrow(filter(pts, inside)) / n

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



# r normal distribution for fun ------------------------------------------------------------------

n_norm <- 10000

pts_norm <- 
  tibble(
    point = 1:n_norm,
    x = rnorm(n_norm, mean = 0, sd = 1),
    y = rnorm(n_norm, mean = 0, sd = 1)) %>%
  mutate(
    h = sqrt(x ^ 2 + y ^ 2),
    inside = (h <= 1),
    n_inside = cumsum(inside),
    ratio = format(4 * n_inside / point, digits = 4))

ratio_norm <- 4 * nrow(filter(pts_norm, inside)) / n

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
    subtitle = paste("Ratio of inside/outside =", format(ratio_norm, digits = 10)))
plt_norm



# make animation ----------------------------------------------------------

# make animation (slow!)
{
  n_pts <- 1000
  n_frames <- 100
  
  tibble(
    point = 1:n_pts,
    x = runif(n_pts, min = -1, max = 1),
    y = runif(n_pts, min = -1, max = 1)) %>%
  mutate(
    frame = floor(point / (n_pts / n_frames)),
    h = sqrt(x ^ 2 + y ^ 2),
    inside = (h <= 1),
    n_inside = cumsum(inside),
    ratio = format(4 * n_inside / point, digits = 4)) %>%
  mutate(
    desc = ifelse(frame != lead(frame),
      paste0('\nIn:', n_inside, '\nOut: ', (point - n_inside), '\nRatio: ', ratio),
      "")) %>%
  ggplot(aes(x, y, color = inside, fill = inside)) +
  annotate("path",
    x = cos(seq(0, 2 * pi, length.out = 100)),
    y = sin(seq(0, 2 * pi, length.out = 100))) +
  geom_point(size = 2, alpha = 1, show.legend = F) +
  geom_text(aes(x = 0, y = 0, label = desc), inherit.aes = F) +
  coord_fixed(clip = 'off') +
  labs(
    title = paste0("Monte Carlo simulation of pi (n = ", format(n_pts, big.mark = ",", scientific = F), ")"),
    x = NULL,  y = NULL) +
  transition_states(frame) +
  enter_appear() +
  shadow_mark(exclude_layer = 3)
  } %>%
  animate(nframes = n_frames)

anim_save("anim.gif")
