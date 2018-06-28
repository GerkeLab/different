library(dplyr)
library(ggplot2)
# library(scico)
library(hexSticker)

set.seed(8675309)
dat <- tibble(
  x = rep(c(1:3, 5:7), each = 4),
  y = rep(1:4, 6),
  color = rep(sample(letters[1:5], 12, replace = TRUE), 2),
  shape = rep("A", 24)
)
dat$shape[19] <- "B"
dat$color[19] <- 12
dat$color[19] <- 'z'

dat <- bind_rows(
  dat,
  data_frame(
    x = c(1:3, 5:7),
    y = 5,
    color = "g",
    shape = "C"
  )
)

colors <- c(
  a = "#143167",
  b = "#1B407D",
  c = "#2E68A7",
  d = "#498BB9",
  e = "#6AB1D0",
  g = "#B3B3B3",
  z = "#C5402D"
)

p <- ggplot(dat) +
  aes(x, y, color = color) +
  geom_point(aes(shape = shape, size = shape)) +
  xlim(.75, 8.75) +
  ylim(0, 5) +
  # scale_color_scico(palette = "roma") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c(16, 18, 15)) +
  scale_size_manual(values = c("A" = 3.5, "B" = 4.25, "C" = 3)) +
  theme_void() +
  theme(legend.position = "none")

sysfonts::font_add("Roboto Slab", "/Library/Fonts/RobotoSlab-Regular.ttf")
outfile <- here::here("man", "figures", "different-hexlogo.png")
if (!dir.exists(dirname(outfile))) dir.create(dirname(outfile), recursive = TRUE)
sticker(
  p,
  package = "different",
  s_x = 1.10,
  s_y = 0.69,
  s_width = 1.4,
  s_height = 1.04,
  p_x = 1,
  p_y = 1.42,
  h_fill = "#E4F2DD",
  h_color = "#498BB9",
  p_color = "#499557",
  p_family = "Roboto Slab",
  filename = outfile
)
