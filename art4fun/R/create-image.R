
# Out-Sourcing ------------------------------------------------------------
Rcpp::sourceCpp("R/image-rcpp.cpp")
source("R/utils_fun.R")

# Generative Image Maker -------------------------------------------------------------

base <- base_data(
  seed = 007,
  rows = 100,
  cols = 100,
  iterations = 400000,
  span = 5
)

base
base |>
  ggplot2::ggplot(ggplot2::aes(
    x = x,
    y = y,
    fill = value
  )) +
  ggplot2::geom_raster(show.legend = FALSE) +
  ggplot2::coord_equal() +
  ggplot2::theme_void() +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0))

flow <- base |>
  curl_loop(
    seed = 100,
    iterations = 20,
    scale = .0002,
    octaves = 5
  )

flow


pic <- flow |>
  ggplot2::ggplot(ggplot2::aes(
    x = x,
    y = y,
    colour = value,
    size = -iter
  )) +
  ggplot2::geom_point(
    stroke = 0,
    show.legend = FALSE
  ) +
  ggplot2::coord_equal(
    xlim = c(.05, .95),
    ylim = c(.05, .95)
  ) +
  ggplot2::scale_size(range = c(0, 6)) +
  ggplot2::theme_void()

pic


#pic + gganimate::transition_time(iter)

flow |>
  dplyr::filter(iter < 2) |>
  ggplot2::ggplot(ggplot2::aes(
    x = x,
    y = y,
    group = id
  )) +
  ggplot2::geom_path(
    arrow = grid::arrow(
      length = grid::unit(.008, "npc"),
      ends = "last",
      type = "open"
    )
  ) +
  ggplot2::coord_equal(
    xlim = c(.2, .5),
    ylim = c(.2, .5)
  ) +
  ggplot2::theme_void()


scales::show_col(sample_palette(seed = 100, size = 4))
scales::show_col(sample_palette(seed = 123, size = 4))
scales::show_col(sample_palette(seed = 666, size = 4))

make_plot(flow, seed = 007)


splatter(seed = c(101, 102, 100))
splatter(seed = c(101, 102, 123))
splatter(seed = c(101, 102, 666))
