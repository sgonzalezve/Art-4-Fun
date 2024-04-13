Rcpp::sourceCpp("R/image-rcpp.cpp")
base_data <- function(seed, rows, cols, iterations, span) {
  set.seed(seed)
  tidyr::expand_grid(
    x = seq(0, 1, length.out = cols),
    y = seq(0, 1, length.out = rows),
    z = 1,
    iter = 0
  ) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      value = t(automaton(rows, cols, iterations, span)) |>
        as.vector() |>
        ambient::normalise()
    )
}
base <- base_data(
  seed = 789,
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



curl_step <- function(data,
                      iteration,
                      scale,
                      octaves,
                      seed) {

  noise_data <- ambient::curl_noise(
    x = data$x,
    y = data$y,
    z = data$z,
    seed = seed,
    generator = ambient::fracture,
    noise = ambient::gen_simplex,
    fractal = ambient::ridged,
    octaves = octaves
  )
  data$iter <- iteration
  data$x <- data$x + noise_data$x * scale
  data$y <- data$y + noise_data$y * scale
  data$z <- data$z + noise_data$z * scale
  data
}


curl_loop <- function(data,
                      seed,
                      iterations,
                      scale,
                      octaves) {
  states <- purrr::accumulate(
    .x = 1:iterations,
    .f = curl_step,
    .init = data,
    scale = scale,
    octaves = octaves,
    seed = seed
  )
  dplyr::bind_rows(states)
}


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



sample_palette <- function(seed, size) {
  set.seed(seed)
  cols <- c(
    "#de9151", "#f34213", "#2e2e3a", "#bc5d2e", "#bbb8b2",
    "#a63446", "#fbfef9", "#0c6291", "#000004", "#7e1946",
    "#ffffff", "#ffcad4", "#b0d0d3", "#c08497", "#f7af9d",
    "#aa8f66", "#ed9b40", "#ffeedb", "#61c9a8", "#ba3b46",
    "#241023", "#6b0504", "#a3320b", "#d5e68d", "#47a025",
    "#64113f", "#de4d86", "#f29ca3", "#f7cacd", "#84e6f8",
    "#660000", "#990033", "#5f021f", "#8c001a", "#ff9000",
    "#c9cba3", "#ffe1a8", "#e26d5c", "#723d46", "#472d30",
    "#0e7c7b", "#17bebb", "#d4f4dd", "#d62246", "#4b1d3f",
    "#0a0908", "#49111c", "#f2f4f3", "#a9927d", "#5e503f",
    "#020202", "#0d324d", "#7f5a83", "#a188a6", "#9da2ab",
    "#c2c1c2", "#42213d", "#683257", "#bd4089", "#f51aa4",
    "#820263", "#d90368", "#eadeda", "#2e294e", "#ffd400",
    "#f4e409", "#eeba0b", "#c36f09", "#a63c06", "#710000",
    "#d9d0de", "#bc8da0", "#a04668", "#ab4967", "#0c1713",
    "#012622", "#003b36", "#ece5f0", "#e98a15", "#59114d",
    "#3c1518", "#69140e", "#a44200", "#d58936", "#fffb46",
    "#6e0d25", "#ffffb3", "#dcab6b", "#774e24", "#6a381f",
    "#bcabae", "#0f0f0f", "#2d2e2e", "#716969", "#fbfbfb",
    "#2b4162", "#385f71", "#f5f0f6", "#d7b377", "#8f754f"
  )
  sample(cols, size = size)
}


scales::show_col(sample_palette(seed = 100, size = 4))
scales::show_col(sample_palette(seed = 123, size = 4))
scales::show_col(sample_palette(seed = 666, size = 4))


make_plot <- function(data, seed) {
  palette <- sample_palette(seed = seed, size = 4)
  data |>
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
    ggplot2::scale_colour_gradientn(colours = palette) +
    ggplot2::theme_void()
}

make_plot(flow, seed = 007)


splatter <- function(seed) {
  stopifnot(length(seed) %in% c(1, 3))
  if(length(seed) == 1) seed <- rep(seed, 3)
  base_data(
    seed = seed[1],
    rows = 100,
    cols = 100,
    iterations = 400000,
    span = 5
  ) |>
    curl_loop(
      seed = seed[2],
      iterations =20,
      scale = .0002,
      octaves = 5
    ) |>
    make_plot(seed = seed[3])
}



splatter(seed = c(101, 102, 100))
splatter(seed = c(101, 102, 123))
splatter(seed = c(101, 102, 666))
