#' @importFrom stats dnorm
petal <- function(x, y, shape = "heart") {
  # ignore shape for right now, but we could implement other shapes later
  x0 <- seq(-pi, pi, by = 0.1)
  petal_shape <- data.frame(x0 = x0)
  if (shape == "box") {
    petal_shape <- data.frame(
      x0 = c(0, 0, 1, 1) - 0.5,
      y0 <- c(0, 1, 1, 0)
    )
  }
  if (shape == "normal") {
    petal_shape$y0 <- dnorm(petal_shape$x0)
  }
  if (shape == "cos") {
    petal_shape$y0 <- cos(petal_shape$x0)
  }
  if (shape == "circle") {
    petal_shape$y0 <- 5 + sqrt(1 - (petal_shape$x0 / pi)^2)
    petal_shape$y0[1] <- 0
    petal_shape$y0[length(petal_shape$y0)] <- 0
  }
  if (shape == "heart") {
    petal_shape$y0 <- 5 + abs(3 * sin(petal_shape$x0))
    petal_shape$y0[1] <- 0
    petal_shape$y0[length(petal_shape$y0)] <- 0
  }

  # scale the shape to be between ± 0.5 horizontally and [0,1] height
  petal_shape$x0 <- petal_shape$x0 / diff(range(petal_shape$x0))
  petal_shape$y0 <- (petal_shape$y0 - min(petal_shape$y0)) / (max(petal_shape$y0) - min(petal_shape$y0))

  area <- sum(petal_shape$y0) / nrow(petal_shape)
  area2 <- sum(petal_shape$y0^2) / nrow(petal_shape)
  petal_shape$y0 <- 2 * petal_shape$y0 / area2

  stopifnot(length(x) == 1, length(y) == 1)
  petal_shape <- petal_shape %>%
    dplyr::mutate(
      x = petal_shape$x0 + x,
      y = petal_shape$y0 * sqrt(y)
    ) %>%
    dplyr::select(-x0, -y0)
  rbind(
    petal_shape,
    petal_shape %>% mutate(
      x = rev(x),
      y = 0
    )
  )
}

#' Create the flower geom
#'

#' @param shape shape of the petal, one of "normal", "cos", "circle", "heart"
#' @param disk.size Size of the flower center
#' @param disk.color Color of the flower center
#' @param disk.colour See disk.color
#' @param na.rm If `FALSE` (the default), removes missing values with a warning.
#'              If `TRUE` silently removes missing values.
#' @param ... other arguments passed on to `layer`. These are often aesthetics,
#'            used to set an aesthetic to a fixed value, like `color = 'red'`
#'            or `size = 3`. They may also be parameters to the paired geom/stat.
#' @importFrom dplyr group_by summarise mutate left_join select filter arrange
#' @importFrom ggplot2 layer
#' @inheritParams ggplot2::layer
#' @export
#' @return a list consisting of a [ggplot2::layer()] object and its associated scales.
#' @examples
#' n <- 8
#' dframe <- data.frame(index = 1:n, y = n:1)
#' # scaling is not correct for cartesian coordinates
#' # squaring the y values ensures that heights are linear in y
#' dframe |>
#'   ggplot() +
#'   geom_flower(aes(x = index, y = y^2, fill = factor(index)))
#' # use polar coordinates
#' dframe |>
#'   ggplot(aes(x = index, y = y)) +
#'   geom_flower(aes(fill = factor(index)),
#'     shape = "cos", colour = NA, alpha = 0.8
#'   ) +
#'   geom_flower(aes(x = 1:8, y = 2), linewidth = 0.125, fill = NA,
#'               colour = "lightyellow", shape = "cos") +
#'   geom_flower(aes(x = 1:8, y = 4), linewidth = 0.25, fill = NA,
#'               colour = "lightyellow", shape = "cos") +
#'   geom_flower(aes(x = 1:8, y = 6), linewidth = 0.125, fill = NA,
#'               colour = "lightyellow", shape = "cos") +
#'   # annotate("point", x = 1, y = -3, color="lightyellow", size=20) +
#'   geom_flower(aes(x = 1:8, y = 0), linewidth = 0.25, fill = NA,
#'               colour = "yellow", shape = "cos") +
#'   coord_polar() +
#'   theme_void() +
#'   theme(legend.position = "bottom")
#' dframe |>
#'   ggplot(aes(x = index, y = y)) +
#'   geom_flower(aes(fill = factor(index)), shape = "circle") +
#'   coord_polar()
geom_flower <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = F, show.legend = NA,
                        inherit.aes = T,
                        disk.size = 1, disk.colour = "lightyellow",
                        disk.color = disk.colour,
                        shape = "normal", ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlower,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      shape = shape,
      disk.size = disk.size,
      disk.colour = disk.colour,
      ...
    )
  )
}

#' @rdname geom_flower
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom aes
#' @importFrom tidyr unnest
#' @importFrom purrr map2
#' @export
GeomFlower <- ggproto(
  "GeomFlower", Geom,
  # Set default aesthetics
  default_aes = ggplot2::aes(
    color = "grey50", fill = "grey70", linewidth = 0.5,
    linetype = "solid", alpha = NA, subgroup = NULL
    # min_height = 0, scale = 1, alpha = NA, datatype = "ridgeline",
    #
    # # point aesthetics with default
    # point_shape = 19, point_size = 1.5, point_stroke = 0.5,
    #
    # # point aesthetics, inherited
    # point_colour = NULL,# point_color = NULL,
    # point_fill = NULL, point_alpha = NULL,
    #
    # # vline aesthetics, all inherited
    # vline_colour = NULL, #vline_color = NULL,
    # vline_size = NULL, vline_linetype = NULL
  ),
  required_aes = c("x", "y"),
  optional_aes = c("fill", "color"),
  extra_params = c("na.rm", "shape", "disk.size", "disk.colour"),
  param_values = NA,
  setup_data = function(self, data, params) {
    # This is where the computations are done
    # set up columns in your data that map to what
    # you're actually going to plot
    # goal is to modify data

    self$param_values <- params
    # aggregate over x
    data_x <- data %>%
      dplyr::group_by(x, PANEL) %>%
      dplyr::summarise(
        y = sum(y, na.rm = TRUE),
        group = x # this is dangerous. what if there is already a grouping?
      ) %>%
      dplyr::mutate(
        petals = purrr::map2(.x = x, .y = y, .f = function(.x, .y) {
          petal(.x, .y, shape = params$shape)
        })
      )
    data <- data %>%
      dplyr::select(-y, -group) %>%
      dplyr::left_join(data_x, by = c("x", "PANEL")) %>%
      dplyr::select(-x, -y) %>%
      tidyr::unnest(col = petals)
    data$type__ <- "petal"

    disk <- data %>% dplyr::filter(y == 0)
    disk$type__ <- "disk"

    diskcenter <- disk %>% dplyr::filter(!duplicated(PANEL))
    diskcenter$y <- -params$disk.size
    diskcenter$type__ <- "center"

    rbind(data, diskcenter, disk)
  },
  handle_na = function(data, params) {
    data
  },
  draw_key = draw_key_polygon,
  draw_panel = function(self, data, panel_params, coord,
                        lineend = "butt", linejoin = "round",
                        linemitre = 10, ...) {
    # cat("draw_panel in GeomFlower\n")
    if (!("CoordPolar" %in% class(coord))) {
      warning("geom_flower should only be used in polar coordinates.
              Mappings to y are not linear.
              Add + coord_polar() to your call.")
    }
    #   browser()
    center <- data %>% dplyr::filter(type__ == "center")
    disk <- data %>%
      dplyr::filter(type__ == "disk") %>%
      dplyr::mutate(
        fill = self$param_values$disk.colour,
        group = -1
      ) %>%
      arrange(x)

    data <- data %>% filter(type__ == "petal")
    ggplot2:::ggname(
      "geom_flower",
      grid::grobTree(
        grid::pointsGrob(
          center$x, center$y,
          pch = 1,
          gp = grid::gpar(
            col = NA,
            fill = NA,
            # Stroke is added around the outside of the point
            fontsize = 1,
            lwd = 1
          )
        ),
        ggplot2::GeomPolygon$draw_panel(data, panel_params, coord, lineend = lineend, linejoin = linejoin, linemitre = linemitre, ...),
        ggplot2::GeomPolygon$draw_panel(disk, panel_params, coord, lineend = lineend, linejoin = linejoin, linemitre = linemitre, ...)
      )
    )
  }
)
