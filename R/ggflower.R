petal <- function (x, y, shape = "heart") {
  # ignore shape for right now, but we could implement other shapes later
  petal_shape <- data.frame(x0 = seq(-pi, pi, by=0.1))
  if (shape == "box") {
    petal_shape <- data.frame(
      x0 =c(0,0,1,1) - 0.5,
      y0 <- c(0,1,1,0))
  }
  if (shape=="normal")
    petal_shape$y0 <- dnorm(petal_shape$x0)
  if (shape == "cos")
    petal_shape$y0 = cos(petal_shape$x0)
  if (shape == "circle") {
    petal_shape$y0 = 5 + sqrt(1-(petal_shape$x0/pi)^2)
    petal_shape$y0[1] = 0
    petal_shape$y0[length(petal_shape$y0)] = 0
  }
  if (shape == "heart") {
    petal_shape$y0 = 5 + abs(3*sin(petal_shape$x0))
    petal_shape$y0[1] = 0
    petal_shape$y0[length(petal_shape$y0)] = 0
  }

  # scale the shape to be between ± 0.5 horizontally and [0,1] height
  petal_shape$x0 <- petal_shape$x0/diff(range(petal_shape$x0))
  petal_shape$y0 <- (petal_shape$y0 - min(petal_shape$y0))/(max(petal_shape$y0)-min(petal_shape$y0))

  area <- sum(petal_shape$y0)/nrow(petal_shape)
  area2 <- sum(petal_shape$y0^2)/nrow(petal_shape)
  petal_shape$y0 <- 2*petal_shape$y0/area2

  stopifnot(length(x)==1, length(y)==1)
  petal_shape %>%
    dplyr::mutate(
        x = petal_shape$x0+x,
        y = petal_shape$y0*sqrt(y)
      ) %>%
    dplyr::select(-x0, -y0)
}

#' Create the flower geom
#'
#' @param x categorical variable to group by
#' @param y the height of the group (y > 0)
#' @param shape shape of the petal, one of "normal", "cos", "circle", "heart"
#' @export
#' @examples
#' n <- 8
#' dframe <- data.frame(index = 1:n, y = n:1)
#' # scaling is not correct for cartesian coordinates
#' # squaring the y values ensures that heights are linear in y
#' dframe |>
#'   ggplot(aes(x = index, y = y^2)) +
#'   geom_flower(aes(fill = factor(index)))
#' # use polar coordinates
#' dframe |>
#'   ggplot(aes(x = index, y = y)) +
#'   geom_flower(aes(fill = factor(index)),
#'   shape = "cos", colour = NA, alpha = 0.8) +
#'   geom_flower(aes(x=1:8, y=2),linewidth=0.125,fill = NA, colour="lightyellow", shape = "cos") +
#'   geom_flower(aes(x=1:8, y=4),linewidth=0.25,fill = NA, colour="lightyellow", shape = "cos") +
#'   geom_flower(aes(x=1:8, y=6),linewidth=0.125,fill = NA, colour="lightyellow", shape = "cos") +
#'   annotate("point", x = 1, y = 0, color="lightyellow", size=20) +
#'   coord_polar() +
#'   theme_void() +
#'   theme(legend.position="bottom")
#' dframe |>
#'   ggplot(aes(x = index, y = y)) +
#'   geom_flower(aes(fill = factor(index)), shape = "circle") +
#'   coord_polar()
geom_flower <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = F, show.legend = NA,
                        inherit.aes = T, shape="normal", ...) {
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
      ...
    )
  )
}

#' @rdname geom_flower
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom
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

  extra_params = c("na.rm", "shape"),

  setup_data = function(self, data, params) {
    # This is where the computations are done
    # set up columns in your data that map to what
    # you're actually going to plot
    # goal is to modify data

    # aggregate over x
    data_x <- data %>% group_by(x) %>%
      summarise(
        y = sum(y, na.rm=TRUE),
        group = x
        ) %>%
      mutate(
        petals = purrr::map2(.x = x, .y = y, .f = function(.x, .y) {
          petal(.x, .y, shape = params$shape)
        })
      )
    data <- data %>% dplyr::select(-y, -group) %>%
      left_join(data_x, by="x") %>%
      dplyr::select(-x, -y) %>%
      tidyr::unnest(col=petals)

    # Count # classes/groups
    # Set up x variable for density/petals
    # Scale y so that everything's within 0-1 (ish)
    # create normal densities and multiply by y to scale petal height

    # if (!"scale" %in% names(data)) {
    #   if (!"scale" %in% names(params))
    #     data <- cbind(data, scale = self$default_aes$scale)
    #   else
    #     data <- cbind(data, scale = params$scale)
    # }
    #
    # if (!"min_height" %in% names(data)){
    #   if (!"min_height" %in% names(params))
    #     data <- cbind(data, min_height = self$default_aes$min_height)
    #   else
    #     data <- cbind(data, min_height = params$min_height)
    # }
    #
    # transform(data, ymin = y, ymax = y + scale*height)
    data
  },

  handle_na = function(data, params) {
    data
  },
  draw_key = draw_key_polygon,

  draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "round", linemitre = 10,   ...) {

    #cat("draw_panel in GeomFlower\n")
 #   browser()

    data <- ggplot2:::check_linewidth(data, snake_class(self))
    n <- nrow(data)
    if (n == 1) return(zeroGrob())

    munched <- coord_munch(coord, data, panel_params)

      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group), ]

      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      ggplot2:::ggname(
        "geom_flower",
        grid::polygonGrob(
          munched$x, munched$y, default.units = "native",
          id = munched$group,
          gp = grid::gpar(
            col = first_rows$colour,
            fill = alpha(first_rows$fill, first_rows$alpha),
            lwd = first_rows$linewidth * .pt,
            lty = first_rows$linetype,
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre
          )
        )
      )
  },

  # draw_group = function(self, data, panel_params, coord, na.rm = FALSE) {
  #   if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
  #
  #   # split data into data types (ridgeline, vline, point)
  #   data_list <- split(data, factor(data$datatype))
  #
  #   point_grob <- self$make_point_grob(data_list[["point"]], panel_params, coord)
  #   vline_grob <- self$make_vline_grob(data_list[["vline"]], panel_params, coord)
  #
  #   data <- data_list[["ridgeline"]]
  #
  #   # if the final data set is empty then we're done here
  #   if (is.null(data)) {
  #     return(grid::grobTree(vline_grob, point_grob))
  #   }
  #
  #   # otherwise, continue. First we order the data, in preparation for polygon drawing
  #   data <- data[order(data$group, data$x), ]
  #
  #   # remove all points that fall below the minimum height
  #   data$ymax[data$height < data$min_height] <- NA
  #
  #   # Check that aesthetics are constant
  #   aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
  #   if (nrow(aes) > 1) {
  #     stop("Aesthetics can not vary along a ridgeline")
  #   }
  #   aes <- as.list(aes)
  #
  #   # Instead of removing NA values from the data and plotting a single
  #   # polygon, we want to "stop" plotting the polygon whenever we're
  #   # missing values and "start" a new polygon as soon as we have new
  #   # values.  We do this by creating an id vector for polygonGrob that
  #   # has distinct polygon numbers for sequences of non-NA values and NA
  #   # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
  #   # 4, 4, 4, NA)
  #   missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
  #   ids <- cumsum(missing_pos) + 1
  #   ids[missing_pos] <- NA
  #
  #   # munching for polygon
  #   positions <- with(data, data.frame(
  #     x = c(x, rev(x)),
  #     y = c(ymax, rev(ymin)),
  #     id = c(ids, rev(ids))
  #   ))
  #   munched_poly <- ggplot2::coord_munch(coord, positions, panel_params)
  #
  #   # munching for line
  #   positions <- with(data, data.frame(
  #     x = x,
  #     y = ymax,
  #     id = ids
  #   ))
  #   munched_line <- ggplot2::coord_munch(coord, positions, panel_params)
  #
  #   # calculate line and area grobs
  #   line_grob <- self$make_line_grob(munched_line, munched_poly, aes)
  #   area_grob <- self$make_area_grob(munched_poly, aes)
  #
  #   # combine everything and return
  #   grid::grobTree(area_grob, vline_grob, line_grob, point_grob)
  # },

#
#   make_point_grob = function(data, panel_params, coord) {
#     if (is.null(data)) {
#       return(grid::nullGrob())
#     }
#     data$y <- data$ymin
#     coords <- coord$transform(data, panel_params)
#     ggname("geom_ridgeline",
#            grid::pointsGrob(
#              coords$x, coords$y,
#              pch = coords$point_shape,
#              gp = grid::gpar(
#                col = alpha(
#                  data$point_colour %||% data$point_color %||% data$colour,
#                  data$point_alpha %||% data$alpha
#                ),
#                fill = alpha(
#                  data$point_fill %||% data$fill,
#                  data$point_alpha %||% data$alpha
#                ),
#                # Stroke is added around the outside of the point
#                fontsize = coords$point_size * .pt + coords$point_stroke * .stroke / 2,
#                lwd = coords$point_stroke * .stroke / 2
#              )
#            )
#     )
#   },
#
#   make_vline_grob = function(data, panel_params, coord) {
#     if (is.null(data)) {
#       return(grid::nullGrob())
#     }
#     data$xend <- data$x
#     data$y <- data$ymin
#     data$yend <- data$ymax
#     data$alpha <- NA
#
#     # copy vline aesthetics over if set
#     data$colour <- data$vline_colour %||% data$vline_color %||% data$colour
#     data$linetype <- data$vline_linetype %||% data$linetype
#     data$size <- data$vline_size %||% data$size
#     ggplot2::GeomSegment$draw_panel(data, panel_params, coord)
#   },

# I think this is where things are actually drawn
# Need to enforce polar coords

  # make_line_grob = function(munched_line, munched_poly, aes) {
  #   ggname("geom_flower",
  #          grid::polylineGrob(
  #            munched_line$x, munched_line$y, id = munched_line$id,
  #            default.units = "native",
  #            gp = grid::gpar(
  #              col = aes$colour,
  #              lwd = aes$size * .pt,
  #              lty = aes$linetype)
  #          )
  #   )
  # },
  #
  # make_area_grob = function(munched_poly, aes) {
  #   ggname("geom_ridgeline",
  #          grid::polygonGrob(
  #            munched_poly$x, munched_poly$y, id = munched_poly$id,
  #            default.units = "native",
  #            gp = grid::gpar(
  #              fill = ggplot2::alpha(aes$fill, aes$alpha),
  #              lty = 0)
  #          )
  #   )
  # }


)

