#' Create the ggflower geom
#'
#' @param x categorical variable to group by
#' @param y the relative height of the group
#' @export
geom_flower <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = F, show.legend = NA,
                        inherit.aes = T, ...) {
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
      ...
    )
  )
}

#' @rdname geom_flower
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom
#' @export
GeomRidgeline <- ggproto(
  "GeomFlower", Geom,
  # Set default aesthetics
  default_aes = aes(
    # # ridgeline aesthetics
    # color = "black", fill = "grey70", y = 0, size = 0.5, linetype = 1,
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

  extra_params = c("na.rm"),

  setup_data = function(self, data, params) {
    # This is where the computations are done
    # set up columns in your data that map to what
    # you're actually going to plot
    # goal is to modify data

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
  },

  handle_na = function(data, params) {
    data
  },

  # draw_panel = function(self, data, panel_params, coord, ...) {
  #   groups <- split(data, factor(data$group))
  #
  #   # sort list so highest ymin values are in the front
  #   # we take a shortcut here and look only at the first ymin value given
  #   o <- order(unlist(lapply(groups, function(data){data$ymin[1]})), decreasing = TRUE)
  #   groups <- groups[o]
  #
  #   grobs <- lapply(groups, function(group) {
  #     self$draw_group(group, panel_params, coord, ...)
  #   })
  #
  #   ggname(snake_class(self), gTree(
  #     children = do.call("gList", grobs)
  #   ))
  # },

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

  make_line_grob = function(munched_line, munched_poly, aes) {
    ggname("geom_flower",
           grid::polylineGrob(
             munched_line$x, munched_line$y, id = munched_line$id,
             default.units = "native",
             gp = grid::gpar(
               col = aes$colour,
               lwd = aes$size * .pt,
               lty = aes$linetype)
           )
    )
  },

  make_area_grob = function(munched_poly, aes) {
    ggname("geom_ridgeline",
           grid::polygonGrob(
             munched_poly$x, munched_poly$y, id = munched_poly$id,
             default.units = "native",
             gp = grid::gpar(
               fill = ggplot2::alpha(aes$fill, aes$alpha),
               lty = 0)
           )
    )
  }


)

