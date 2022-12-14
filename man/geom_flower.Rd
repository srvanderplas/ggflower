% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggflower.R
\docType{data}
\name{geom_flower}
\alias{geom_flower}
\alias{GeomFlower}
\title{Create the flower geom}
\usage{
geom_flower(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = F,
  show.legend = NA,
  inherit.aes = T,
  disk.size = 1,
  disk.colour = "lightyellow",
  disk.color = disk.colour,
  shape = "normal",
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{na.rm}{If \code{FALSE} (the default), removes missing values with a warning.
If \code{TRUE} silently removes missing values.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{disk.size}{Size of the flower center}

\item{disk.colour}{See disk.color}

\item{disk.color}{Color of the flower center}

\item{shape}{shape of the petal, one of "normal", "cos", "circle", "heart"}

\item{...}{other arguments passed on to \code{layer}. These are often aesthetics,
used to set an aesthetic to a fixed value, like \code{color = 'red'}
or \code{size = 3}. They may also be parameters to the paired geom/stat.}
}
\value{
a list consisting of a \code{\link[ggplot2:layer]{ggplot2::layer()}} object and its associated scales.
}
\description{
Create the flower geom
}
\examples{
n <- 8
dframe <- data.frame(index = 1:n, y = n:1)
# scaling is not correct for cartesian coordinates
# squaring the y values ensures that heights are linear in y
dframe |>
  ggplot() +
  geom_flower(aes(x = index, y = y^2, fill = factor(index)))
# use polar coordinates
dframe |>
  ggplot(aes(x = index, y = y)) +
  geom_flower(aes(fill = factor(index)),
    shape = "cos", colour = NA, alpha = 0.8
  ) +
  geom_flower(aes(x = 1:8, y = 2), linewidth = 0.125, fill = NA,
              colour = "lightyellow", shape = "cos") +
  geom_flower(aes(x = 1:8, y = 4), linewidth = 0.25, fill = NA,
              colour = "lightyellow", shape = "cos") +
  geom_flower(aes(x = 1:8, y = 6), linewidth = 0.125, fill = NA,
              colour = "lightyellow", shape = "cos") +
  # annotate("point", x = 1, y = -3, color="lightyellow", size=20) +
  geom_flower(aes(x = 1:8, y = 0), linewidth = 0.25, fill = NA,
              colour = "yellow", shape = "cos") +
  coord_polar() +
  theme_void() +
  theme(legend.position = "bottom")
dframe |>
  ggplot(aes(x = index, y = y)) +
  geom_flower(aes(fill = factor(index)), shape = "circle") +
  coord_polar()
}
\keyword{datasets}
