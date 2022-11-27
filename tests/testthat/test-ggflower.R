test_that("geom_flower issues warning without coord_polar", {
  dat <- tibble(x = c("a", "b", "b", "c", "c", "c"))

  p <- ggplot(dat, aes(x)) + geom_flower(stat = "count")

  expect_warning( # warning created at render stage
    ggplotGrob(p + ylim(0, 2.5)),
    "geom_flower should only be used in polar coordinates"
  )
})

gen_petal <- function(df, shape) {
  ggplot(df, aes(x, y)) + coord_polar() + geom_flower(shape = shape)
}

test_that("petals work for all available options", {
  dat <- tibble(x = letters[1:3], y = 1:3)

  base <- ggplot(dat, aes(x, y)) + coord_polar()

  p <- expect_no_error(
    purrr::map(c("box", "normal", "cos", "circle", "heart"), gen_petal, df = dat)
  )
  x <- purrr::map(p, layer_data)

  xy <- purrr::map(x, purrr::pluck, "y")
  xnoy <- purrr::map(x, ~select(., -y))

  # # Petal types change y values only
  # expect_false(do.call("identical", xy))
  # expect_true(do.call("identical", xnoy))
})
