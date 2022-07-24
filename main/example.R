# colley's method example
solve(
  a = matrix(
    c(
      5, 0, -1, -1, -1,
      0, 3, 0, 0, -1,
      -1, 0, 4, 0, -1,
      -1, 0, 0, 3, 0,
      -1, -1, -1, 0, 5
    ),
    byrow = TRUE,
    nrow = 5,
    ncol = 5
  ),
  b = matrix(
    c(1+3/2, 1+1/2, 1, 1-1/2, 1-3/2),
    nrow = 5,
    ncol = 1
  )
)

