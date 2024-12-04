library(plotly)

# Define the discrete joint probability function f(x, y)
f <- function(x, y) {
  if ((x == 1 & y == 1) | (x == 1 & y == 2) | (x == 2 & y == 1) | (x == 2 & y == 2)) {
    return(0.25)
  }
  return(0)
}

# Define the discrete cumulative distribution function F(x, y)
F <- function(x, y) {
  if (x < 1 | y < 1) {
    return(0)
  } else if (1 <= x & x < 2 & 1 <= y & y < 2) {
    return(0.25)
  } else if (1 <= x & x < 2 & y >= 2) {
    return(0.5)
  } else if (x >= 2 & 1 <= y & y < 2) {
    return(0.5)
  } else if (x >= 2 & y >= 2) {
    return(1)
  }
}

# Create coordinates for f(x, y)
x_points <- c(1, 1, 2, 2)
y_points <- c(1, 2, 1, 2)
z_points <- c(f(1, 1), f(1, 2), f(2, 1), f(2, 2))

# Create bars for f(x, y)
f_plot <- plot_ly()
for (i in seq_along(x_points)) {
  f_plot <- f_plot %>%
    add_trace(
      x = c(x_points[i], x_points[i]),
      y = c(y_points[i], y_points[i]),
      z = c(0, z_points[i]),
      type = "scatter3d",
      mode = "lines+marker",
      line = list(width = 10, color = "blue"),
      marker = list(width = 10, color = "blue"),
      name = NULL #paste0("f(", x_points[i], ",", y_points[i], ")")
    )
}

# Layout for f(x, y)
f_plot <- f_plot %>%
  layout(
    title = "Joint Probability Function (f(x, y))",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "f(x, y)", range = c(0, 0.3))
    )
  )

# Create grid for F(x, y)
x_range <- seq(0.5, 2.5, length.out = 100)
y_range <- seq(0.5, 2.5, length.out = 100)
Z_F <- outer(x_range, y_range, Vectorize(F))

# Plot F(x, y) as a surface
F_plot <- plot_ly(
  x = x_range,
  y = y_range,
  z = Z_F,
  type = "surface",
  colorscale = list(c(0,1), c("gray10","darkred")), opacity =0.8,
  showscale = FALSE
) %>%
  layout(
    title = "Cumulative Distribution Function (F(x, y))",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "F(x, y)", range = c(0, 1))
    )
  )


# Combine plots into a subplot
subplot(f_plot, F_plot, nrows = 1, shareX = TRUE, shareY = TRUE) %>%
  layout(title = "Joint Probability and Cumulative Distribution Functions")
