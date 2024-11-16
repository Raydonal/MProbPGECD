# Instalar e carregar o pacote plotly
install.packages("plotly")
library(plotly)

# Definir a função F0
F0 <- function(x, y) {
  ifelse(x >= 0 & y >= 0 & x + y >= 1, 1, 0)
}

# Criar uma grade de valores para x e y
x <- seq(-1, 2, length.out = 300)
y <- seq(-1, 2, length.out = 300)
grid <- expand.grid(x = x, y = y)

# Calcular F0 em cada ponto da grade
grid$z <- with(grid, F0(x, y))

# Converter para matriz para plotagem
Z_matrix <- matrix(grid$z, nrow = length(x), ncol = length(y))

# Criar o gráfico 3D
plot_ly(x = x, y = y, z = Z_matrix) %>%
   add_surface(colorscale = list(c(0, 1), c("darkgray","darkred")), opacity =0.8) %>%
  layout(title = "Superfície da Função F₀(x, y)",
         scene = list(xaxis = list(title = "x"),
                      yaxis = list(title = "y"),
                      zaxis = list(title = "F₀(x, y)", range = c(0, 1.2))))


# Instalar e carregar pacotes necessários
if (!require("plotly")) install.packages("plotly")
library(plotly)

# Definir a função F0
F0 <- function(x, y) {
  ifelse(x >= 0 & y >= 0 & x + y >= 1, 1, 0)
}

# Criar uma grade de valores para x e y
x <- seq(-0.5, 1.5, length.out = 100)
y <- seq(-0.5, 1.5, length.out = 100)
grid <- expand.grid(x = x, y = y)

# Calcular os valores de F0 para a superfície
grid$z <- with(grid, F0(x, y))

# Definir os pontos para a região de interesse
region <- expand.grid(x = c(0, 1), y = c(0, 1))  # Retângulo entre (0, 0) e (1, 1)
region$z <- with(region, F0(x, y))

# Criar a superfície da função F0 com projeção da região de interesse
fig <- plot_ly() %>%
  add_surface(
    x = ~matrix(grid$x, nrow = length(x), ncol = length(y)),
    y = ~matrix(grid$y, nrow = length(x), ncol = length(y)),
    z = ~matrix(grid$z, nrow = length(x), ncol = length(y)),
    colors = c("darkgray","darkred"), opacity =0.8,
    showscale = FALSE
  ) %>%
  add_trace(
    type = "scatter3d",
    mode = "lines+markers",
    x = c(0, 1, 1, 0, 0),
    y = c(0, 0, 1, 1, 0),
    z = c(0, 0, 0, 0, 0),
    line = list(color = "red", width = 5),
    marker = list(size = 5, color = "red"),
    name = "Projeção da Região"
  ) %>%
  add_trace(
    type = "scatter3d",
    mode = "lines",
    x = c(0, 1, 1, 0, 0),
    y = c(0, 0, 1, 1, 0),
    z = c(1, 1, 1, 1, 1),
    line = list(color = "green", width = 5),
    name = "Região na Superfície"
  ) %>%
  layout(
    title = "Superfície da Função F₀(x, y) com Região de Interesse",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "F₀(x, y)", range = c(0, 1.2))
    )
  )

# Exibir o gráfico
fig


# Instalar e carregar o pacote plotly
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)

# Definir a função F0
F0 <- function(x, y) {
  ifelse(x >= 0 & y >= 0 & x + y >= 1, 1, 0)
}

# Criar uma grade de valores para x e y
x <- seq(-0.5, 1.5, length.out = 100)
y <- seq(-0.5, 1.5, length.out = 100)
grid <- expand.grid(x = x, y = y)

# Calcular F0 na grade
grid$z <- with(grid, F0(x, y))

# Identificar as regiões de interesse
grid$region <- with(grid, 
                    (x > 0 & x <= 1 & y > 0 & y <= 1) * F0(1, 1) -
                    (x > 0 & x <= 1 & y >= 0 & y <= 0) * F0(1, 0) -
                    (x >= 0 & x <= 0 & y > 0 & y <= 1) * F0(0, 1) +
                    (x >= 0 & x <= 0 & y >= 0 & y <= 0) * F0(0, 0))

# Calcular o ponto Combined = F0(1,1) - F0(1,0) - F0(0,1) + F0(0,0)
combined_value <- F0(1, 1) - F0(1, 0) - F0(0, 1) + F0(0, 0)
combined_point <- data.frame(x = 1, y = 1, z = combined_value)

# Converter os dados para formato de matriz para superfície
z_matrix <- matrix(grid$z, nrow = length(x), ncol = length(y))
region_matrix <- matrix(grid$region, nrow = length(x), ncol = length(y))

# Criar o gráfico 3D da superfície
plot <- plot_ly(x = x, y = y, z = z_matrix, type = "surface", colorscale =  c("darkgray","darkred"), opacity =0.8) %>%
  add_surface(z = z_matrix, showscale = TRUE, name = "F0(x, y)", c("gray90","green"), opacity =0.3) %>%
  add_surface(z = region_matrix, opacity = 0.3, colorscale = list(c(0, 1), c("grey", "green")),
              name = "Região de Interesse") %>%
  add_trace(type = "scatter3d", mode = "markers", 
            x = combined_point$x, y = combined_point$y, z = combined_point$z,
            marker = list(size = 6, color = "red", symbol = "circle"),
            name = "Prob do retangulo") %>%
  layout(
    title = "Superfície de F0(x, y) com Região de Interesse",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "F0(x, y)")
    )
  )

# Mostrar o gráfico
plot


library(ggplot2)



# Variáveis discretas
x_disc <- c(0, 1, 2)
y_disc <- c(0, 1, 2)

# Matriz de probabilidades conjuntas P(X1 = i, X2 = j)
P_joint <- matrix(c(
  0.1, 0.2, 0.1,
  0.1, 0.2, 0.1,
  0.05, 0.05, 0.1
), nrow = 3, byrow = TRUE)

# Cálculo da função conjunta acumulada
F_joint <- apply(P_joint, 2, cumsum)
F_joint <- t(apply(F_joint, 1, cumsum))  # Acumulado bidimensional

# Gráfico da superfície acumulada conjunta
df_joint <- data.frame(expand.grid(x = x_disc, y = y_disc), z = as.vector(F_joint))
ggplot(df_joint, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  geom_text(aes(label = round(z, 2)), color = "white") +
  labs(title = "Função de Distribuição Conjunta (Discreta)",
       x = "X1", y = "X2", fill = "F(X1, X2)") +
  theme_minimal()

# Cálculo das marginais acumuladas
F_X1 <- apply(F_joint, 1, max)  # Marginal acumulada para X1
F_X2 <- apply(F_joint, 2, max)  # Marginal acumulada para X2

# Gráficos das marginais acumuladas
ggplot(data.frame(x = x_disc, F_X1 = F_X1), aes(x, F_X1)) +
  geom_step(color = "blue", size = 1.2) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Marginal Acumulada F_X1(x) (Discreta)", x = "x", y = "F_X1(x)") +
  theme_minimal()

ggplot(data.frame(y = y_disc, F_X2 = F_X2), aes(y, F_X2)) +
  geom_step(color = "green", size = 1.2) +
  geom_point(size = 3, color = "green") +
  labs(title = "Marginal Acumulada F_X2(y) (Discreta)", x = "y", y = "F_X2(y)") +
  theme_minimal()


# Função Triangular Contínua
F_triangular <- function(x, y) {
  pmax(0, pmin(1, x + y - x * y))
}

# Dados para a grade
x <- seq(0, 1, length.out = 100)
y <- seq(0, 1, length.out = 100)
z <- outer(x, y, F_triangular)

# Gráfico da superfície acumulada conjunta
df_cont <- data.frame(expand.grid(x = x, y = y), z = as.vector(z))
ggplot(df_cont, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  labs(title = "Função de Distribuição Conjunta (Contínua)",
       x = "X1", y = "X2", fill = "F(X1, X2)") +
  theme_minimal()

# Marginais acumuladas contínuas
F_X1_cont <- sapply(x, function(x1) integrate(function(y) F_triangular(x1, y), 0, 1)$value)
F_X2_cont <- sapply(y, function(y2) integrate(function(x) F_triangular(x, y2), 0, 1)$value)

# Gráficos das marginais acumuladas
ggplot(data.frame(x = x, F_X1 = F_X1_cont), aes(x, F_X1)) +
  geom_line(color = "blue") +
  labs(title = "Marginal Acumulada F_X1(x)", x = "x", y = "F_X1(x)") +
  theme_minimal()

ggplot(data.frame(y = y, F_X2 = F_X2_cont), aes(y, F_X2)) +
  geom_line(color = "green") +
  labs(title = "Marginal Acumulada F_X2(y)", x = "y", y = "F_X2(y)") +
  theme_minimal()



library(plotly)

# Função Triangular Contínua
F_triangular <- function(x, y) {
  pmax(0, pmin(1, x + y - x * y))
}

# Dados para a grade contínua
x_cont <- seq(0, 1, length.out = 100)
y_cont <- seq(0, 1, length.out = 100)
z_cont <- outer(x_cont, y_cont, F_triangular)

# Gráfico da superfície contínua usando plotly
fig_cont <- plot_ly(
  x = ~x_cont,
  y = ~y_cont,
  z = ~z_cont,
  type = "surface",
  colorscale =  c("darkgray","darkred"), opacity =0.5
)

fig_cont <- fig_cont %>%
  layout(
    title = "Função de Distribuição Conjunta Contínua",
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "F(X1, X2)")
    )
  )

fig_cont


library(plotly)

# Função de distribuição acumulada conjunta discreta
x_disc <- c(0, 1, 2)
y_disc <- c(0, 1, 2)
F_discrete <- matrix(c(
  0.1, 0.2, 0.4,
  0.2, 0.4, 0.6,
  0.4, 0.6, 1.0
), nrow = 3, byrow = TRUE)

# Gráfico da superfície discreta usando plotly
fig_disc <- plot_ly(
  x = ~x_disc,
  y = ~y_disc,
  z = ~F_discrete,
  type = "surface",
  colorscale = "Viridis"
)

fig_disc <- fig_disc %>%
  layout(
    title = "Função de Distribuição Conjunta Discreta",
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "F(X1, X2)")
    )
  )

fig_disc


#################################################
library(plotly)

# Função Triangular Contínua
F_triangular <- function(x, y) {
  pmax(0, pmin(1, x + y - x * y))
}

# Dados para a grade contínua
x_cont <- seq(0, 1, length.out = 100)
y_cont <- seq(0, 1, length.out = 100)
z_cont <- outer(x_cont, y_cont, F_triangular)

# Cálculo das marginais acumuladas
F_X1_cont <- sapply(x_cont, function(x1) F_triangular(x1, 1))  # Marginal acumulada F_X1
F_X2_cont <- sapply(y_cont, function(y2) F_triangular(1, y2))  # Marginal acumulada F_X2

# Gráfico da superfície contínua com marginais
fig_cont <- plot_ly() %>%
  add_surface(
    x = ~x_cont,
    y = ~y_cont,
    z = ~z_cont,
    colorscale = "Viridis",
    name = "F(X1, X2)"
  ) %>%
  add_trace(
    x = ~x_cont,
    y = rep(1.1, length(x_cont)),  # Colocar a marginal acima da superfície
    z = ~F_X1_cont,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "blue", width = 4),
    name = "F_X1(x)"
  ) %>%
  add_trace(
    x = rep(1.1, length(y_cont)),  # Colocar a marginal acima da superfície
    y = ~y_cont,
    z = ~F_X2_cont,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "red", width = 4),
    name = "F_X2(y)"
  )

fig_cont <- fig_cont %>%
  layout(
    title = "Função de Distribuição Conjunta Contínua com Marginais",
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "F(X1, X2)")
    )
  )

fig_cont


###############################################
library(plotly)

# Função de distribuição acumulada conjunta discreta
x_disc <- c(0, 1, 2)
y_disc <- c(0, 1, 2)
F_discrete <- matrix(c(
  0.1, 0.2, 0.4,
  0.2, 0.4, 0.6,
  0.4, 0.6, 1.0
), nrow = 3, byrow = TRUE)

# Marginais acumuladas
F_X1_disc <- apply(F_discrete, 1, max)  # Marginal acumulada F_X1
F_X2_disc <- apply(F_discrete, 2, max)  # Marginal acumulada F_X2

# Gráfico da superfície discreta com marginais
fig_disc <- plot_ly() %>%
  add_surface(
    x = ~x_disc,
    y = ~y_disc,
    z = ~F_discrete,
    colorscale = "Viridis",
    name = "F(X1, X2)"
  ) %>%
  add_trace(
    x = ~x_disc,
    y = rep(2.5, length(x_disc)),  # Colocar a marginal acima da superfície
    z = ~F_X1_disc,
    type = "scatter3d",
    mode = "lines+markers",
    line = list(color = "blue", width = 4),
    marker = list(size = 6),
    name = "F_X1(x)"
  ) %>%
  add_trace(
    x = rep(2.5, length(y_disc)),  # Colocar a marginal acima da superfície
    y = ~y_disc,
    z = ~F_X2_disc,
    type = "scatter3d",
    mode = "lines+markers",
    line = list(color = "red", width = 4),
    marker = list(size = 6),
    name = "F_X2(y)"
  )

fig_disc <- fig_disc %>%
  layout(
    title = "Função de Distribuição Conjunta Discreta com Marginais",
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "F(X1, X2)")
    )
  )

fig_disc

################################

library(plotly)

# Variáveis discretas
x_disc <- c(0, 1, 2)
y_disc <- c(0, 1, 2)

# Matriz de probabilidades conjuntas P(X1 = i, X2 = j)
P_joint <- matrix(c(
  0.1, 0.2, 0.1,
  0.2, 0.2, 0.1,
  0.1, 0.05, 0.05
), nrow = 3, byrow = TRUE)

# Calcular a distribuição acumulada conjunta F(x1, x2)
F_discrete <- apply(P_joint, 2, cumsum)
F_discrete <- t(apply(F_discrete, 1, cumsum))

# Criar uma grade para visualização
x_grid <- rep(x_disc, each = length(y_disc))
y_grid <- rep(y_disc, times = length(x_disc))
z_grid <- as.vector(t(F_discrete))  # A função acumulada é constante entre os degraus

# Marginais acumuladas
F_X1_disc <- rowSums(P_joint)  # Marginal acumulada F_X1
F_X1_cum <- cumsum(F_X1_disc)  # Acumulada escada para F_X1

F_X2_disc <- colSums(P_joint)  # Marginal acumulada F_X2
F_X2_cum <- cumsum(F_X2_disc)  # Acumulada escada para F_X2

# Gráfico da superfície discreta com marginais
fig_disc <- plot_ly() %>%
  add_trace(
    x = ~x_grid,
    y = ~y_grid,
    z = ~z_grid,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 6, color = z_grid, colorscale = "Viridis"),
    name = "F(X1, X2)"
  ) %>%
  add_trace(
    x = x_disc,
    y = rep(max(y_disc) + 0.5, length(x_disc)),  # Marginal acima da superfície
    z = F_X1_cum,
    type = "scatter3d",
    mode = "lines+markers",
    line = list(color = "blue", width = 4),
    marker = list(size = 6),
    name = "F_X1(x)"
  ) %>%
  add_trace(
    x = rep(max(x_disc) + 0.5, length(y_disc)),  # Marginal acima da superfície
    y = y_disc,
    z = F_X2_cum,
    type = "scatter3d",
    mode = "lines+markers",
    line = list(color = "red", width = 4),
    marker = list(size = 6),
    name = "F_X2(y)"
  )

fig_disc <- fig_disc %>%
  layout(
    title = "Função de Distribuição Conjunta Discreta com Marginais (Escada)",
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "F(X1, X2)")
    )
  )

fig_disc


library(plotly)

# Variáveis discretas
x <- c(1, 2)
y <- c(1, 2)

# Matriz de probabilidades conjuntas
f_xy <- matrix(c(
  1/4, 1/4,
  1/4, 1/4
), nrow = 2, byrow = TRUE)

# Construção da função acumulada conjunta
F_xy <- apply(f_xy, 2, cumsum)
F_xy <- t(apply(F_xy, 1, cumsum))

# Gráfico da densidade conjunta
fig_density <- plot_ly() %>%
  add_trace(
    x = rep(x, each = length(y)),
    y = rep(y, times = length(x)),
    z = as.vector(f_xy),
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 6, color = "blue"),
    name = "f(x, y)"
  ) %>%
  layout(
    title = "Função de Densidade Conjunta f(x, y)",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "f(x, y)")
    )
  )

fig_density

# Gráfico da função acumulada conjunta
fig_accumulated <- plot_ly() %>%
  add_surface(
    x = ~x,
    y = ~y,
    z = ~F_xy,
    colorscale = "Viridis"
  ) %>%
  layout(
    title = "Função de Distribuição Acumulada Conjunta F(x, y)",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "F(x, y)")
    )
  )

fig_accumulated


library(plotly)

# Suporte discreto para as variáveis
x <- c(1, 2)
y <- c(1, 2)

# Matriz de probabilidades conjuntas P(X1 = i, X2 = j)
f_xy <- matrix(c(
  1/4, 1/4,
  1/4, 1/4
), nrow = 2, byrow = TRUE)

# Construção da função acumulada conjunta F(x, y)
F_xy <- apply(f_xy, 2, cumsum)  # Acumula nas colunas
F_xy <- t(apply(F_xy, 1, cumsum))  # Acumula nas linhas

# Preparação dos dados para o gráfico da função escada
x_steps <- c(0, 1, 2, 2)
y_steps <- c(0, 1, 1, 2)
z_steps <- matrix(0, nrow = 4, ncol = 4)

# Adicionar os valores da função acumulada nos pontos de salto
z_steps[2:3, 2:3] <- F_xy

# Gráfico da função acumulada conjunta
fig_accumulated <- plot_ly() %>%
  add_surface(
    x = ~x_steps,
    y = ~y_steps,
    z = ~z_steps,
    colorscale = "Viridis"
  ) %>%
  layout(
    title = "Função de Distribuição Acumulada Conjunta F(x, y)",
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "F(X1, X2)")
    )
  )

fig_accumulated

library(plotly)
library(MASS)
library(mvtnorm)

# Definir os parâmetros da distribuição normal bivariada
rho <- 0.5  # Correlação
mu <- c(0, 0)  # Vetor de médias
sigma <- matrix(c(1, rho, rho, 1), nrow = 2)  # Matriz de covariância

# Gerar os dados para a grade
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
cdf_values <- matrix(0, nrow = length(x), ncol = length(y))

# Calcular a função de distribuição acumulada (CDF) em cada ponto da grade
for (i in seq_along(x)) {
  for (j in seq_along(y)) {
    cdf_values[i, j] <- pmvnorm(upper = c(x[i], y[j]), mean = mu, sigma = sigma)
  }
}

# Converter para formato adequado para plotagem
x_grid <- rep(x, each = length(y))
y_grid <- rep(y, times = length(x))
z_grid <- as.vector(t(cdf_values))

# Gráfico 3D da CDF usando plotly
fig <- plot_ly(
  x = ~x,
  y = ~y,
  z = ~cdf_values,
  type = "surface",
  colorscale = "Viridis"
)

fig <- fig %>%
  layout(
    title = "Função de Distribuição Acumulada Conjunta (Bivariada Normal)",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "F(X, Y)")
    )
  )

fig

##########################################
library(plotly)
library(mvtnorm)

# Parâmetros da distribuição normal bivariada
rho <- 0.5  # Correlação
mu <- c(0, 0)  # Vetor de médias
sigma <- matrix(c(1, rho, rho, 1), nrow = 2)  # Matriz de covariância

# Gerar os dados para a grade
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
cdf_values <- matrix(0, nrow = length(x), ncol = length(y))

# Calcular a CDF conjunta
for (i in seq_along(x)) {
  for (j in seq_along(y)) {
    cdf_values[i, j] <- pmvnorm(upper = c(x[i], y[j]), mean = mu, sigma = sigma)
  }
}

# Calcular as CDFs marginais
F_X1 <- pnorm(x, mean = mu[1], sd = sqrt(sigma[1, 1]))
F_X2 <- pnorm(y, mean = mu[2], sd = sqrt(sigma[2, 2]))

# Gráfico da superfície da CDF conjunta
fig <- plot_ly() %>%
  add_surface(
    x = ~x,
    y = ~y,
    z = ~cdf_values,
    colorscale =  c("darkgray","darkred"), opacity =0.7,
    name = "CDF Conjunta"
  )

# Adicionar a marginal F_X1(x) como uma linha no eixo X
fig <- fig %>%
  add_trace(
    x = ~x,
    y = rep(3.5, length(x)),  # Posicionar acima da superfície
    z = ~F_X1,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "blue", width = 4),
    name = "Marginal F_X1(x)"
  )

# Adicionar a marginal F_X2(y) como uma linha no eixo Y
fig <- fig %>%
  add_trace(
    x = rep(3.5, length(y)),  # Posicionar acima da superfície
    y = ~y,
    z = ~F_X2,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "red", width = 4),
    name = "Marginal F_X2(y)"
  )

# Layout final
fig <- fig %>%
  layout(
    title = "Função de Distribuição Acumulada Conjunta com Marginais",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "F(X, Y)")
    )
  )

fig


####################################
library(plotly)
library(mvtnorm)

# Parâmetros da distribuição normal bivariada
rho <- 0.5  # Correlação
mu <- c(0, 0)  # Vetor de médias
sigma <- matrix(c(1, rho, rho, 1), nrow = 2)  # Matriz de covariância

# Gerar os dados para a grade
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
cdf_values <- matrix(0, nrow = length(x), ncol = length(y))

# Calcular a CDF conjunta
for (i in seq_along(x)) {
  for (j in seq_along(y)) {
    cdf_values[i, j] <- pmvnorm(upper = c(x[i], y[j]), mean = mu, sigma = sigma)
  }
}

# Calcular as CDFs marginais
F_X1 <- pnorm(x, mean = mu[1], sd = sqrt(sigma[1, 1]))
F_X2 <- pnorm(y, mean = mu[2], sd = sqrt(sigma[2, 2]))

# Gráfico da superfície da CDF conjunta
fig <- plot_ly() %>%
  add_surface(
    x = ~x,
    y = ~y,
    z = ~cdf_values,
    colorscale =  c("darkgray","darkred"), opacity =0.9,
    name = "CDF Conjunta",
    contours = list(
      z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "darkblue", project = list(z = TRUE))
    )
  )

# Adicionar a marginal F_X1(x) como uma linha no eixo X
fig <- fig %>%
  add_trace(
    x = ~x,
    y = rep(3.5, length(x)),  # Posicionar acima da superfície
    z = ~F_X1,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "blue", width = 4),
    name = "Marginal F_X1(x)"
  )

# Adicionar a marginal F_X2(y) como uma linha no eixo Y
fig <- fig %>%
  add_trace(
    x = rep(3.5, length(y)),  # Posicionar acima da superfície
    y = ~y,
    z = ~F_X2,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "red", width = 4),
    name = "Marginal F_X2(y)"
  )

# Layout final
fig <- fig %>%
  layout(
    title = "Função de Distribuição Acumulada Conjunta com Marginais e Curvas de Nível",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "F(X, Y)")
    )
  )

fig

