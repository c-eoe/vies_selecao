# Carrega os pacotes necessários ------------------------------------------

library(shiny)
library(tidyverse)
library(patchwork)

# Funções auxiliares ------------------------------------------------------

# converting reactives to functions

#' Gera a distribuição do mérito
#' 
#' @param n número de observações
#' 
#' @return um vetor com a distribuição do mérito
simular_y <- function(n) {
  stats::rgamma(n, 5, 6)
}

#' Gera a probabilidade do autor
#' 
#' @param n número de observações
#' @param y vetor com a distribuição do mérito
#' @param info quantidade de informação (inverso do desvio padrão)
#' @param ys mérito da decisão do juiz
#' 
#' @return um vetor com a probabilidade do autor
simular_pa <- function(n, y, info, ys) {
  sig <- 1 / exp(info)
  ya <- rnorm(n, sd = sig) + y
  pnorm((ya - ys) / sig)
}

#' Gera a probabilidade do réu
#' 
#' @param n número de observações
#' @param y vetor com a distribuição do mérito
#' @param info quantidade de informação (inverso do desvio padrão)
#' @param ys mérito da decisão do juiz
#' 
#' @return um vetor com a probabilidade do réu
simular_pr <- function(n, y, info, ys) {
  sig <- 1 / exp(info)
  yr <- rnorm(n, sd = sig) + y
  pnorm((yr - ys) / sig)
}

#' Cria a condição de litígio LPG
#' 
#' @param ca custo do autor
#' @param cr custo do réu
#' @param sa valor de acordo do autor
#' @param sr valor de acordo do réu
#' @param j valor da sentença no julgamento
#' 
#' @return o valor limítrofe
calcular_limite <- function(ca, cr, sa, sr, j) {
  (ca + cr - sa - sr) / j
}

#' Gera o gráfico de dispersão
#' 
#' @param pa probabilidade do autor
#' @param pr probabilidade do réu
#' @param limite valor limítrofe
#' 
#' @return um gráfico de dispersão
plot_litigar <- function(pa, pr, limite) {
  tibble::tibble(x = pa, y = pr, litigou = x > y + limite) |>
    dplyr::mutate(litigou = dplyr::if_else(litigou, "Sim", "Não")) |>
    ggplot2::ggplot() +
    ggplot2::aes(x, y, colour = litigou) +
    ggplot2::geom_point(alpha = .5) +
    ggplot2::geom_abline(slope = 1, intercept = -limite) +
    ggplot2::labs(
      x = "Pa", y = "Pr", colour = "Litigou"
    ) +
    ggplot2::scale_colour_manual(
      values = viridis::viridis(2, 1, .2, .8)
    ) +
    ggplot2::theme_minimal()
}

#' Gera o gráfico das densidades e da decisão
#' 
#' @param y vetor com a distribuição do mérito
#' @param pa probabilidade do autor
#' @param pr probabilidade do réu
#' @param ys mérito da decisão do juiz
#' @param limite valor limítrofe
#' @param info quantidade de informação
#' 
#' @return um gráfico das densidades e da decisão
plot_decisao <- function(y, pa, pr, ys, limite, info) {
  dados <- tibble::tibble(
    x = y,
    litigou = pa - pr > limite
  ) |>
    dplyr::mutate(litigou = dplyr::if_else(litigou, "Litigou = Sim", "Litigou = Não"))
  
  p <- ggplot2::ggplot(dados, ggplot2::aes(x, fill = litigou)) +
    ggplot2::geom_density(alpha = .9, show.legend = FALSE) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = ys),
      colour = 2, linetype = 2
    ) +
    ggplot2::labs(x = "Mérito", y = "Densidade", fill = "Litigou") +
    ggplot2::facet_wrap(~litigou, scales = "free_y") +
    ggplot2::scale_fill_manual(
      values = viridis::viridis(2, 1, .2, .8)
    ) +
    ggplot2::theme_minimal(14)
  
  p
}

# Simulações --------------------------------------------------------------
# Simulação 1: variando a informação

set.seed(1)

# informação == 1
y_calc <- simular_y(n = 10000)
pa_calc <- simular_pa(n = 10000, y = y_calc, info = 1, ys = 1)
pr_calc <- simular_pr(n = 10000, y = y_calc, info = 1, ys = 1)
limite_calc <- calcular_limite(ca = 20, cr = 20, sa = 0, sr = 0, j = 100)

p1_litigio <- plot_litigar(pa = pa_calc, pr = pr_calc, limite = limite_calc) +
  labs(title = "Informação baixa")

p1_dist <- plot_decisao(
  y = y_calc, pa = pa_calc, pr = pr_calc, 
  ys = 1, limite = limite_calc, info = 1
) +
  labs(title = "Informação baixa")


# informação == 5
y_calc <- simular_y(n = 10000)
pa_calc <- simular_pa(n = 10000, y = y_calc, info = 5, ys = 1)
pr_calc <- simular_pr(n = 10000, y = y_calc, info = 5, ys = 1)
limite_calc <- calcular_limite(ca = 20, cr = 20, sa = 0, sr = 0, j = 100)

p2_litigio <- plot_litigar(pa = pa_calc, pr = pr_calc, limite = limite_calc) +
  labs(title = "Informação alta")

p2_dist <- plot_decisao(
  y = y_calc, pa = pa_calc, pr = pr_calc, 
  ys = 1, limite = limite_calc, info = 5
) +
  labs(title = "Informação baixa")

# juntando gráficos
(p_litigio <- (p1_litigio / p2_litigio))
(p_dist <- (p1_dist / p2_dist))

#ggplot2::ggsave("fig/pk_simul1_litigio.pdf", p_litigio, width = 10, height = 8)  
#ggplot2::ggsave("fig/pk_simul1_dist.pdf", p_dist, width = 10, height = 8)  


# informação == 5, variando settlement
y_calc <- simular_y(n = 10000)
pa_calc <- simular_pa(n = 10000, y = y_calc, info = 5, ys = 1)
pr_calc <- simular_pr(n = 10000, y = y_calc, info = 5, ys = 1)
limite_calc <- calcular_limite(ca = 20, cr = 20, sa = 15, sr = 15, j = 100)

p2b_litigio <- plot_litigar(pa = pa_calc, pr = pr_calc, limite = limite_calc) +
  labs(title = "Informação alta")

p2b_dist <- plot_decisao(
  y = y_calc, pa = pa_calc, pr = pr_calc, 
  ys = 1, limite = limite_calc, info = 5
) +
  labs(title = "Informação baixa")

# juntando gráficos
(p_litigio <- (p2_litigio / p2b_litigio))
(p_dist <- (p2_dist / p2b_dist))

#ggplot2::ggsave("fig/pk_simul1b_litigio.pdf", p_litigio, width = 10, height = 8)  

# Simulação 2: variando ys e informação

set.seed(1)

# ys == 1, info == 1
y_calc <- simular_y(n = 10000)
pa_calc <- simular_pa(n = 10000, y = y_calc, info = 1, ys = 1)
pr_calc <- simular_pr(n = 10000, y = y_calc, info = 1, ys = 1)
limite_calc <- calcular_limite(ca = 20, cr = 20, sa = 0, sr = 0, j = 100)

p1_litigio <- plot_litigar(pa = pa_calc, pr = pr_calc, limite = limite_calc) +
  labs(title = "Informação baixa, mérito = 1")

p1_dist <- plot_decisao(
  y = y_calc, pa = pa_calc, pr = pr_calc, 
  ys = 1, limite = limite_calc, info = 1
) +
  labs(title = "Informação baixa, limite de mérido baixo")


# ys == 2, info == 1
y_calc <- simular_y(n = 10000)
pa_calc <- simular_pa(n = 10000, y = y_calc, info = 3, ys = 2)
pr_calc <- simular_pr(n = 10000, y = y_calc, info = 1, ys = 2)
limite_calc <- calcular_limite(ca = 20, cr = 20, sa = 0, sr = 0, j = 100)

p2_litigio <- plot_litigar(pa = pa_calc, pr = pr_calc, limite = limite_calc) +
  labs(title = "Informação baixa, mérito = 2")

p2_dist <- plot_decisao(
  y = y_calc, pa = pa_calc, pr = pr_calc, 
  ys = 2, limite = limite_calc, info = 1
) +
  labs(title = "Informação baixa, limite de mérido alto")

# ys == 2, info == 4
y_calc <- simular_y(n = 10000)
pa_calc <- simular_pa(n = 10000, y = y_calc, info = 4, ys = 2)
pr_calc <- simular_pr(n = 10000, y = y_calc, info = 4, ys = 2)
limite_calc <- calcular_limite(ca = 20, cr = 20, sa = 0, sr = 0, j = 100)

p3_litigio <- plot_litigar(pa = pa_calc, pr = pr_calc, limite = limite_calc) +
  labs(title = "Informação alta")

p3_dist <- plot_decisao(
  y = y_calc, pa = pa_calc, pr = pr_calc, 
  ys = 2, limite = limite_calc, info = 4
) +
  labs(title = "Informação alta, limite de mérido alto")


# juntando gráficos
(p_litigio <- (p1_litigio / p2_litigio / p3_litigio))
(p_dist <- (p1_dist / p2_dist / p3_dist))



#ggplot2::ggsave("fig/pk_simul2_litigio.pdf", p_litigio, width = 10, height = 8)
#ggplot2::ggsave("fig/pk_simul2_dist.pdf", p_dist, width = 10, height = 8)


# Shiny app ---------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        # Quantidade de informação
        column(12, sliderInput(
          "info", "Informação (log)", 0, 10, 1, .1)
        ),
        # Quantidade de simulações
        column(12, sliderInput(
          "n", "Quantidade de simulações", 1, 10000, step = 1000, 5000
        )),
        # Mérito da decisão do juiz
        column(12, sliderInput("ys", "Valor limite", 0.1, 3, 1, .1))
      ),
      # Custo do acordo
      fluidRow(
        column(6, sliderInput("sa", "Valor acordo autor", 0, 100, 0)),
        column(6, sliderInput("sr", "Valor acordo réu", 0, 100, 0))
      ),
      # Custo do litígio
      fluidRow(
        column(6, sliderInput("ca", "Custo litígio autor", 0, 100, 20)),
        column(6, sliderInput("cr", "Custo litígio réu", 0, 100, 20))
      ),
      # Valor do julgamento
      fluidRow(
        sliderInput("j", "J", 1, 1000, 100)
      ),
      # Botão para rodar a simulação
      fluidRow(
        actionButton("rodar", "Rodar simulação")
      )
    ),
    mainPanel(
      # Gráficos
      plotly::plotlyOutput("litigar", 800, 400),
      plotly::plotlyOutput("decisao", 800, 400),
      # Textos
      textOutput("txt_qtd"),
      textOutput("txt_prop")
    )
  )
)

server <- function(input, output, session) {
  
  y <- reactive(simular_y(input$n)) |> 
    bindEvent(input$rodar, input$n)
  
  pa <- reactive(simular_pa(input$n, y(), input$info, input$ys))
  
  pr <- reactive(simular_pr(input$n, y(), input$info, input$ys))
  
  limite <- reactive(
    calcular_limite(input$ca, input$cr, input$sa, input$sr, input$j)
  )
  
  output$litigar <- plotly::renderPlotly({
    plot_litigar(pa(), pr(), limite()) |> 
      plotly::ggplotly()
  })
  
  output$decisao <- plotly::renderPlotly({
    plot_decisao(y(), pa(), pr(), input$ys, limite(), input$info) |> 
      plotly::ggplotly()
  })
  
  output$txt_qtd <- renderText({
    n_litig <- sum(pa() - pr() > limite())
    sprintf("Quantidade de litígios em %d simulações: %d", input$n, n_litig)
  })
  
  output$txt_prop <- renderText({
    litigou <- pa() - pr() > limite()
    prop_vitoria <- scales::percent(mean(y()[litigou] > input$ys))
    sprintf("Proporção de vitórias: %s", prop_vitoria)
  })
  
}

shinyApp(ui, server)


