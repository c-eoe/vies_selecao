# Carregando as bibliotecas
library(tidyverse)

# Simulação: Monte Carlo --------------------------------------------------

N <- 1000
n_amostras <- 1000

# Reprodutibilidade
set.seed(123)

# Gera os dados
sal_base <- runif(N, min = 4000, max = 8000)
gen <- sample(c("Homem", "Mulher"), N, replace = TRUE, prob = c(0.5, 0.5))
gen_dummy <- as.integer(gen == "Mulher")
sal <- sal_base - 1500 * gen_dummy + rnorm(N, sd = 800)

# Valor de corte arbitrário para truncagem
corte <- 5000

# Cria a base de dados com a variável truncada, para ser usada na análise
da_mc <- tibble(sal, gen) |> 
  mutate(trunc = sal < corte & gen == "Mulher")

# Distribuição dos salários -----------------------------------------------

# Gráfico de densidade dos salários, comparando homens e mulheres,
# considerando todos os dados
p <- da_mc |> 
  ggplot(aes(x = sal, fill = gen)) +
  geom_density() +
  scale_fill_viridis_d(begin = .2, end = .8, option = 1, alpha = .4) +
  theme_minimal() +
  labs(
    x = "Salário",
    y = "Densidade",
    fill = "Gênero"
  )

p

ggsave("fig/dist1.pdf", p, width = 8, height = 6, dpi = 600, bg = "white")

# Gráfico cumulativo ------------------------------------------------------

# Realiza amostras de 30 observações, obtendo a média dos salários truncados 
# e não truncados para as mulheres
amostras <- map(seq_len(n_amostras), \(x) {
  da_mc |> 
    slice_sample(n = 30) |> 
    summarise(
      m_trunc = mean(sal[!trunc]),
      m_ntrunc = mean(sal),
      .by = gen
    )
}, .progress = TRUE)

# Constrói o gráfico cumulativo das médias, comparando truncados e não truncados
p <- amostras |> 
  list_rbind(names_to = "amostra") |> 
  pivot_longer(c(m_trunc, m_ntrunc)) |> 
  mutate(
    name = ifelse(name == "m_trunc", "Observado", "Real"),
    name = factor(name, levels = c("Real", "Observado"))
  ) |> 
  ggplot(aes(x = value, colour = gen, linetype = name)) +
  stat_ecdf(geom = "step", linewidth = 1.2) +
  scale_colour_viridis_d(begin = .2, end = .8, option = 1) +
  scale_linetype_manual(values = c(4, 1)) +
  theme_minimal() +
  labs(
    x = "Salário",
    y = "Densidade acumulada das médias",
    colour = "Truncagem",
    linetype = "Gênero"
  )

p

ggsave("fig/monte1.pdf", p, width = 10, height = 6, dpi = 600, bg = "white")
