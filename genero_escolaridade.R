# Prompt inicial utilizado:

# Prepare um código R que gere um banco de dados de salários, 
# escolaridade e genero:
#
# - A escolaridade deve apresentar anos de estudo, com valores compatíveis 
# com os observados em dados e pesquisas no Brasil, além de dummies para maior 
# nível de ensino alcançado (ensino fundamental incompleto, fundamental, 
# secundário, superior  e outros típicos de pesquisas do tipo
# - A ocupação deve possuir um pequeno conjunto de ocupações típicas
# - Salários devem estar em reais, em valores compatíveis com os níveis 
# de salários no Brasil
# 
# Este grupo de trabalhadores deve ser constituído por metade de homens e 
# metade de mulheres, com 1000 observações. Os salários guardam alguma 
# correlação com escolaridade, com o efeito da escolaridade para as mulheres 
# sendo um menor que a dos homens – essa correlação não deve ser perfeita, 
# assim inclua uma variação estocástica, na forma de um termo de erro com 
# média zero e variância.
# 
# O salário médio também deve ser menor para as mulheres em relação aos homens.
#
# Ainda em relação aos salários, os salários das mulheres devem ser de tal 
# ordem que a média de salários para as mulheres seja menor que a dos homens 
# mas que, se os salários das mulheres abaixo de um determinado valor forem 
# omitidos ou desconsiderados na média, a média de homens e mulheres passa a 
# ser igual (dados censurados abaixo de um threshold).
# 
# Apresente as médias e desvios-padrões para os salários dos homens e para 
# os salários das mulheres (com e sem censura), testando se a diferença entre 
# os salários das mulheres, tanto censurado quanto sem censura, e o dos homens 
# é significativa.
# 
# Teste a necessidade de instalação de pacotes e os instale, e apresente 
# gráficos de dispersão, com linha de tendência, e informações mostrando 
# essas diferenças.
# 
# Apresente gráficos de dispersão, com linha de tendência, e informações 
# mostrando essas diferenças. Mostre as linhas de tendência para homens, 
# para mulheres considerando a censura e, novamente, para mulheres sem 
# considerar a censura, além dos pontos com as observações em cores e 
# formatos diferentes. Na análise gráfica, use uma cor e um padrão diferente 
# para a linha de tendência de salários para as mulheres com a censura de 
# dados e sem a censura (na legenda, use as expressões "com autosseleção" e 
# "sem autosseleção"). Use também cores diferentes para os pontos de 
# observação censurados.
# 
# Apresente dois gráficos de frequência acumulada comparando 1) a 
# distribuição dos salários das mulheres com e sem censura e 2) a 
# distribuição acumulada de salários de homens e mulheres, sem censura
# Crie um código para uma simulação do tipo Monte Carlo, fazendo 1000 
# amostragens de salários de mulheres em amostras de 30, com e sem a censura, 
# e apresente um gráfico com a distribuição acumulada destas duas simulações.
# 
# Farei mais solicitações de códigos R na sequência especificamente para 
# este banco de dados, , chamando este código de “Simulação de Salários”, 
# assim nas próximas questões mantenha essa referência.

# Carregando as bibliotecas
library(tidyverse)

# Simulação: escolaridade e gênero ----------------------------------------

# Definindo o número de observações
N <- 2000
# Definindo um valor arbitrário para a truncagem
corte <- 6000

# Gerando os dados
set.seed(1) # Para reprodutibilidade
gen <- sample(c("Homem", "Mulher"), N, replace = TRUE, prob = c(0.5, 0.5))
gen_dummy <- as.integer(gen == "Mulher")

# Escolaridade em anos (valores típicos no Brasil)
educ_anos <- sample(4:20, N, replace = TRUE)

# Associação entre salário, gênero e escolaridade
# Note que existe interação entre gênero e escolaridade
sal_base <- runif(N, min = 1500, max = 35000)
sal <- sal_base - 3000 * gen_dummy + 500 * educ_anos - 200 * educ_anos * gen_dummy + rnorm(N, sd = 500)

# Definimos que existe truncagem quando o salário é 
# menor que corte + 500 * anos de escolaridade e o gênero é feminino
trunc <- as.integer((sal < corte + 500 * educ_anos) & (gen == "Mulher"))

# Identificando e aplicando a truncagem nos salários das mulheres
dados <- tibble(sal, gen, educ_anos, trunc)

# Criando o gráfico
p <- dados |> 
  ggplot(aes(x = educ_anos, y = sal, color = gen)) +
  geom_point(
    aes(alpha = as.factor(trunc), shape = as.factor(trunc)), 
    size = 5
  ) +
  # apenas não truncados
  geom_smooth(
    method = "lm", aes(color = gen, linetype = gen), se = FALSE,
    linewidth = 2,
    data = filter(dados, trunc == 0)
  ) +
  # todos os dados
  geom_smooth(
    method = "lm", 
    se = FALSE,
    color = "red",
    linewidth = 2,
    data = filter(dados, gen == "Mulher"),
    linetype = 3
  ) +
  scale_shape_manual(values = c(16, 17)) + 
  scale_colour_viridis_d(begin = .2, end = .8, option = 1) +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "R$", big.mark = ".", decimal.mark = ","
  )) +
  scale_x_continuous(limits = c(3, 20)) +
  scale_alpha_manual(values = c(.1, .7)) +
  labs(
    x = "Anos de Escolaridade",
    y = "Salário",
    color = "Gênero",
    linetype = "Gênero",
    alpha = "Truncagem",
    shape = "Truncagem"
  ) +
  theme_minimal()

p

ggsave("fig/graf1.pdf", p, width = 8, height = 6, dpi = 600, bg = "white")
ggsave("fig/graf1.svg", p, width = 8, height = 6, dpi = 600, bg = "white")

# Modelos com e sem truncagem ---------------------------------------------

modelo <- lm(
  sal/1000 ~ gen * educ_anos, 
  data = dados
)

modelo_trunc <- lm(
  sal/1000 ~ gen * educ_anos, 
  data = filter(dados, trunc == 0)
)

stargazer::stargazer(
  modelo,
  modelo_trunc, 
  column.labels = c("Dados completos", "Com truncagem"),
  type = "latex"
)


# Modelos com amostragem --------------------------------------------------

# distribuição dos dados completos, sem separar por educação
p <- dados |> 
  ggplot(aes(x = sal, fill = gen)) +
  geom_density() +
  scale_fill_viridis_d(begin = .2, end = .8, option = 1, alpha = .4) +
  theme_minimal() +
  scale_y_continuous(labels = scales::number_format()) +
  labs(
    x = "Salário",
    y = "Densidade",
    fill = "Gênero"
  )

p
ggsave("fig/dist1.pdf", p, width = 8, height = 6, dpi = 600, bg = "white")
ggsave("fig/dist1.svg", p, width = 8, height = 6, dpi = 600, bg = "white")



# modelos com amostras de 86 pessoas
set.seed(1)
modelos_amostra <- purrr::map(1:3, \(x) {
  lm(
    sal/1000 ~ gen * educ_anos, 
    data = dados |> 
      filter(trunc == 0) |> 
      slice_sample(n = 86)
  )
})

stargazer::stargazer(
  modelos_amostra, 
  column.labels = c("Amostra 1", "Amostra 2", "Amostra 3"),
  type = "latex"
)


# Gráfico cumulativo ------------------------------------------------------

set.seed(99)
n_amostras <- 1000

# Realiza amostras de 86 observações, obtendo a média dos salários truncados 
# e não truncados para as mulheres
amostras <- map(seq_len(n_amostras), \(x) {
  dados |> 
    slice_sample(n = 86) |> 
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
ggsave("fig/monte1.svg", p, width = 10, height = 6, dpi = 600, bg = "white")
