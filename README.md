## Simulações em R para o Artigo "Viés de Seleção na Pesquisa Social, em Políticas Públicas e no Direito"

Este repositório contém scripts em R que demonstram simulações de dados e análises relacionadas a questões socioeconômicas, com foco em desigualdade salarial e  modelos de litígio, para o artigo **"Viés de Seleção na Pesquisa Social, em Políticas Públicas e no Direito"**. Os scripts abordam os seguintes tópicos:

**1. Simulação de Salários (genero_escolaridade.R)**

Este script simula um banco de dados de salários, escolaridade e gênero, com o objetivo de analisar a diferença salarial entre homens e mulheres. O script inclui:

* Geração de dados com variáveis como anos de estudo, nível de ensino e ocupação, considerando disparidades de gênero.
* Introdução de correlação entre salário e escolaridade, com efeitos diferenciados para homens e mulheres.
* Simulação de viés de dados (censura) nos salários das mulheres, criando um cenário onde a média salarial entre gêneros se iguala ao desconsiderar salários baixos.
* Cálculo de médias, desvios-padrão e testes de significância para comparar os salários entre homens e mulheres, com e sem censura.
* Visualização dos resultados através de gráficos de dispersão com linhas de tendência e gráficos de frequência acumulada.
* Implementação de uma simulação de Monte Carlo para analisar a distribuição de médias salariais em diversas amostras.

**2. Simulação de Monte Carlo para Salários Censurados (monte_carlo.R)**

Este script complementa a análise de salários do primeiro script, focando na aplicação da técnica de Monte Carlo para analisar o impacto da censura de dados. O script:

* Gera dados simulados de salários para homens e mulheres, com a introdução de censura nos salários baixos das mulheres.
* Realiza múltiplas amostragens dos dados, calculando a média salarial para mulheres com e sem considerar a censura.
* Plota a distribuição acumulada das médias salariais obtidas nas diferentes amostras, comparando os resultados com e sem censura.
* Demonstra visualmente como a censura de dados pode distorcer a percepção da diferença salarial entre gêneros.

**3. Modelos de Probabilidade de Litígio (hipotese_pk.R)**

Este script explora o modelo teórico de Priest & Klein (1983) sobre a probabilidade de litígio, que analisa as decisões de autores e réus em buscar ou evitar o judiciário. O script:

* Define funções para simular a distribuição de mérito de um caso, as probabilidades de sucesso do autor e do réu e o cálculo do limite para a decisão de litigar.
* Cria gráficos interativos que ilustram a relação entre as probabilidades de sucesso, o limite para litigar e a quantidade de informação disponível.
* Permite a manipulação de parâmetros do modelo, como custos do litígio, valores de acordo e mérito da decisão do juiz, para observar seus efeitos na probabilidade de litígio.
* Demonstra a influência da quantidade de informação disponível para as partes na decisão de litigar e na seleção de casos que chegam ao judiciário.

**Como usar os scripts:**

* Faça o download ou clone este repositório.
* Abra os arquivos ".R" no RStudio ou em seu ambiente de programação R preferido.
* Execute o código passo-a-passo para visualizar as simulações e resultados.
* Explore a modificação dos parâmetros e funções para adaptar as análises às suas próprias questões de pesquisa.

**Bibliotecas R necessárias:**

* `tidyverse`
* `patchwork`
* `shiny`
* `plotly`

**Observações:**

* Os scripts utilizam dados simulados para fins didáticos e de demonstração. As conclusões e interpretações devem ser consideradas dentro do contexto das simulações.
* É encorajada a experimentação com diferentes parâmetros e cenários para aprofundar a compreensão dos conceitos abordados. 

## Licença

Este projeto está licenciado sob a licença MIT - veja o arquivo [LICENSE.md](LICENSE) para detalhes. 
