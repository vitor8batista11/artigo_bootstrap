library(tidyverse)

results <- readxl::read_xlsx("resultados_simulacao.xlsx")

# Gráficos para análise da amplitude dos intervalos

graficos_amplitude <- function(dados, distribution){
  
  dados %>%
    filter(distribuicao == distribution) %>%
    ggplot() +
    aes(x = comprimento, y = metodo) +
    geom_boxplot() +
    labs(x = "Amplitude", y = "Método",
         title = paste0("Amplitudes dos Intervalos - Distribuição ", distribution)) +
    facet_grid(n~confianca) +
    theme(line = element_blank(),
          axis.ticks = element_line(),
          text = element_text(size = 13))
  
}

graficos_amplitude(results, "normal")
graficos_amplitude(results, "exponential")
graficos_amplitude(results, "laplace")
graficos_amplitude(results, "uniform")
graficos_amplitude(results, "t")

ggplot(results) +
  aes(x = comprimento, y = factor(confianca), fill = n) +
  geom_boxplot() +
  labs(x = "Amplitude", y = "Nível de confiança",
       title = "Amplitudes dos Intervalos", fill = "Tamanho amostral") +
  facet_grid(distribuicao~metodo) +
  theme(line = element_blank(),
        axis.ticks = element_line(),
        text = element_text(size = 13))
