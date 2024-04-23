library(tidyverse)
library(readxl)

dados <- read_excel("resultados_simulacao_pareado.xlsx")

# performance geral

geral <- dados %>%
  group_by(metodo) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

ggplot(geral) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3)

ggplot(geral) +
  aes(x = fct_reorder(metodo, -comprimento), y = comprimento) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Median Interval Length",
       title = "Median Interval Length by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(comprimento, 2)), vjust = -0.3)

ggplot(geral) +
  aes(x = fct_reorder(metodo, -indicador), y = indicador) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Indicator Value",
       title = "Performance Indicator by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(indicador, 2)), vjust = -0.3)

# por parametro

por_parametro <- dados %>%
  mutate(parametro = ifelse(parametro == "sd", "Standard Deviation", "Mean")) %>%
  group_by(metodo, parametro) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

ggplot(por_parametro) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~parametro)

ggplot(por_parametro) +
  aes(x = fct_reorder(metodo, -comprimento), y = comprimento) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Median Interval Length",
       title = "Median Interval Length by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(comprimento, 2)), vjust = -0.3) +
  facet_wrap(~parametro)

ggplot(por_parametro) +
  aes(x = fct_reorder(metodo, -indicador), y = indicador) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Indicator Value",
       title = "Performance Indicator by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(indicador, 2)), vjust = -0.3) +
  facet_wrap(~parametro)

# por alpha

por_alpha <- dados %>%
  mutate(alpha = paste0("Alpha = ", alpha)) %>%
  group_by(metodo, alpha) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

ggplot(por_alpha) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~alpha)

ggplot(por_alpha) +
  aes(x = fct_reorder(metodo, -comprimento), y = comprimento) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Median Interval Length",
       title = "Median Interval Length by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(comprimento, 2)), vjust = -0.3) +
  facet_wrap(~alpha)

ggplot(por_alpha) +
  aes(x = fct_reorder(metodo, -indicador), y = indicador) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Indicator Value",
       title = "Performance Indicator by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(indicador, 2)), vjust = -0.3) +
  facet_wrap(~alpha)

# por distribuição

por_distribuicao <- dados %>%
  group_by(metodo, distribuicao) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

ggplot(por_distribuicao) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~distribuicao)

ggplot(por_distribuicao) +
  aes(x = fct_reorder(metodo, -comprimento), y = comprimento) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Median Interval Length",
       title = "Median Interval Length by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(comprimento, 2)), vjust = -0.3) +
  facet_wrap(~distribuicao)

ggplot(por_distribuicao) +
  aes(x = fct_reorder(metodo, -indicador), y = indicador) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Indicator Value",
       title = "Performance Indicator by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(indicador, 2)), vjust = -0.3) +
  facet_wrap(~distribuicao)

# por tamanho amostral

por_n <- dados %>%
  mutate(n = factor(paste0("N = ", n), levels = c("N = 10", "N = 20", "N = 100"))) %>%
  group_by(metodo, n) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

ggplot(por_n) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~n)

ggplot(por_n) +
  aes(x = fct_reorder(metodo, -comprimento), y = comprimento) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Median Interval Length",
       title = "Median Interval Length by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(comprimento, 2)), vjust = -0.3) +
  facet_wrap(~n)

ggplot(por_n) +
  aes(x = fct_reorder(metodo, -indicador), y = indicador) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Indicator Value",
       title = "Performance Indicator by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(indicador, 2)), vjust = -0.3) +
  facet_wrap(~n)

# por nivel de confianca

por_confianca <- dados %>%
  mutate(confianca = paste0("Confidence Level = ", confianca)) %>%
  group_by(metodo, confianca) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

ggplot(por_confianca) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~confianca)

ggplot(por_confianca) +
  aes(x = fct_reorder(metodo, -comprimento), y = comprimento) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Median Interval Length",
       title = "Median Interval Length by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(comprimento, 2)), vjust = -0.3) +
  facet_wrap(~confianca)

ggplot(por_confianca) +
  aes(x = fct_reorder(metodo, -indicador), y = indicador) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Indicator Value",
       title = "Performance Indicator by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(indicador, 2)), vjust = -0.3) +
  facet_wrap(~confianca)

############### vinicius - grafico adicional

por_confianca10 <- dados %>% filter(n==10) %>%
  mutate(confianca = paste0("Confidence Level = ", confianca)) %>%
  group_by(metodo, confianca) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

por_confianca20 <- dados %>% filter(n==20) %>%
  mutate(confianca = paste0("Confidence Level = ", confianca)) %>%
  group_by(metodo, confianca) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

por_confianca100 <- dados %>% filter(n==100) %>%
  mutate(confianca = paste0("Confidence Level = ", confianca)) %>%
  group_by(metodo, confianca) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

g10 = ggplot(por_confianca10) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method (N = 10)") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~confianca) + ylim(0, 1)

g20 = ggplot(por_confianca20) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method (N = 20)") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~confianca) + ylim(0, 1)

g100 = ggplot(por_confianca100) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method (N = 100)") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~confianca) + ylim(0, 1)

install.packages("ggpubr")
library(ggpubr)

ggarrange(g10, g20, g100 + font("x.text", size = 10),
          ncol = 1, nrow = 3)

############### para media

por_confianca10 <- dados %>% filter(n==10, parametro == "mean") %>%
  mutate(confianca = paste0("Confidence Level = ", confianca)) %>%
  group_by(metodo, confianca) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

por_confianca20 <- dados %>% filter(n==20, parametro == "mean") %>%
  mutate(confianca = paste0("Confidence Level = ", confianca)) %>%
  group_by(metodo, confianca) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

por_confianca100 <- dados %>% filter(n==100, parametro == "mean") %>%
  mutate(confianca = paste0("Confidence Level = ", confianca)) %>%
  group_by(metodo, confianca) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

g10 = ggplot(por_confianca10) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method (N = 10) (Parameter: Mean)") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~confianca) + ylim(0, 1)

g20 = ggplot(por_confianca20) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method (N = 20) (Parameter: Mean)") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~confianca) + ylim(0, 1)

g100 = ggplot(por_confianca100) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method (N = 100) (Parameter: Mean)") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~confianca) + ylim(0, 1)

install.packages("ggpubr")
library(ggpubr)

ggarrange(g10, g20, g100 + font("x.text", size = 10),
          ncol = 1, nrow = 3)

############### para sd

por_confianca10 <- dados %>% filter(n==10, parametro == "sd") %>%
  mutate(confianca = paste0("Confidence Level = ", confianca)) %>%
  group_by(metodo, confianca) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

por_confianca20 <- dados %>% filter(n==20, parametro == "sd") %>%
  mutate(confianca = paste0("Confidence Level = ", confianca)) %>%
  group_by(metodo, confianca) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

por_confianca100 <- dados %>% filter(n==100, parametro == "sd") %>%
  mutate(confianca = paste0("Confidence Level = ", confianca)) %>%
  group_by(metodo, confianca) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = median(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

g10 = ggplot(por_confianca10) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method (N = 10) (Parameter: Standard Deviation)") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~confianca) + ylim(0, 1)

g20 = ggplot(por_confianca20) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method (N = 20) (Parameter: Standard Deviation)") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~confianca) + ylim(0, 1)

g100 = ggplot(por_confianca100) +
  aes(x = fct_reorder(metodo, -cobertura), y = cobertura) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Coverage Rate",
       title = "Coverage Rate by Method (N = 100) (Parameter: Standard Deviation)") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(cobertura, 2)), vjust = -0.3) +
  facet_wrap(~confianca) + ylim(0, 1)

install.packages("ggpubr")
library(ggpubr)

ggarrange(g10, g20, g100 + font("x.text", size = 10),
          ncol = 1, nrow = 3)
