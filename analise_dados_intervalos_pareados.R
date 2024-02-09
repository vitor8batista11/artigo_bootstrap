library(tidyverse)
library(readxl)
setwd("~/artigo-bootstrap")

dados = read_excel("resultados_simulacao_pareado-1.xlsx")


inf = filter(dados, comprimento == "Inf")
unique(inf$metodo)
unique(inf$n)
unique(inf$confianca)
unique(inf$alpha)

dados_resumidos = dados %>% group_by(parametro, metodo, distribuicao, n, confianca, alpha) %>% summarise(comprimento_mediano = median(comprimento), cobertura = sum(sucesso)/n())
dados_resumidos = dados %>% group_by(parametro, metodo, distribuicao, n, confianca, alpha) %>% summarise(comprimento_medio = mean(comprimento), cobertura = sum(sucesso)/n())

dados_resumidos$indicador = dados_resumidos$cobertura / (1 + dados_resumidos$comprimento_mediano)

dados_alpha = dados_resumidos %>% group_by(alpha) %>% summarise(comprimento_medio = mean(comprimento_medio), cobertura = mean(cobertura))

# melhor cobertura

melhor_cobertura = dados_resumidos %>% group_by(parametro, distribuicao, n, confianca, alpha) %>% arrange(desc(cobertura)) %>%
  slice(1L)

table(melhor_cobertura$metodo)

melhor_cobertura2 = filter(melhor_cobertura, parametro == "mean")
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, parametro == "sd")
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, alpha == -1)
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, alpha == 0)
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, alpha == 1)
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, distribuicao == "normal")
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, distribuicao == "laplace")
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, distribuicao == "t")
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, distribuicao == "exponential")
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, distribuicao == "uniform")
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, n == 10)
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, n == 20)
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, n == 100)
table(melhor_cobertura2$metodo)

# melhor comprimento

dados_resumidos %>% group_by(metodo) %>% summarise(comprimento = median(comprimento_mediano))

melhor_comprimento = dados_resumidos %>% group_by(parametro, distribuicao, n, confianca, alpha) %>% arrange(desc(-comprimento_mediano)) %>%
  slice(1L)

table(melhor_comprimento$metodo)

melhor_comprimento2 = filter(melhor_comprimento, parametro == "mean")
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, parametro == "sd")
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, alpha == -1)
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, alpha == 0)
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, alpha == 1)
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, distribuicao == "normal")
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, distribuicao == "laplace")
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, distribuicao == "t")
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, distribuicao == "exponential")
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, distribuicao == "uniform")
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, n == 10)
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, n == 20)
table(melhor_comprimento2$metodo)

melhor_comprimento2 = filter(melhor_comprimento, n == 100)
table(melhor_comprimento2$metodo)

# indicador

melhor_indicador = dados_resumidos %>% group_by(parametro, distribuicao, n, confianca, alpha) %>% arrange(desc(indicador)) %>%
  slice(1L)

table(melhor_indicador$metodo)

melhor_indicador2 = filter(melhor_indicador, parametro == "mean")
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, parametro == "sd")
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, alpha == -1)
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, alpha == 0)
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, alpha == 1)
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, distribuicao == "normal")
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, distribuicao == "laplace")
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, distribuicao == "t")
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, distribuicao == "exponential")
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, distribuicao == "uniform")
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, n == 10)
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, n == 20)
table(melhor_indicador2$metodo)

melhor_indicador2 = filter(melhor_indicador, n == 100)
table(melhor_indicador2$metodo)

# pior comprimento

pior_comprimento = dados_resumidos %>% group_by(parametro, distribuicao, n, confianca, alpha) %>% arrange(desc(comprimento_mediano)) %>%
  slice(1L)

table(pior_comprimento$metodo)

pior_comprimento2 = filter(pior_comprimento, parametro == "mean")
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, parametro == "sd")
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, alpha == -1)
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, alpha == 0)
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, alpha == 1)
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, distribuicao == "normal")
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, distribuicao == "laplace")
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, distribuicao == "t")
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, distribuicao == "exponential")
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, distribuicao == "uniform")
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, n == 10)
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, n == 20)
table(pior_comprimento2$metodo)

pior_comprimento2 = filter(pior_comprimento, n == 100)
table(pior_comprimento2$metodo)


# impacto do alpha

dados_alpha = dados_resumidos %>% group_by(parametro, distribuicao, n, confianca, metodo) %>% arrange(desc(indicador)) %>%
  slice(1L)
dados_alpha = dados_resumidos %>% group_by(parametro, distribuicao, n, confianca, metodo) %>% arrange(desc(cobertura)) %>%
  slice(1L)
dados_alpha = dados_resumidos %>% group_by(parametro, distribuicao, n, confianca, metodo) %>% arrange(desc(-comprimento_mediano)) %>%
  slice(1L)

table(dados_alpha$alpha)
table(filter(dados_alpha, parametro == "mean")$alpha)
table(filter(dados_alpha, parametro == "sd")$alpha)

# mais analises

dados %>% group_by(metodo) %>% summarise(comprimento_mediano = median(comprimento), cobertura = sum(sucesso)/n(), indicador = cobertura/(1+comprimento_mediano))

dados %>% filter(n == 10) %>% group_by(metodo) %>% summarise(comprimento_mediano = median(comprimento), cobertura = sum(sucesso)/n(), indicador = cobertura/(1+comprimento_mediano))
dados %>% filter(n == 20) %>% group_by(metodo) %>% summarise(comprimento_mediano = median(comprimento), cobertura = sum(sucesso)/n(), indicador = cobertura/(1+comprimento_mediano))
dados %>% filter(n == 100) %>% group_by(metodo) %>% summarise(comprimento_mediano = median(comprimento), cobertura = sum(sucesso)/n(), indicador = cobertura/(1+comprimento_mediano))

dados %>% filter(alpha == 0) %>% group_by(metodo) %>% summarise(comprimento_mediano = median(comprimento), cobertura = sum(sucesso)/n(), indicador = cobertura/(1+comprimento_mediano))
dados %>% filter(alpha == -1) %>% group_by(metodo) %>% summarise(comprimento_mediano = median(comprimento), cobertura = sum(sucesso)/n(), indicador = cobertura/(1+comprimento_mediano))
dados %>% filter(alpha == 1) %>% group_by(metodo) %>% summarise(comprimento_mediano = median(comprimento), cobertura = sum(sucesso)/n(), indicador = cobertura/(1+comprimento_mediano))

# graficos

ggplot(teste, aes(metodo, comprimento_mediano)) +
  +     geom_col()

ggplot() + geom_bar(aes(melhor_cobertura$metodo))

ggplot(melhor_cobertura,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")

melhor_cobertura2 = filter(melhor_cobertura, parametro == "mean")
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (Média)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, parametro == "sd")
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (Desvio padrão)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, alpha == -1)
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (Alpha = -1)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, alpha == 0)
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (Alpha = 0)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, alpha == 1)
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (Alpha = 1)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, distribuicao == "normal")
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (Normal)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, distribuicao == "laplace")
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (Laplace)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, distribuicao == "t")
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (T)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, distribuicao == "exponential")
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (Exponencial)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, distribuicao == "uniform")
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (Uniforme)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, n == 10)
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (N = 10)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, n == 20)
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (N = 20)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_cobertura2 = filter(melhor_cobertura, n == 100)
ggplot(melhor_cobertura2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Maior taxa de cobertura (N = 100)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")

ggplot(melhor_comprimento,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")

melhor_comprimento2 = filter(melhor_comprimento, parametro == "mean")
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (Média)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, parametro == "sd")
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (Desvio padrão)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, alpha == -1)
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (Alpha = -1)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, alpha == 0)
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (Alpha = 0)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, alpha == 1)
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (Alpha = 1)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, distribuicao == "normal")
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (Normal)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, distribuicao == "laplace")
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (Laplace)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, distribuicao == "t")
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (T)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, distribuicao == "exponential")
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (Exponencial)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, distribuicao == "uniform")
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (Uniforme)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, n == 10)
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (N = 10)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, n == 20)
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (N = 20)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_comprimento2 = filter(melhor_comprimento, n == 100)
ggplot(melhor_comprimento2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor comprimento (N = 100)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")

ggplot(melhor_indicador,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")

melhor_indicador2 = filter(melhor_indicador, parametro == "mean")
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (Média)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, parametro == "sd")
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (Desvio padrão)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, alpha == -1)
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (Alpha = -1)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, alpha == 0)
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (Alpha = 0)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, alpha == 1)
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (Alpha = 1)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, distribuicao == "normal")
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (Normal)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, distribuicao == "laplace")
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (Laplace)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, distribuicao == "t")
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (T)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, distribuicao == "exponential")
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (Exponencial)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, distribuicao == "uniform")
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (Uniforme)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, n == 10)
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (N = 10)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, n == 20)
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (N = 20)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")


melhor_indicador2 = filter(melhor_indicador, n == 100)
ggplot(melhor_indicador2,
       aes(x=reorder(metodo,metodo,
                     function(x)-length(x)))) +
  geom_bar() + labs(x = "", y = "Quantidade", title = "Melhor indicador (N = 100)") + geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, colour = "black")
