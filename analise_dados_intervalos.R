library(tidyverse)
library(readxl)
setwd("~/artigo-bootstrap")

dados = read_excel("resultados_simulacao.xlsx")


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

melhor_cobertura2 = filter(melhor_cobertura, n == 5)
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, n == 20)
table(melhor_cobertura2$metodo)

melhor_cobertura2 = filter(melhor_cobertura, n == 100)
table(melhor_cobertura2$metodo)

# melhor comprimento

dados_resumidos %>% group_by(metodo) %>% summarise(comprimento = median(comprimento_mediano))

melhor_comprimento = dados_resumidos %>% group_by(parametro, distribuicao, n, confianca, alpha) %>% arrange(desc(comprimento_mediano)) %>%
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

melhor_comprimento2 = filter(melhor_comprimento, n == 5)
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

melhor_indicador2 = filter(melhor_indicador, n == 5)
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

pior_comprimento2 = filter(pior_comprimento, n == 5)
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
