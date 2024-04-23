library(ggplot2)


ggplot(dados, aes(x=comprimento, y=metodo)) + 
  geom_boxplot() + labs(x = "Length", y = "Method", title = "Interval Length by Method") + 
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13))

ggplot(filter(dados, n==100), aes(x=comprimento, y=metodo)) + 
  geom_boxplot() + labs(x = "Length", y = "Method", title = "Interval Length by Method (N = 100)") + 
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13))

dados_resumidos = dados %>% group_by(parametro, metodo, distribuicao, n, confianca, alpha) %>% summarise(comprimento_medio = mean(comprimento), cobertura = sum(sucesso)/n())
ggplot(dados_resumidos, aes(x=comprimento_medio, y=metodo)) + 
  geom_boxplot() + labs(x = "Length", y = "Method", title = "Average Interval Length by Method") + 
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13))

#dados_media = dados %>% group_by(metodo) %>% summarise(comprimento_medio = mean(comprimento), cobertura = sum(sucesso)/n(), indicador = cobertura/(1+comprimento_medio)^(1/2))

ggplot(filter(dados, parametro == "mean"), aes(x=comprimento, y=metodo)) + 
  geom_boxplot() + labs(x = "Length", y = "Method", title = "Interval Length by Method (Parameter: mean)") + 
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13))

ggplot(filter(dados, parametro == "mean", n == 100), aes(x=comprimento, y=metodo)) + 
  geom_boxplot() + labs(x = "Length", y = "Method", title = "Interval Length by Method (Parameter: mean) (N = 100)") + 
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13))


geral <- dados %>%
  group_by(metodo) %>%
  summarise(cobertura = mean(sucesso),
            comprimento = mean(comprimento)) %>%
  ungroup() %>%
  mutate(indicador = cobertura/sqrt(1+comprimento))

ggplot(geral) +
  aes(x = fct_reorder(metodo, -comprimento), y = comprimento) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Mean Interval Length",
       title = "Mean Interval Length by Method") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(comprimento, 2)), vjust = -0.3)

ggplot(geral) +
  aes(x = fct_reorder(metodo, -indicador), y = indicador) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Indicator Value",
       title = "Performance Indicator by Method (using average length)") +
  theme(line = element_blank(), axis.ticks = element_line(), text = element_text(size = 13)) +
  geom_text(aes(label = round(indicador, 2)), vjust = -0.3)

########################


metodo_tabela = c()
indice_tabela = c()
quantil_tabela = c()


for(i in unique(dados$metodo)){
  xi = filter(dados, metodo == i)$comprimento
  metodo_tabela = c(metodo_tabela, rep(i, 99))
  indice_tabela = c(indice_tabela, seq(0.01,0.99,0.01))
  quantil_tabela = c(quantil_tabela, quantile(xi, seq(0.01,0.99,0.01)))
}

for(i in unique(dados$metodo)){
  xi = filter(dados, metodo == i)$comprimento
  metodo_tabela = c(metodo_tabela, rep(i, 90))
  indice_tabela = c(indice_tabela, seq(0.01,0.90,0.01))
  quantil_tabela = c(quantil_tabela, quantile(xi, seq(0.01,0.90,0.01)))
}

for(i in unique(dados$metodo)){
  xi = filter(dados, metodo == i)$comprimento
  metodo_tabela = c(metodo_tabela, rep(i, 999))
  indice_tabela = c(indice_tabela, seq(0.001,0.999,0.001))
  quantil_tabela = c(quantil_tabela, quantile(xi, seq(0.001,0.999,0.001)))
}

tabela = data.frame(metodo = metodo_tabela, indice = indice_tabela, quantil = quantil_tabela)

ggplot(tabela) + geom_point(aes(indice, quantil, color = metodo))

ggplot() + geom_point(aes(seq(0.01,0.99,0.01), quantile(dados$comprimento, seq(0.01,0.99,0.01))))






