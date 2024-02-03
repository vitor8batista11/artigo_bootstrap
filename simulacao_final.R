library(tidyverse)
load("functions.Rdata")

# Função que faz a simulação para um dado método, uma distribuição, um nível de confiança e um tamanho amostral

simulacao <- function(distribution, average, variance = 1, alpha, method, n, confidence, B = 100, B_2 = 100, n_sim = 100, parameter){
  
  # Vetor que vai armazenar se o intervalo contém a média ou não
  sucessos <- numeric(n_sim)
  
  # Vetor que vai armazenar o comprimento do intervalo
  comprimentos <- numeric(n_sim)
  
  # Vetores que vão armazenar os limites do intervalo
  lower <- numeric(n_sim); upper <- numeric(n_sim)
  
  for(j in 1:n_sim){
    
    # Gerando a amostra original
    amostra <- generate_sample(distribution, average, variance, alpha, n)
    
    # intervalos
    if(method == 'better'){
      intrvl <- better_interval(amostra, B, confidence, parameter)
    }else if(method == 'bayesian'){
      intrvl <- bayesian_interval(amostra, B, confidence, parameter)
    }else if(method == 'studentized'){
      intrvl <- boot_t_interval(amostra, B, B_2, confidence, parameter)
    }else if(method == 'normal'){
      intrvl <- normal_interval(amostra, B, confidence, parameter)
    }else if(method == 'quantile'){
      intrvl <- quantile_interval(amostra, B, confidence, parameter)
    }
    
    # Verificando se o intervalo contém a média ou não
    if(parameter == "mean"){
      if(between(average, intrvl[1], intrvl[2])) sucessos[j] <- TRUE
      else sucessos[j] <- FALSE
    }
    if(parameter == "sd"){
      if(distribution == "exponential"){
        if(between(average, intrvl[1], intrvl[2])) sucessos[j] <- TRUE
        else sucessos[j] <- FALSE
      }else{
        if(between(sqrt(variance), intrvl[1], intrvl[2])) sucessos[j] <- TRUE
        else sucessos[j] <- FALSE
      }
    }
    
    comprimentos[j] <- intrvl[2] - intrvl[1]
    lower[j] <- intrvl[1]
    upper[j] <- intrvl[2]
    
  }
  
  df_infos <- data.frame(intervalo = 1:n_sim,
                         parametro = rep(parameter, n_sim),
                         metodo = rep(method, n_sim),
                         distribuicao = rep(distribution, n_sim),
                         n = rep(n, n_sim),
                         confianca = rep(confidence, n_sim),
                         alpha = rep(alpha, n_sim),
                         lim_inf = lower,
                         lim_sup = upper,
                         sucesso = sucessos,
                         comprimento = comprimentos)
  
  return(df_infos)
  
}

simulacao(distribution = 'exponential',
          average = 5,
          alpha = 0,
          method = 'better',
          confidence = 0.95,
          n = 100,
          B = 100,
          n_sim = 100,
          parameter = "sd")

# Função que roda a simulação para os parâmetros do artigo

simulacao_artigo <- function(distributions = c('normal', 'exponential', 'laplace', 'uniform', 't'),
                             methods = c('better', 'bayesian', 'studentized', 'normal', 'quantile'),
                             ns = c(5, 20, 100), confidence_levels = c(0.8, 0.95, 0.99),
                             alphas = c(-1, 0, 1), parameters = c('mean', 'sd'),
                             average, variance = 4, B = 100, B_2 = 100, n_sim = 100, seed = 42){
  
  df_resultados <- data.frame(intervalo = numeric(),
                              parametro = character(),
                              metodo = character(),
                              distribuicao = character(),
                              n = numeric(),
                              confianca = numeric(),
                              alpha = numeric(),
                              lim_inf = numeric(),
                              lim_sup = numeric(),
                              sucesso = numeric(),
                              comprimento = numeric())
  
  set.seed(seed)
  
  for(parameter in parameters) {
    
    cat('#############################\n')
    cat('PARAMETRO:', parameter, '\n')
    cat('#############################\n\n')
    
    for(method in methods){
      
      cat('MÉTODO:', method, '-------------\n\n')
      
      for(distribution in distributions){
        
        cat('DISTRIBUIÇÃO:', distribution, '\n\n')
        
        for(confidence in confidence_levels){
          
          cat('Nível de Confiança:', confidence, '\n\n')
          
          for(alpha in alphas){
            
            cat('Alpha:', alpha, '\n')
            
            for(n in ns){
              
              resultados <- simulacao(distribution, average, variance, alpha, method, n, confidence, B, B_2, n_sim, parameter)
              df_resultados <- bind_rows(df_resultados, resultados)
              sucessos <- resultados$sucesso
              comprimentos <- resultados$comprimento
              cat('n = ', n, ': ', 100*mean(sucessos),
                  '% de sucessos, amplitude média de ',
                  round(mean(comprimentos),2), '\n', sep = '')
              
            }
            
            cat('\n')  
            
          }
          
        }
        
      }
      
    }  
    
  }
  
  return(df_resultados)
  
}

simulacao_artigo_pareado <- function(distributions = c('normal', 'exponential', 'laplace', 'uniform', 't'),
                                     ns = c(10, 20, 100), confidence_levels = c(0.8, 0.95, 0.99),
                                     alphas = c(-1, 0, 1), parameters = c('mean', 'sd'),
                                     average, variance = 4, B = 100, B_2 = 100, n_sim = 100, seed = 42){
  
  df_resultados <- data.frame(intervalo = numeric(),
                              parametro = character(),
                              metodo = character(),
                              distribuicao = character(),
                              n = numeric(),
                              confianca = numeric(),
                              alpha = numeric(),
                              lim_inf = numeric(),
                              lim_sup = numeric(),
                              sucesso = numeric(),
                              comprimento = numeric())
  
  set.seed(seed)
  
  for(distribution in distributions) {
    
    cat('Distribuição ', distribution, ', ', sep = '')
    
    for(n in ns){
      
      cat('N = ', n, ', ', sep = '')
      
      for(alpha in alphas){
        
        cat('Alpha = ', alpha, ' ... ', sep = '')
        amostras <- replicate(n_sim, generate_sample(distribution, average, variance, alpha, n))
        
        for(parameter in parameters){
          
          for(confidence_level in confidence_levels){
            
            intervalos_bayesian <- apply(amostras, MARGIN = 2, function(amostra) bayesian_interval(amostra, B, confidence_level, parameter)) %>%
              t() %>% data.frame() %>%
              mutate(metodo = 'bayesian', intervalo = 1:n_sim)
            intervalos_better <- apply(amostras, MARGIN = 2, function(amostra) better_interval(amostra, B, confidence_level, parameter)) %>%
              t() %>% data.frame() %>%
              mutate(metodo = 'better', intervalo = 1:n_sim)
            intervalos_quantile <- apply(amostras, MARGIN = 2, function(amostra) quantile_interval(amostra, B, confidence_level, parameter)) %>%
              t() %>% data.frame() %>%
              mutate(metodo = 'quantile', intervalo = 1:n_sim)
            intervalos_normal <- apply(amostras, MARGIN = 2, function(amostra) normal_interval(amostra, B, confidence_level, parameter)) %>%
              t() %>% data.frame() %>%
              mutate(metodo = 'normal', intervalo = 1:n_sim)
            intervalos_studentized <- apply(amostras, MARGIN = 2, function(amostra) boot_t_interval(amostra, B, B_2, confidence_level, parameter)) %>%
              t() %>% data.frame() %>%
              mutate(metodo = 'studentized', intervalo = 1:n_sim)
            
            resultados <- bind_rows(intervalos_bayesian, intervalos_better, intervalos_quantile,
                                    intervalos_normal, intervalos_studentized) %>%
              transmute(intervalo,
                        metodo,
                        lim_inf = X1,
                        lim_sup = X2,
                        distribuicao = distribution,
                        parametro = parameter,
                        n = n,
                        confianca = confidence_level,
                        alpha = alpha,
                        comprimento = X2 - X1)
            
            if(parameter == 'mean'){
              
              resultados <- resultados %>%
                mutate(true_value = average) %>%
                mutate(sucesso = ifelse(between(true_value, lim_inf, lim_sup), 1, 0)) %>%
                select(-true_value)
              
            }else{
              
              resultados <- resultados %>%
                mutate(true_value = sqrt(variance)) %>%
                mutate(sucesso = ifelse(between(true_value, lim_inf, lim_sup), 1, 0)) %>%
                select(-true_value)
              
            }
            
            df_resultados <- bind_rows(df_resultados, resultados)
            
            cat('FEITO!\n')
            
          }
          
        }
        
      }
      
    }
    
  }
  
  return(df_resultados)
  
}

results <- simulacao_artigo(average = 5, variance = 4, B = 100, B_2 = 100, n_sim = 100)
results_pareado <- simulacao_artigo_pareado(average = 5, variance = 4, B = 100, B_2 = 100, n_sim = 100)
  
writexl::write_xlsx(results, "resultados_simulacao.xlsx")
writexl::write_xlsx(results_pareado, "resultados_simulacao_pareado.xlsx")
