############################################################
###### gerador das amostras ################################
############################################################

# funcao que gera a amostra

# obs: para a exponencial, especificar apenas a media
# obs: para a t, especificar apenas a variancia - senao, sera gerada uma t nao central
# obs: alpha é um parâmetro entre -1 e 1 que controla a dependencia; alpha=0 implica independencia

generate_sample = function(distribution, average, variance, alpha, n){
  if(alpha < -1 | alpha > 1){
    return("Error: alpha must be in [-1, 1] range")
  }else{
    if(alpha == 0){
      u = runif(n)
    }else{
      u = runif(1)
      for(i in 1:(n-1)){
        Fau = runif(1)
        Ux = u[length(u)]
        Un = v = (1 + alpha*(1-(2*Ux)) - sqrt((1 + alpha*(1-(2*Ux)))^2 - 4*Fau*alpha*(1-(2*Ux)))) / (2*alpha*(1-(2*Ux)))
        u = c(u, Un)
      }
    }
    if(distribution == "normal"){
      x = qnorm(p=u, mean = average, sd = sqrt(variance))
    }
    if(distribution == "exponential"){
      x = qexp(p=u, rate = 1/average)
    }
    if(distribution == "uniform"){
      x = qunif(p=u, min = average - sqrt(3*variance), max = average + sqrt(3*variance))
    }
    if(distribution == "t"){
      x = average + qt(p=u, df = 2*(variance/(variance-1)))
    }
    if(distribution == "laplace"){
      x = c()
      for(i in 1:n){
        if(u[i] < 1/2){
          xi = average + log(2*u[i])*sqrt(variance/2)
        }else{
          xi = average - log(2-(2*u[i]))*sqrt(variance/2)
        }
        x = c(x, xi)
      }
    }
    return(x)
  }
}

# testando o codigo:

dados = generate_sample(distribution = "laplace", average = 0, variance = 16, alpha = 1, n = 10000)

plot(dados)
hist(dados)
mean(dados)
sd(dados)

# pela teoria de copulas, a correlacao de spearman entre duas observacoes consecutivas ? alpha/3. Verifique:

plot(dados[1:(length(dados)-1)], dados[2:length(dados)])
cor(dados[1:(length(dados)-1)], dados[2:length(dados)], method = 'spearman')

############################################################
###### gerador dos intervalos ##############################
############################################################

# ao contrario da funcao "sd" do R, essa funcao calcular o estimador plug-in
# isto e, usamos n no deminador (e n?o n-1)

sd_plug = function(x){return(sqrt(sum((x-mean(x))^2)/length(x)))}

# normal interval:

normal_interval = function(amostra, B, confidence, parameter){
  qinf = (1-confidence)/2
  qsup = confidence + qinf
  z_value = abs(qnorm(qinf))
  estimates = numeric(B)
  if(parameter == "mean"){
    for(i in 1:B){
      resample <- sample(amostra, size = length(amostra), replace = TRUE)
      estimates[i] <- mean(resample)
    }
  }
  if(parameter == "sd"){
    for(i in 1:B){
      resample <- sample(amostra, size = length(amostra), replace = TRUE)
      estimates[i] <- sd_plug(resample)
    }
  }
  q1 = mean(estimates) - z_value*sd(estimates)
  q2 = mean(estimates) + z_value*sd(estimates)
  return(c(q1, q2))
}

normal_interval(amostra = dados, B = 200, confidence = 0.95, parameter = "mean")
normal_interval(amostra = dados, B = 200, confidence = 0.95, parameter = "sd")

# bayesian interval:

rdirch = function(n){
  x = c(0, runif(n-1), 1)
  x = sort(x)
  x = diff(x)
  return(x)
}

bayesian_interval = function(amostra, B, confidence, parameter){
  qinf = (1-confidence)/2
  qsup = confidence + qinf
  estimates = c()
  if(parameter == "mean"){
    for(i in 1:B){
      theta = rdirch(length(amostra))
      estimates = c(estimates, sum(theta*amostra))
    }
  }
  if(parameter == "sd"){
    for(i in 1:B){
      theta = rdirch(length(amostra))
      sam_mean = sum(theta*amostra)
      estimates = c(estimates, sqrt(sum(theta * (amostra - sam_mean)^2 )))
    }
  } 
  q1 = as.numeric(quantile(estimates, qinf))
  q2 = as.numeric(quantile(estimates, qsup))
  return(c(q1, q2))
}

bayesian_interval(amostra = dados, B = 200, confidence = 0.95, parameter = "mean")
bayesian_interval(amostra = dados, B = 200, confidence = 0.95, parameter = "sd")


# bootstrap-t interval:

boot_t_interval = function(amostra, B, B_2, confidence, parameter){
  qinf = (1-confidence)/2
  qsup = confidence + qinf
  estimates = c()
  ses = c()
  for(i in 1:B){
    x = sample(amostra, size = length(amostra), replace = T)
    if(parameter == "mean"){
      estimates = c(estimates, mean(x))
      nested_estimates = c()
      for (j in 1:(B_2)) {
        xn = sample(x, size = length(x), replace = T)
        nested_estimates = c(nested_estimates, mean(xn))
      }
    }
    if(parameter == "sd"){
      estimates = c(estimates, sd_plug(x))
      nested_estimates = c()
      for (j in 1:(B_2)) {
        xn = sample(x, size = length(x), replace = T)
        nested_estimates = c(nested_estimates, sd_plug(xn))
      }
    }
    se = sd_plug(nested_estimates)
    ses = c(ses, se)
  }
  if(parameter == "mean"){
    z = (estimates-mean(amostra))/ses
    se_global = sqrt(sum((estimates - mean(amostra))^2)/B)
    q1 = mean(amostra) - (as.numeric(quantile(z, qsup)) * se_global)
    q2 = mean(amostra) - (as.numeric(quantile(z, qinf)) * se_global)
  }
  if(parameter == "sd"){
    z = (estimates - sd_plug(amostra))/ses
    se_global = sqrt(sum((estimates - sd_plug(amostra))^2)/B)
    q1 = sd_plug(amostra) - (as.numeric(quantile(z, qsup)) * se_global)
    q2 = sd_plug(amostra) - (as.numeric(quantile(z, qinf)) * se_global)
  }
  return(c(q1, q2))
}

boot_t_interval(amostra = dados, B = 100, B_2 = 20, confidence = 0.95, parameter = "mean")
boot_t_interval(amostra = dados, B = 100, B_2 = 20, confidence = 0.95, parameter = "sd")


# quantile interval:

quantile_interval = function(amostra, B, confidence, parameter){
  qinf = (1-confidence)/2
  qsup = confidence + qinf
  estimates = c()
  if(parameter == "mean"){
    for(i in 1:B){
      x = sample(amostra, size = length(amostra), replace = T)
      estimates = c(estimates, mean(x))
    }
  }
  if(parameter == "sd"){
    for(i in 1:B){
      x = sample(amostra, size = length(amostra), replace = T)
      estimates = c(estimates, sd_plug(x))
    }
  }
  q1 = as.numeric(quantile(estimates, qinf))
  q2 = as.numeric(quantile(estimates, qsup))
  return(c(q1, q2))
}

quantile_interval(amostra = dados, B = 200, confidence = 0.95, parameter = "mean")
quantile_interval(amostra = dados, B = 200, confidence = 0.95, parameter = "sd")

# better bootstrap:

better_interval = function(amostra, B, confidence, parameter){
  sd_plug = function(x){return(sqrt(sum((x-mean(x))^2)/length(x)))}
  qinf = (1-confidence)/2
  qsup = confidence + qinf
  estimates = c()
  if(parameter == "mean"){
    for(i in 1:B){
      x = sample(amostra, size = length(amostra), replace = T)
      estimates = c(estimates, mean(x))
    }
  }
  if(parameter == "sd"){
    for(i in 1:B){
      x = sample(amostra, size = length(amostra), replace = T)
      estimates = c(estimates, sd_plug(x))
    }
  }
  est_del = c()
  if(parameter == "mean"){
    for(i in 1:B){
      est_del = c(est_del, mean(amostra[-i]))
    }
    z0 = qnorm(sum(estimates < mean(amostra))/B)
    a0 = sum((mean(est_del) - est_del)^3) / (6*(sum((mean(est_del) - est_del)^2))^(3/2))
  }
  if(parameter == "sd"){
    for(i in 1:B){
      est_del = c(est_del, sd_plug(amostra[-i]))
    }
    z0 = qnorm(sum(estimates < sd_plug(amostra))/B)
    a0 = sum((mean(est_del) - est_del)^3) / (6*(sum((mean(est_del) - est_del)^2))^(3/2))
  }
  
  alpha1 = pnorm(z0 + ((z0 + as.numeric(qnorm(qinf))))/(1 - (a0*(z0 + as.numeric(qnorm(qinf))))))
  alpha2 = pnorm(z0 + ((z0 + as.numeric(qnorm(qsup))))/(1 - (a0*(z0 + as.numeric(qnorm(qsup))))))
  
  q1 = as.numeric(quantile(estimates, alpha1))
  q2 = as.numeric(quantile(estimates, alpha2))
  return(c(q1, q2))
}


better_interval(amostra = dados, B = 200, confidence = 0.95, param = "mean")
better_interval(amostra = dados, B = 200, confidence = 0.95, param = "sd")


save(rdirch, sd_plug, quantile_interval, generate_sample, boot_t_interval,
     better_interval, bayesian_interval, normal_interval, file = 'functions.Rdata')
