#Padrões globais de gráficos

tamanho_da_letra <- 15
tamanho_do_pontinho <- 2.2

#Máscara pra colocar nos ggplots

padroes_esteticos <- theme_bw(tamanho_da_letra)

pontos <- geom_point(size = tamanho_do_pontinho)

# Constrói base

constroi_base_inferencial <- function(d_full, fluv){
  d_full %>% 
  filtra_posto_fluv(fluv) %>% 
  mutate(Pre = Precipitacao,
         l_Pre = lag(Precipitacao),
         l_2_Pre = lag(Precipitacao, 2),
         l_3_Pre = lag(Precipitacao, 3),
         l_4_Pre = lag(Precipitacao, 4),
         l_5_Pre = lag(Precipitacao, 5),
         l_6_Pre = lag(Precipitacao, 6),
         l_7_Pre = lag(Precipitacao, 7),
         l_IOS = lag(IOS),
         l_2_IOS = lag(IOS, 2),
         l_3_IOS = lag(IOS, 3),
         l_4_IOS = lag(IOS, 4),
         l_5_IOS = lag(IOS, 5),
         acum_la_nina = cumsum(Evento == 'La Niña'),
         acum_el_nino = cumsum(Evento == 'El Niño'),
         acum_la_nina = acum_la_nina - lag(acum_la_nina, 6),
         acum_el_nino = acum_el_nino - lag(acum_el_nino, 6),
         l_Evento = lag(Evento),
         l_Evento = factor(l_Evento, c('Nada','La Niña','El Niño')),
         lVazao = log(Vazao),
         lPre = log(Precipitacao),
         l_lPre = lag(lPre, 1),
         l_2_lPre = lag(lPre, 2),
         l_3_lPre = lag(lPre, 3),
         l_4_lPre = lag(lPre, 4),
         l_5_lPre = lag(lPre, 5),
         l_6_lPre = lag(lPre, 6),
         l_7_lPre = lag(lPre, 7),
         ano_estranho = ifelse(year(data) == c(1982,1983), 1, 0),
         sqrt_Pre = sqrt(Pre))}

constroi_base_teste <- function(d, ano_1, ano_2){
  
  d %>% 
    filter(year(data) <= ano_2, year(data) >= ano_1)
  
}

constroi_base_treino <- function(d, ano_1, ano_2){
  d %>% 
    filter(year(data) < ano_1| year(data) > ano_2)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                  labels = names(x)){
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))
  
  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }
  
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE
  
  if(!is.null(labels)){ 
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
  }
  
  p <- ggplot(df, aes(x=z, y=ord.x)) +
    geom_point() + 
    geom_abline(intercept = coef[1], slope = coef[2]) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
    padroes_esteticos +
    xlab('Quantis da normal') +
    ylab('Quantis dos resíduos')
    
  
  if(!is.null(labels)) p <- p + geom_text( aes(label = label))
  return(p)
}

fit_versus_obs <- function(model_fit, d = santa_maria, gamlss){
  
  linhas <- nrow(d)
  if(!gamlss){
    linhas_modelo <- nrow(model_fit$model)
  } else {
    linhas_modelo <- length(resid(model_fit))
  }  
  if(is.null(linhas_modelo)){
    linhas_modelo <- nrow(model_fit$xreg)
  }
  
  k <- c(1:(linhas-linhas_modelo))
  if(linhas != linhas_modelo){
    d2 <- d[-k,]
    datas <- d2$data
  } else {
    datas <- d$data
  }
  
  fitado <- fitted(model_fit)
  
  if(!gamlss){
    residuos <- resid(model_fit, type = "response")
  } else {
    residuos <- resid(model_fit, what = "mu", type = "simple")
  }
  
  obs <- fitado + residuos
  
  d <- data_frame(`Ajustado` = fitado, `Observado` = obs,
                  Data = datas) %>% 
    gather(Tipo, Valor, -Data) %>% 
    mutate(decenio = cut(year(Data), 4, labels = F),
           Tipo = factor(Tipo, levels = c('Observado','Ajustado')))
    
  p <- d %>% 
    ggplot(aes(y = Valor, x = Data, linetype = Tipo, color = Tipo)) +
    facet_wrap(~decenio, ncol = 2, scales = 'free') + 
    geom_line() +
    padroes_esteticos +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          legend.position = 'bottom')
  
  return(p)
}

residuos_no_tempo <- function(model_fit, d = santa_maria, gamlss){
  
  linhas <- nrow(d)
  if(!gamlss){
    linhas_modelo <- nrow(model_fit$model)
  } else {
    linhas_modelo <- length(resid(model_fit))
  }  
  if(is.null(linhas_modelo)){
    linhas_modelo <- nrow(model_fit$xreg)
  }
  
  k <- c(1:(linhas-linhas_modelo))
  if(linhas != linhas_modelo){
    datas <- santa_maria$data[-k]
  } else {
    datas <- santa_maria$data
  }
  
  if(!gamlss){
    residuos <- resid(model_fit, type = "response")
  } else {
    residuos <- resid(model_fit, type = "simple", what = "z-score")
  }
  
  d <- data_frame(Data = datas, `Resíduos` = residuos) %>% 
       mutate(decenio = cut(year(Data), 4, labels = F))
  
  p <- d %>% 
    ggplot(aes(y = `Resíduos`, x = Data)) +
    facet_wrap(~decenio, ncol = 2, scales = 'free_x') + 
    padroes_esteticos +
    pontos +
    theme(strip.background = element_blank(),
                      strip.text.x = element_blank())
  return(p)
}

acf_resid <- function(model_fit, d = santa_maria, gamlss){
  
  linhas <- nrow(d)
  if(!gamlss){
    linhas_modelo <- nrow(model_fit$model)
  } else {
    linhas_modelo <- length(resid(model_fit))
  }  
  if(is.null(linhas_modelo)){
    linhas_modelo <- nrow(model_fit$xreg)
  }
  
  k <- c(1:(linhas-linhas_modelo))
  if(linhas != linhas_modelo){
    datas <- santa_maria$data[-k]
  } else {
    datas <- santa_maria$data
  }
  
  if(!gamlss){
    residuos <- resid(model_fit, type = "response")
  } else {
    residuos <- resid(model_fit, type = "simple", what = "z-score")
  }  
  y_text <- 0.95
  x_text <- 7
  anotacao <- sprintf('Valor-p de Ljung-Box: %s', round(Box.test(residuos, type = 'Ljung-Box')$p.value, 2))
  
  p <- residuos %>% 
    acf(plot = F) %>% 
    ggfortify:::autoplot.acf() +
    geom_hline(yintercept = 0) +
    ylab('Auto-correlação') +
    xlab('Defasagem') +
    annotate('text', x = x_text, y = y_text, label = anotacao, size = 5) +
    padroes_esteticos 
  
  return(p)
}

fit_versus_resid <- function(model_fit, d = santa_maria, gamlss){
  
  linhas <- nrow(d)
  
  if(!gamlss){
    linhas_modelo <- nrow(model_fit$model)
  } else {
    linhas_modelo <- length(resid(model_fit))
  }
  
  if(is.null(linhas_modelo)){
    linhas_modelo <- nrow(model_fit$xreg)
  }
  
  k <- c(1:(linhas-linhas_modelo))
  if(linhas != linhas_modelo){
    datas <- santa_maria$data[-k]
  } else {
    datas <- santa_maria$data
  }
  
  fitado <- fitted(model_fit)
  if(!gamlss){
    residuos <- resid(model_fit, type = "response")
  } else {
    residuos <- resid(model_fit, type = "simple", what = "z-score")
  }
  
  d <- data_frame(`Ajustado` = fitado, `Resíduo` = residuos, Data = datas) %>% 
    mutate(decenio = cut(year(Data), 4, labels = F))
  
  p <- d %>% 
    ggplot(aes(y = `Resíduo`, x = `Ajustado`)) +
    padroes_esteticos +
    pontos
  
  return(p)
}

diagnostico <- function(model_fit, d = santa_maria, gamlss = F){
  
  if(!gamlss){
    residuos <- resid(model_fit, type = "response")
  } else {
    residuos <- resid(model_fit, type = "simple", what = "z-score")
  }
  
  multiplot(gg_qq(residuos),
            fit_versus_resid(model_fit, d, gamlss),
            acf_resid(model_fit, d, gamlss),
            residuos_no_tempo(model_fit, d, gamlss), cols = 2)

}

tabelaArima <- function(fit_ar){
  
  rotulo <- names(fit_ar$coef)
  coef <- fit_ar$coef
  se <- sqrt(diag(fit_ar$var.coef))
  t <- abs(coef)/se
  p_value <- (1-pnorm(t))/2
  
  resumo <- data_frame(rotulo, coef, se, t, p_value) %>% 
    mutate(p_value = round(p_value, 4),
           p_value = ifelse(p_value == 0, "<0.0001", p_value),
           se = round(se, 4),
           coef = round(coef, 4),
           t = round(t, 4))
  
  return(resumo)
}

tabelaGAMLSS <- function(fit_gamlss){
  
  matriz <- summary(fit_gamlss)
  
  passos <- (nrow(matriz)-1)/2
  
  rotulo_mu <- rownames(matriz)[1:passos]
  coef_mu <- matriz[1:passos, 1]
  se_mu <- matriz[1:passos, 2]
  t_mu <- matriz[1:passos, 3]
  p_value_mu <- matriz[1:passos, 4]
  
  resumo_mu <- data_frame(rotulo_mu, coef_mu, se_mu, t_mu, p_value_mu) %>% 
    mutate(p_value_mu = round(p_value_mu, 4),
           p_value_mu = ifelse(p_value_mu == 0, "<0.0001", p_value_mu),
           se_mu = round(se_mu, 4),
           coef_mu = round(coef_mu, 4),
           t_mu = round(t_mu, 4))
  
  rotulo_sigma <- rownames(matriz)[(passos+1):(2*passos)]
  coef_sigma <- matriz[(passos+1):(2*passos), 1]
  se_sigma <- matriz[(passos+1):(2*passos), 2]
  t_sigma <- matriz[(passos+1):(2*passos), 3]
  p_value_sigma <- matriz[(passos+1):(2*passos), 4]
  
  resumo_sigma <- data_frame(rotulo_sigma, coef_sigma, se_sigma, t_sigma, p_value_sigma) %>% 
    mutate(p_value_sigma = round(p_value_sigma, 4),
           p_value_sigma = ifelse(p_value_sigma == 0, "<0.0001", p_value_sigma),
           se_sigma = round(se_sigma, 4),
           coef_sigma = round(coef_sigma, 4),
           t_sigma = round(t_sigma, 4))
  
  return(list(resumo_mu = resumo_mu, resumo_sigma = resumo_sigma))  
}

plota_diag_modelos <- function(modelos, rotulo, G, n_inicial, var){
  
  for(i in seq(modelos)){
    
    arquivo <- sprintf("figuras/%s_%s_%s_diagnostico.png",
                       n_inicial, var, rotulo)
    
    png(filename = arquivo, width = 800, height = 600)
    
    diagnostico(modelos[[i]], gamlss = G[i])
    
    dev.off()
    
    n_inicial <- n_inicial + 1
    
    arquivo <- sprintf("figuras/%s_%s_%s_fit_versus_obs.png",
                       n_inicial, var, rotulo)
    
    p <- fit_versus_obs(modelos[[i]], gamlss = G[i])
  
    ggsave(arquivo, plot = p)
    
    n_inicial <- n_inicial + 1
    
  }
  
  return(n_inicial)
}