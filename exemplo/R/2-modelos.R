source('R/0-load.R')
source('R/funcoes/analise_descritiva.R')
source('R/funcoes/limpeza_de_dados.R')
source('R/funcoes/analise_inferencial.R')
source('R/0-depara_pluv_fluv.R')

completo <- readRDS('dados/completo.rds')

santa_maria <- constroi_base_inferencial(completo, "Santa Maria")
guampara <- constroi_base_inferencial(completo, "Guampará")
porto_guarani <- constroi_base_inferencial(completo, "Porto Guarani")

ano_1 <- 2002
ano_2 <- 2002

# train_sm <- constroi_base_treino(santa_maria, ano_1, ano_2)
# train_g <- constroi_base_treino(guampara, ano_1, ano_2)
# train_pg <- constroi_base_treino(porto_guarani, ano_1, ano_2)
# 
# test_sm <- constroi_base_teste(santa_maria, ano_1, ano_2)
# test_g <- constroi_base_teste(santa_maria, ano_1, ano_2)
# test_pg <- constroi_base_teste(santa_maria, ano_1, ano_2)

modelos_precipitacao <- function(d, ano_t_1, ano_t_2){

  lm_pre_1 <- lm(Pre ~ l_IOS + Mes, data = d)

  lm_pre_bp_1 <- lmtest::bptest(lm_pre_1)
  
  lm_pre_2 <- lm(lPre ~ l_IOS + Mes, data = d)
  
  lm_pre_bp_2 <- lmtest::bptest(lm_pre_2)
  
  d2 <- d %>% 
    select(Pre, l_IOS, Mes) %>% 
    filter(!is.na(l_IOS)) %>% 
    as.data.frame
  
  gamlss_pre_3 <- gamlss(Pre ~ l_IOS + Mes, data = d2,
                     sigma.formula = ~l_IOS + Mes, family = NO())
  
  gamlss_pre_4 <- gamlss(Pre ~ l_IOS + Mes, data = d2,
                         sigma.formula = ~l_IOS + Mes, family = GG(),
                         method = RS(50))
  
  treino <- constroi_base_treino(d, ano_t_1, ano_t_2) %>% 
    select(Pre, l_IOS, Mes) %>% 
    filter(!is.na(l_IOS)) %>% 
    as.data.frame()
  
  teste <- constroi_base_teste(d, ano_t_1, ano_t_2) %>%
    select(Pre, l_IOS, Mes) %>% 
    filter(!is.na(l_IOS)) %>% 
    as.data.frame()
  
  gamlss_pre_3_cv <- gamlss(Pre ~ l_IOS + Mes, data = treino,
                         sigma.formula = ~l_IOS + Mes, family = NO())
  
  gamlss_pre_4_cv <- gamlss(Pre ~ l_IOS + Mes, data = treino,
                           sigma.formula = ~l_IOS + Mes, family = GG(),
                           method = RS(100))
  
  gamlss_pre_4_cv_1 <- gamlss(Pre ~ Mes, data = treino,
                         sigma.formula = ~ Mes, family = GG(),
                         method = RS(50))
  
  gamlss_pre_4_cv_2 <- gamlss(Pre ~ l_IOS + Mes, data = treino,
                            sigma.formula = ~ Mes, family = GG(),
                            method = RS(50))
  
  gamlss_pre_4_cv_3 <- gamlss(Pre ~ Mes, data = treino,
                              sigma.formula = ~ Mes + l_IOS, family = GG(),
                              method = RS(50))
  
  erro_3 <- mean((teste$Pre - predict(gamlss_pre_3_cv, newdata = teste, type = 'response', data = treino))^2)
  erro_4 <- mean((teste$Pre - predict(gamlss_pre_4_cv, newdata = teste, type = 'response', data = treino))^2)
  
  erro_4_1 <- mean((teste$Pre - predict(gamlss_pre_4_cv_1,newdata = teste, type = "response", data = treino))^2)
  erro_4_2 <- mean((teste$Pre - predict(gamlss_pre_4_cv_2,newdata = teste, type = 'response', data = treino))^2)
  erro_4_3 <- mean((teste$Pre - predict(gamlss_pre_4_cv_3,newdata = teste, type = 'response', data = treino))^2)
  
  return(list(
    modelo_1 = lm_pre_1,
    bp_1 = lm_pre_bp_1,
    modelo_2 = lm_pre_2,
    bp_2 = lm_pre_bp_2,
    modelo_3 = gamlss_pre_3,
    modelo_4 = gamlss_pre_4,
    erro_3 = erro_3,
    erro_4 = erro_4,
    modelo_4_3 = gamlss_pre_4_cv_3,
    modelo_4_2 = gamlss_pre_4_cv_2,
    modelo_4_1 = gamlss_pre_4_cv_1,
    erro_4_1 = erro_4_1,
    erro_4_2 = erro_4_2,
    erro_4_3 = erro_4_3
  ))  
}

modelos_vazao <- function(d, ano_t_1, ano_t_2){
 
  treino <- constroi_base_treino(d, ano_t_1, ano_t_2) %>% 
    filter(!is.na(l_IOS)) %>% 
    as.data.frame()
  
  teste <- constroi_base_teste(d, ano_t_1, ano_t_2) %>%
    filter(!is.na(l_IOS)) %>% 
    as.data.frame()
  
  fit_lm_y <- lm(Vazao ~ l_IOS +
                   Pre +
                   l_Pre +
                   l_2_Pre +
                   Mes,
                 data = santa_maria,
                 contrasts = list(Mes = "contr.sum"))
  
  fit_lm_ly <- lm(lVazao ~ l_IOS +
                    Pre +
                    l_Pre +
                    l_2_Pre +
                    Mes,
                  data = santa_maria)
  
  XO1 <- model.matrix(~ Mes +
                       Pre +
                       l_Pre +
                       l_2_Pre +
                       l_IOS,
                     data = santa_maria)
  
  lY <- santa_maria$lVazao[-c(1:2)]
  
  fit_ar_1 <- Arima(lY, order = c(1,0,0), xreg = XO1,
                   include.mean = F)
  
  fit_bp_test <- lm(fit_ar_1$residuals ~ XO1)
  
  bp_test <- anova(fit_bp_test)[[5]][[1]]
  
  XO2 <- model.matrix(~ Mes +
                       Pre +
                       l_Pre +
                       l_2_Pre +
                       l_3_Pre +
                       l_IOS,
                     data = santa_maria)
  
  lY <- santa_maria$lVazao[-c(1:3)]
  
  fit_ar_2 <- Arima(lY, order = c(1,0,0), xreg = XO2,
                   include.mean = F)
  
  XO3 <- model.matrix(~ Mes +
                       Pre +
                       l_Pre +
                       l_2_Pre +
                       l_3_Pre +
                       l_4_Pre +
                       l_IOS,
                     data = santa_maria)
  
  lY <- santa_maria$lVazao[-c(1:4)]
  
  fit_ar_3 <- Arima(lY, order = c(1,0,0), xreg = XO3,
                    include.mean = F)
  
  XO4 <- model.matrix(~ Mes +
                        Pre +
                        l_Pre +
                        l_2_Pre +
                        l_3_Pre +
                        l_4_Pre +
                        l_5_Pre +
                        l_IOS,
                      data = santa_maria)
  
  lY <- santa_maria$lVazao[-c(1:5)]
  
  fit_ar_4 <- Arima(lY, order = c(1,0,0), xreg = XO4,
                    include.mean = F)
  
  XO5 <- model.matrix(~ Mes +
                        Pre +
                        l_Pre +
                        l_2_Pre +
                        l_3_Pre +
                        l_4_Pre +
                        l_5_Pre +
                        l_6_Pre + 
                        l_IOS,
                      data = santa_maria)
  
  lY <- santa_maria$lVazao[-c(1:6)]
  
  fit_ar_5 <- Arima(lY, order = c(1,0,0), xreg = XO5,
                    include.mean = F)
  
  XO_1 <- model.matrix(~ Mes +
                         Pre +
                         l_Pre +
                         l_2_Pre +
                         l_IOS,
                       data = teste)
  
  XO_2 <- model.matrix(~ Mes +
                         Pre +
                         l_Pre +
                         l_2_Pre +
                         l_3_Pre +
                         l_IOS,
                       data = teste)
  
  XO_3 <- model.matrix(~ Mes +
                         Pre +
                         l_Pre +
                         l_2_Pre +
                         l_3_Pre + 
                         l_4_Pre +
                         l_IOS,
                       data = teste)

  XO_4 <- model.matrix(~ Mes +
                         Pre +
                         l_Pre +
                         l_2_Pre +
                         l_3_Pre + 
                         l_4_Pre +
                         l_5_Pre +
                         l_IOS,
                       data = teste)
  
  XO_5 <- model.matrix(~ Mes +
                         Pre +
                         l_Pre +
                         l_2_Pre +
                         l_3_Pre + 
                         l_4_Pre +
                         l_5_Pre +
                         l_6_Pre + 
                         l_IOS,
                       data = teste)
  
  erro_1 <- mean((teste$Vazao - exp(predict(fit_ar_1, newxreg = XO_1)$pred))^2)
  erro_2 <- mean((teste$Vazao - exp(predict(fit_ar_2, newxreg = XO_2)$pred))^2)
  erro_3 <- mean((teste$Vazao - exp(predict(fit_ar_3, newxreg = XO_3)$pred))^2)
  erro_4 <- mean((teste$Vazao - exp(predict(fit_ar_4, newxreg = XO_4)$pred))^2)
  erro_5 <- mean((teste$Vazao - exp(predict(fit_ar_5, newxreg = XO_5)$pred))^2)
  
  erros <- c(erro_1, erro_2, erro_3, erro_4, erro_5)
  
  return(list(
    modelo_1 = fit_lm_y,
    modelo_2 = fit_lm_ly,
    modelo_3 = fit_ar_1,
    bp_test_1 = bp_test,
    modelo_4 = fit_ar_2,
    modelo_5 = fit_ar_3,
    modelo_6 = fit_ar_4,
    modelo_7 = fit_ar_5,
    erros = erros
  ))  
}

