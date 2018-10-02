source("R/2-modelos.R")
source("R/1-graficos_analise_descritiva.R")

precipitacao_sm <- modelos_precipitacao(santa_maria, 1970, 1986)
vazao_sm <- modelos_vazao(santa_maria, 2000, 2002)

precipitacao_gua <- modelos_precipitacao(guampara, 1980, 1990)
vazao_gua <- modelos_vazao(guampara, 2000, 2002)

precipitacao_pg <- modelos_precipitacao(porto_guarani, 1980, 1990)
vazao_pg <- modelos_vazao(porto_guarani, 2000, 2002)

ultimo_grafico <- 50+1

fited_pre <- precipitacao_sm[c("modelo_1","modelo_2","modelo_3","modelo_4")]

gamlss_pre <- c(F,F,T,T)

fited_vazao <- vazao_sm[c("modelo_1","modelo_2","modelo_3","modelo_4",
                       "modelo_5","modelo_6","modelo_7")]

gamlss_va <- rep(F,7)

N <- plota_diag_modelos(fited_pre, "p", gamlss_pre, ultimo_grafico, "Santa Maria")

N <- plota_diag_modelos(fited_vazao, "v", gamlss_va, N, "Santa Maria")

fited_pre <- precipitacao_gua[c("modelo_1","modelo_2","modelo_3","modelo_4")]

gamlss_pre <- c(F,F,T,T)

fited_vazao <- vazao_sm[c("modelo_1","modelo_2","modelo_3","modelo_4",
                          "modelo_5","modelo_6","modelo_7")]

gamlss_va <- rep(F,7)

N <- plota_diag_modelos(fited_pre, "p", gamlss_pre, N, "Guampará")

N <- plota_diag_modelos(fited_vazao, "v", gamlss_va , N, "Guamparás")

fited_pre <- precipitacao_pg[c("modelo_1","modelo_2","modelo_3","modelo_4")]

gamlss_pre <- c(F,F,T,T)

fited_vazao <- vazao_sm[c("modelo_1","modelo_2","modelo_3","modelo_4",
                          "modelo_5","modelo_6","modelo_7")]

gamlss_va <- rep(F,7)

N <- plota_diag_modelos(fited_pre, "p", gamlss_pre, N, "Porto Guarani")

N <- plota_diag_modelos(fited_vazao, "v", gamlss_va, N, "Porto Guarani")

