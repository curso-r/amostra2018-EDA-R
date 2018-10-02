source("R/2-modelos.R")
source("R/1-graficos_analise_descritiva.R")

precipitacao_sm <- modelos_precipitacao(santa_maria, 1970, 1986)
vazao_sm <- modelos_vazao(santa_maria, 2000, 2002)

precipitacao_gua <- modelos_precipitacao(guampara, 1980, 1990)
vazao_gua <- modelos_vazao(guampara, 2000, 2002)

precipitacao_pg <- modelos_precipitacao(porto_guarani, 1980, 1990)
vazao_pg <- modelos_vazao(porto_guarani, 2000, 2002)

pre_sm <- precipitacao_sm[c("modelo_1","modelo_2","modelo_3","modelo_4")]
pre_gua <- precipitacao_gua[c("modelo_1","modelo_2","modelo_3","modelo_4")]
pre_pg <- precipitacao_pg[c("modelo_1","modelo_2","modelo_3","modelo_4")]

vaz_sm <- precipitacao_sm[c("modelo_1","modelo_2","modelo_3","modelo_4",
                            "modelo_5","modelo_6","modelo_7")]

vaz_gua <- precipitacao_gua[c("modelo_1","modelo_2","modelo_3","modelo_4",
                             "modelo_5","modelo_6","modelo_7")]

vaz_pg <- precipitacao_pg[c("modelo_1","modelo_2","modelo_3","modelo_4",
                           "modelo_5","modelo_6","modelo_7")]

gamlss_pre_4 <- tabelaGAMLSS(precipitacao_sm$modelo_4)
gamlss_pre_3 <- tabelaGAMLSS(precipitacao_sm$modelo_3)

gamlss_vaz_ <- tabelaGAMLSS(precipitacao_sm$modelo_3)

