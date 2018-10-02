source('exemplo/R/0-load.R')
source('exemplo/R/0-depara_pluv_fluv.R')
source('exemplo/R/funcoes/analise_descritiva.R')
source('exemplo/R/funcoes/limpeza_de_dados.R')

completo <- readRDS('exemplo/dados/completo.rds')

todos_os_graficos <- completo %>% 
  group_by(Posto.Fluv) %>% 
  do(mod = filtra_posto_fluv(., posto = .$Posto.Fluv)) %>% 
  do(Posto.Fluv = .$Posto.Fluv,
     vazao = plota_vazao(.$mod),
     acf_v = plota_acf_vazao(.$mod),
     boxplot_vazao = sazonalidade_vazao(.$mod),
     precp = plota_precipitacao(.$mod),
     acf_p = plota_acf_precipitacao(.$mod),
     boxplot_precp = sazonalidade_precipitacao(.$mod),
     v_p_sem_lag = plota_vazao_versus_precipitacao(.$mod),
     v_p_lag_1 = plota_vazao_versus_precipitacao(.$mod, l = 1),
     ccf_v_p = plota_ccf_vazao_precipitacao(.$mod),
     v_i_sem_lag = plota_IOS_e_vazao(.$mod, 0),
     v_i_lag_1 = plota_IOS_e_vazao(.$mod, 1),
     ccf_v_i = plota_ccf_vazao_IOS(.$mod),
     ccf_p_i = plota_ccf_precipitacao_IOS(.$mod),
     p_i_sem_lag = plota_IOS_e_precipitacao(.$mod, 0),
     p_i_lag_1 = plota_IOS_e_precipitacao(.$mod, 1)) %>% 
  unnest(Posto.Fluv) %>% 
  gather(tipo_graf, grafico, -Posto.Fluv) %>% 
  arrange(Posto.Fluv) %>% 
  mutate(id = 1:n()) 

limpa_pasta_graficos()

plyr::mdply(todos_os_graficos, salva_plot)
