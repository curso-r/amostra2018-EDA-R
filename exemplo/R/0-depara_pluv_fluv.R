pluv_fluv <- data_frame(pluviometro = 1:41) %>% 
  mutate(`Santa Maria` = 
           ifelse(!(pluviometro %in% c(3:6,34:36)),
                  1, 0),
         `Porto Guarani` = 
           ifelse(pluviometro %in% c(7,38,40,41),
                  1, 0),
         `Guampará` = 
           ifelse(pluviometro %in% c(7,38),
                  1, 0),
         pluviometro = as.character(pluviometro)) %>% 
  gather(Posto.Fluv, valor, -pluviometro) %>% 
  filter(valor == 1) %>% 
  dplyr::select(-valor)

pluv_fluv <- readRDS('exemplo/R/funcoes/pluv_fluv.rds')
