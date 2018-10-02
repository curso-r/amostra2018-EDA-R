source('R/0-load.R')
source('R/funcoes/limpeza_de_dados.R')

arq_pluviometros <- 'dados_brutos/planilha_precipitacao_piquiri.xlsx'
arq_outros <- 'dados_brutos/dados_marcio_doutorado.xlsx'

pluviometros <- arq_pluviometros %>%
  loadWorkbook %>%
  sheets

pluviometros_2 <- plyr::ldply(pluviometros,
                  ler_pra_detectar_coisas_estranhas,
                  arq = arq_pluviometros,
                  comeco = 15, l = 1:35, c = 1:13)

pluviometros <- plyr::ldply(pluviometros,
                ler_planilhas,
                arq = arq_pluviometros,
                comeco = 15, l = 1:35, c = 1:13)

Posto.Fluv <- c('Guampará',
            'Porto Guarani',
            'Santa Maria')

vazao <-  read.xlsx(arq_outros, 2, startRow = 1) %>%
  mutate(Posto.Fluv = rep(Posto.Fluv, each = 37)) %>%
  setNames(c((.)[1,-ncol(.)],'Posto.Fluv')) %>%
  gather(key = 'Mes', value = 'Valor', -Ano, -Posto.Fluv) %>%
  mutate(Valor = gsub('\\*','',Valor),
         Valor = gsub('\\,','.',Valor)) %>%
  filter(Ano %in% anos,
         Mes %in% meses) %>%
  mutate(Valor = as.numeric(Valor),
         Mes = factor(Mes, meses),
         mes = as.numeric(Mes),
         estacao =  ceiling(mes/3),
         Estacao = ifelse(estacao == 1, 'Verão',
                    ifelse(estacao == 2, 'Outono',
                     ifelse(estacao == 3, 'Inverno',
                            'Primavera'))))

ios <- read.xlsx(arq_outros, 3) %>%
  gather(key = 'Mes', value = 'Valor', -Ano) %>%
  mutate(Valor = gsub('\\*','',Valor),
         Valor = as.numeric(gsub('\\,','.',Valor)),
         Mes = factor(Mes, meses),
         Evento = ifelse(Valor > 7, 'La Niña',
                   ifelse(Valor < -7, 'El Niño',
                          'Neutro')),
         int_lan = intensidade_lan(Ano),
         int_eln = intensidade_eln(Ano)) %>%
  filter(!is.na(Valor))

completo <- inner_join(ios,vazao,
             by = c('Ano','Mes')) %>%
  rename(IOS = Valor.x, Vazao = Valor.y) %>%
  inner_join(pluviometros,
   by = c('Ano','Mes','mes')) %>%
  rename(Precipitacao = Valor,
         Estacao = Estacao.x,
         estacao = estacao.x) %>%
  select(-Estacao.y, -estacao.y) %>% 
  mutate(data = dmy(paste('01',mes,Ano)))

saveRDS(vazao, 'dados/vazao.rds')
saveRDS(ios, 'dados/ios.rds')
saveRDS(pluviometros, 'dados/pluviometros.rds')
saveRDS(completo,'dados/completo.rds')

# write.xlsx(vazao, 'dados/vazao.xlsx')
# write.xlsx(ios, 'dados/ios.xlsx')
# write.xlsx(pluviometros, 'dados/pluviometros.xlsx')
# write.xlsx(completo,'dados/completo.xlsx')
