ler_planilhas <- function(p, arq, comeco, l, c){
  d <- read.xlsx(arq, p, startRow = comeco,
                 colNames = T)[l, c] %>%
    gather(key = 'Mes', value = 'Valor', -Ano) %>%
    mutate(pluviometro = p,
           Valor = gsub('\\*','',Valor),
           Valor = as.numeric(gsub('\\,','.',Valor)),
           Mes = factor(Mes, meses),
           mes = as.numeric(Mes),
           estacao = ceiling(mes/3),
           Estacao = ifelse(estacao == 1, 'Verão',
                            ifelse(estacao == 2, 'Outono',
                                   ifelse(estacao == 3, 'Inverno',
                                          'Primavera'))))
  return(d)
}

ler_pra_detectar_coisas_estranhas <- function(p,
                                              arq,
                                              comeco,
                                              l, c){
  d <- read.xlsx(arq, p, startRow = comeco,
                 colNames = T)[l, c] %>%
    gather(key = 'Mes', value = 'Valor', -Ano) %>%
    mutate(pluviometro = p,
           estranho = grepl('[*,]',Valor),
           #Valor = gsub('\\*','',Valor),
           #Valor = as.numeric(gsub('\\,','.',Valor)),
           Mes = factor(Mes, meses),
           mes = as.numeric(Mes),
           estacao =  ceiling(mes/3),
           Estacao = ifelse(estacao == 1, 'Verão',
                            ifelse(estacao == 2, 'Outono',
                                   ifelse(estacao == 3, 'Inverno',
                                          'Primavera'))))
  return(d)
}

intensidade_eln <- function(anos){
    ifelse(anos %in% c(eln_forte), 'Forte', 
           ifelse(anos %in% c(eln_moderado_pra_forte, eln_moderado), 'Moderado',
                  ifelse(anos %in% c(eln_fraco, eln_fraco_pra_moderado), 'Fraco', 'Neutro'))) %>% 
    factor(c('Neutro','Fraco','Moderado','Forte'))
}

intensidade_lan <- function(anos){
  ifelse(anos %in% c(lan_forte), 'Forte', 
         ifelse(anos %in% c(lan_moderado_pra_forte, lan_moderado), 'Moderado',
                ifelse(anos %in% c(lan_fraco_pra_moderado), 'Fraco', 'Neutro')))%>% 
    factor(c('Neutro','Fraco','Moderado','Forte'))
}