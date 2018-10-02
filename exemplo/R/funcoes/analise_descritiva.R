#Padrões globais de gráficos

tamanho_da_letra <- 15
tamanho_do_pontinho <- 2.2

#Máscara pra colocar nos ggplots

padroes_esteticos <- theme_bw(tamanho_da_letra)

pontos <- geom_point(size = tamanho_do_pontinho)

legenda_defasagem <- function(variavel = 'IOS', l = 0, un = ''){
  if(l == 0){
    return(xlab(sprintf('%s %s', variavel, un)))
  } else {
    return(xlab(sprintf('%s defasado em %s meses %s',
                        variavel, l, un)))
  }
}

limpa_pasta_graficos <- function(path = 'figuras'){
  a <- suppressWarnings(
    file.remove(list.files(path, full.names = T)))
}

filtra_posto_fluv <- function(d, posto){
  d %>% 
    filter(Posto.Fluv == posto) %>% 
    inner_join(pluv_fluv) %>% 
    group_by(data, Evento, Estacao) %>% 
    summarise_each(funs(mean(., na.rm = T)),
                   IOS, Vazao, Precipitacao) %>% 
    mutate(Mes = factor(meses[month(data)], meses),
           int_eln = intensidade_eln(year(data)),
           int_lan = intensidade_lan(year(data))) %>% 
    filter(!is.na(Vazao),
           !is.na(Precipitacao),
           !is.na(IOS)) %>% 
    ungroup()
}

plota_IOS <- function(d) {
  d %>%
    filtra_posto_fluv(posto = 'Santa Maria') %>% 
    mutate(decenio = cut(year(data), 4, labels = F)) %>% 
    ggplot(aes(x = data, y = IOS, color = Evento))+
    geom_line(color = 'black', linetype = 1)+
    facet_wrap(~decenio, scales = 'free_x')+
    geom_hline(yintercept = 7) +
    geom_hline(yintercept = -7) +
    padroes_esteticos + 
    pontos +
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    xlab('Anos') +
    ylab('IOS')
}

plota_todas_as_vazoes <- function(d){
  
  d %>% 
    group_by(Posto.Fluv, data) %>% 
    summarise(Vazao = mean(Vazao, na.rm = T)) %>%
    #mutate(decenio = cut(year(data), 4, labels = F)) %>% 
    ggplot(aes(x = data, y = Vazao, color = Posto.Fluv))+
    geom_line()+
    #geom_point()+
    facet_wrap(~Posto.Fluv, scales = 'free', ncol = 1)+
    ylab('Vazão (m³/s)')+
    xlab('Data')+
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    padroes_esteticos
}

plota_vazao <- function(d){
  d %>% 
    mutate(decenio = cut(year(data), 4, labels = F)) %>% 
    ggplot(aes(x = data, y = Vazao))+
    geom_line(color = 'royalblue')+
    facet_wrap(~decenio, scales = 'free_x', ncol = 2)+
    ylab('Vazão (m³/s)')+
    xlab('Data') +
    padroes_esteticos +
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          strip.text.x = element_blank())
}

plota_vazao_int_eln <- function(d){
  d %>% 
    mutate(decenio = cut(year(data), 4, labels = F)) %>% 
    ggplot(aes(x = data, y = Vazao, color = int_eln))+
    geom_point()+
    facet_wrap(~decenio, scales = 'free_x', ncol = 2)+
    ylab('Vazão (m³/s)')+
    xlab('Data') +
    padroes_esteticos +
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          strip.text.x = element_blank())
}

plota_vazao_int_lan <- function(d){
  d %>% 
    mutate(decenio = cut(year(data), 4, labels = F)) %>% 
    ggplot(aes(x = data, y = Vazao, color = int_lan))+
    geom_point()+
    facet_wrap(~decenio, scales = 'free_x', ncol = 2)+
    ylab('Vazão (m³/s)')+
    xlab('Data') +
    padroes_esteticos +
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          strip.text.x = element_blank())
}

sazonalidade_vazao <- function(d){
  d %>% 
    group_by(Mes) %>% 
    ggplot(aes(x = Mes, y = Vazao)) +
    geom_boxplot() +
    ylab('Vazão (m³/s)') +
    padroes_esteticos
}

plota_acf_vazao <- function(d){
  d %>% 
    with(acf(ts(Vazao, frequency = 12), plot = F)) %>% 
    ggfortify:::autoplot.acf() +
    geom_hline(yintercept = 0) +
    ylab('Auto-correlação') +
    xlab('Defasagem') + 
    padroes_esteticos
}

plota_precipitacao <- function(d){
  d %>% 
    mutate(decenio = cut(year(data), 4, labels = F)) %>% 
    ggplot(aes(x = data, y = Precipitacao))+
    geom_line(color = 'royalblue')+
    facet_wrap(~decenio, scales = 'free_x', ncol = 2)+
    ylab('Precipitação (mm)')+
    xlab('Data') +
    padroes_esteticos +
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          strip.text.x = element_blank())
}

plota_precipitacao_int_lan <- function(d){
  d %>% 
    mutate(decenio = cut(year(data), 4, labels = F)) %>% 
    ggplot(aes(x = data, y = Precipitacao, color = int_lan))+
    geom_point()+
    facet_wrap(~decenio, scales = 'free_x', ncol = 2)+
    ylab('Precipitação (m³/s)')+
    xlab('Data') +
    padroes_esteticos +
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          strip.text.x = element_blank())
}

plota_precipitacao_int_eln <- function(d){
  d %>% 
    mutate(decenio = cut(year(data), 4, labels = F)) %>% 
    ggplot(aes(x = data, y = Precipitacao, color = int_eln))+
    geom_point()+
    facet_wrap(~decenio, scales = 'free_x', ncol = 2)+
    ylab('Precipitação (m³/s)')+
    xlab('Data') +
    padroes_esteticos +
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          strip.text.x = element_blank())
}

sazonalidade_precipitacao <- function(d){
  d %>% 
    group_by(Mes) %>% 
    # group_by(Mes, pluviometro) %>% 
    # summarise(
    #   Prep_max = quantile(Precipitacao, 0.9, na.rm = T),
    #   Prep_min = quantile(Precipitacao, 0.1, na.rm = T),
    #   Precipitacao = mean(Precipitacao, na.rm = T)) %>% 
    ggplot(aes(x = Mes, y = Precipitacao)) +
    geom_boxplot() +
    #geom_point() +
    #geom_ribbon(alpha = 0.15, color = 'black', linetype = 2)+
    #facet_wrap(~pluviometro, ncol = 2) +
    ylab('Precipitação (mm)')+
    xlab('Mes')+
    theme(axis.text.x = element_text(angle = 90)) +
    padroes_esteticos
}

plota_acf_precipitacao <- function(d){
  
  d %>% 
    with(acf(ts(Precipitacao, frequency = 12), plot = F)) %>% 
    ggfortify:::autoplot.acf() +
    geom_hline(yintercept = 0) +
    ylab('Auto-correlação') +
    xlab('Defasagem') +
    padroes_esteticos
}

plota_vazao_versus_precipitacao <- function(d,l = 0){
  
  dados_fit <- d %>% 
    mutate(P = lag(Precipitacao, l),
           P2 = lag(Precipitacao, l)^2) %>% 
    select(Vazao, P, P2)
  
  modelos <- list(
    'Linear' = lm(Vazao ~ P, data = dados_fit),
    `Quadrático` = lm(Vazao ~ P2 + P, data = dados_fit)
  )
  
  quadrado <- data_frame(tipo = c('Linear',
                                  'Quadrático')) %>%
    group_by(tipo) %>% 
    do(d = dados_fit) %>% 
    do(
      Precipitacao = .$d$P,
      Vazao = predict(modelos[[.$tipo]], newdata = .$d)
    ) %>% 
    ungroup() %>% 
    mutate(Modelo = c('Linear','Quadrático')) %>% 
    unnest(Precipitacao, Vazao)
  
  d %>% 
    mutate(Precipitacao = lag(Precipitacao, l)) %>% 
    ggplot(aes(x = Precipitacao, y = Vazao, color = IOS)) +
    geom_point() +
    geom_line(data = quadrado, aes(linetype = Modelo),
              color = 'black') +
    legenda_defasagem('Precipitação', l, '(mm)') +
    ylab('Vazão (m³/s)') +
    scale_color_gradient2(low = 'red',
                          high = 'green',
                          mid = 'gray') +
    padroes_esteticos
}

plota_ccf_vazao_precipitacao <- function(d){
  d %>% 
    with(ccf(Vazao, Precipitacao, plot = F)) %>% 
    ggfortify:::autoplot.acf() +
    ggtitle('') +
    ylab('Auto-correlação parcial') +
    xlab('Defasagem') +
    geom_hline(yintercept = 0) +
    padroes_esteticos
}

plota_IOS_e_vazao <- function(d, l = 0){
  d %>% 
    mutate(Evento = lag(Evento, l)) %>% 
    ggplot(aes(x = lag(IOS, l), y = Vazao, col = Evento)) +
    geom_point() + 
    theme_bw() +
    ylab('Vazão (m³/s)') +
    legenda_defasagem('IOS', l, '') +
    geom_vline(xintercept = 7, col = 'green') +
    geom_vline(xintercept = -7, col = 'red') +
    geom_smooth(method = 'lm', 
                alpha = 0, 
                linetype = 2, 
                col = 'black') +
    padroes_esteticos
}

plota_ccf_vazao_IOS <- function(d){
  d %>% 
    with(ccf(Vazao, IOS, plot = F)) %>% 
    ggfortify:::autoplot.acf() +
    ggtitle('') +
    geom_hline(yintercept = 0) +
    xlab('Defasagem') +
    ylab('Auto-correlação cruzada') +
    padroes_esteticos
}

plota_ccf_precipitacao_IOS <- function(d){
  d %>% 
  with(ccf(Precipitacao, IOS, plot = F)) %>% 
  ggfortify:::autoplot.acf() +
  ggtitle('') +
  geom_hline(yintercept = 0) +
  xlab('Defasagem') +
  ylab('Auto-correlação cruzada') +
  padroes_esteticos
}

plota_IOS_e_precipitacao <- function(d, l = 0){
  d %>% 
    mutate(Evento = lag(Evento, l)) %>% 
    ggplot(aes(x = lag(IOS, l), y = Precipitacao, col = Evento)) +
    geom_point() + 
    theme_bw() +
    ylab('Precipitação (mm)') +
    legenda_defasagem('IOS', l, '') +
    geom_vline(xintercept = 7, col = 'green') +
    geom_vline(xintercept = -7, col = 'red') +
    geom_smooth(method = 'lm', 
                alpha = 0, 
                linetype = 2, 
                col = 'black') +
    padroes_esteticos
}

salva_plot <- function(id, Posto.Fluv, tipo_graf, grafico){
  ggsave(sprintf('figuras/%s_%s_%s.png',
                 id, Posto.Fluv, tipo_graf),
         plot = first(grafico))
}