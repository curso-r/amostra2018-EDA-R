library(gamlss)
library(ggplot2)
library(forecast)
library(ggfortify)
library(lubridate)
library(tidyr)
#library(writexlsx)
library(openxlsx)
library(nlme)
library(xtable)
library(dplyr)

anos <- 1950:2010
meses <- c('Jan','Fev','Mar','Abr','Mai','Jun',
           'Jul','Ago','Set','Out','Nov','Dez')


anos <- 1950:2010
meses <- c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez')

#criterios intensidade el nino

eln_moderado <- c(1977:1978, 1993:1994)

eln_moderado_pra_forte <- c(1987:1988, 1991:1992)

eln_forte <- c(1982:1983, 1994:1995, 1997:1998)

eln_fraco <- c(2002:2003, 2006:2007)

eln_fraco_pra_moderado <- c(2009:2010)

#criterios intensidade la nina

lan_forte <- c(2010:2012)

lan_fraco_pra_moderado <- c(2008:2009, 2007:2008)

lan_moderado <- c(1998:2001, 1988:1989)

lan_moderado_pra_forte <- c(1973:1976)