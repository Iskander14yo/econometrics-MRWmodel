require(readr)
require(readxl)
require(tidyverse)

Sys.setenv(LANG = 'en')
setwd('D:\\Ранх учеба\\3 курс\\Метрика\\Проект')

ed = read_excel('educated.xlsx')
inf = read_excel('inflation.xlsx')
dem = read_excel('demography.xlsx')
demf = read_excel('demography (fraction).xlsx')
gdp = read_excel('gdp.xlsx')
inv = read_excel('investments.xlsx')

regions = dem[,1]
people = (dem[, -1] * demf[,-1] / 100) %>% as_tibble()

w_gdp = (gdp[,-1] / people * 1000) %>% as_tibble() # в тысячах
w_inv = (inv[,-1] / people * 1000) %>% as_tibble() # в тысячах
w_ed = (ed[,-1] / people * 1000) %>% as_tibble() # на одну тысячу человек

dem_rate = ((people['2003'] - people['2002']) / people['2002'])
people

# lm (check year of investments)
df = cbind(w_gdp['2003'], w_inv['2002'] * inf['2003']/100, dem_rate + 0.05)
colnames(df) = c('gdp', 'inv', 'dem_rate_')
df = as_tibble(df)
lm1 = lm(log(gdp) ~ log(inv) + log(dem_rate_) ,data = df)
summary(lm1)

# another lm
df1 = cbind(w_gdp['2003'], w_inv['2002'] * inf['2003']/100, dem_rate + 0.05,
            w_ed['2002'])
colnames(df1) = c('gdp', 'inv', 'dem_rate_', 'ed')
df1 = as_tibble(df1)
lm2 = lm(log(gdp) ~ log(inv) + log(dem_rate_) + log(ed),data = df1)
summary(lm2)


var(df1['inv'] + df1['dem_rate_'] + df1['ed'])/var(df1['gdp'])

