require(readr)
require(readxl)
require(tidyverse)

Sys.setenv(LANG = 'en')
setwd('D:\\Ранх учеба\\3 курс\\Метрика\\Проект')

ed = read_excel('educated.xlsx') # тысяч человек
# inf = read_excel('inflation.xlsx') 
dem = read_excel('demography.xlsx') # тысяч человек
demf = read_excel('demography (fraction).xlsx') # labour (%)
# в процентах от всего населения
gdp = read_excel('GDP.xlsx') # ВРП - в миллионах
inv = read_excel('investments.xlsx') # в миллионах


regions = dem[,1]
# labour
people = (dem[, -1] * demf[,-1] / 100) %>% as_tibble() # тысячи

# приведение показателей на душу трудового населения
w_gdp = cbind(regions, gdp[,-1] * 1000/ people ) %>% 
  as_tibble() # в тысячах рублей
w_inv = cbind(regions, inv[,-1] * 1000/ people ) %>% 
  as_tibble() # в тысячах рублей
w_ed = cbind(regions, ed[,-1] / people ) %>% 
  as_tibble() # в единицах людей
# сколько человек закончило учебу в этот год относительно всего населения
# чтобы перевести в более удобное измерение, домножим на 1000
w_ed[,-1] = (w_ed[, -1] * 1000) %>% as_tibble() # в тысячах людей


dem_rate = ((people['2003'] - people['2002']) / people['2002']) %>% as_tibble()
people

# нужно помнить, что наши данные содержат еще РФ
w_gdp = w_gdp[-1,]; w_inv = w_inv[-1, ]
w_ed = w_ed[-1, ]; regions = regions[-1, ]; dem_rate = dem_rate[-1, ]
m = mean(w_gdp$'2003')
regions1 = regions[w_gdp$'2003' > m, ]
regions2 = regions[w_gdp$'2003' < m, ]




# --------------------
# lm (simple Solow)
df = cbind(w_gdp['2003'], w_inv['2003'], dem_rate + 0.05)
colnames(df) = c('gdp', 'inv', 'dem_rate_')
df = as_tibble(df)

# "обычная" регрессия
lm1 = lm(log(gdp) ~ log(inv) + log(dem_rate_) ,data = df %>% filter(gdp > m))
summary(lm1)
lm2 = lm(log(gdp) ~ log(inv) + log(dem_rate_) ,data = df %>% filter(gdp < m))
summary(lm2)
lm3 = lm(log(gdp) ~ log(inv) + log(dem_rate_) ,data = df)
summary(lm3)
# замечание про dem_rate (не значима)
# этой регрессией чекается alpha (restricted regression)
lm4 = lm(log(gdp) ~ log(inv) - log(dem_rate_) ,data = df)
summary(lm4)

# исключая тюменскую область
df1 = df[-which(w_inv$'2003' == max(w_inv$'2003')), ]
# не значима
lm3 = lm(log(gdp) ~ log(inv) + log(dem_rate_) ,data = df1%>% filter(gdp > m))
summary(lm3)
lm4 = lm(log(gdp) ~ log(inv) - log(dem_rate_) ,data = df1)
summary(lm4)
# стало получше





# ----------------------------------------------------------
# (augmented Solow)
df1 = cbind(w_gdp['2003'], w_inv['2003'], dem_rate + 0.05,
            w_ed['2003'])
colnames(df1) = c('gdp', 'inv', 'dem_rate_', 'ed')
df1 = df1 %>% as_tibble()

df2 = df1 %>% filter(gdp > m)
df3 = df1 %>% filter(gdp < m)
cor(df1$inv, df1$ed)
cor(df1$dem_rate_, df1$ed)
round(cor(df3),
      digits = 2 # rounded to 2 decimals
)
# нет корреляции (для каждой из групп)
# под вопросом мультиколлинеарность (inv + может быть еще что-то)
# тем не менее все коэф-ы значимы

##### lm`s ##########
lm2 = lm(log(gdp) ~ log(inv) + log(dem_rate_) + log(ed),
         data = df1)
summary(lm2)
# для проверки коэф-ов (restricted regression)
lm3 = lm(log(gdp) ~ (log(inv) - log(dem_rate_)) + (log(ed)- log(dem_rate_)),
         data = df3)
summary(lm3)

# без тюмени
hist(w_inv$'2003', breaks = 20)
regions[which(w_inv$'2003' == max(w_inv$'2003')), ]
df4 = df1[-which(w_inv$'2003' == max(w_inv$'2003')), ]
lm2 = lm(log(gdp) ~ log(inv) + log(dem_rate_) + log(ed),
         data = df4)
summary(lm2)
lm4 = lm(log(gdp) ~ (log(inv) - log(dem_rate_)) + (log(ed)- log(dem_rate_)),
         data = df4)
summary(lm4)






################################ Подсчет через зп ##########
income = read_excel('income.xlsx')
# введение дохода
w_ed = data.frame('2003' = income['2003'] - min(income['2003'])) %>% as_tibble()
