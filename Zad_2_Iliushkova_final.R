# Илюшкова Елена  - Задание 2. Группа Д-Х125
#Задание: Создайте модель множественной линейной регрессии ночных потоков углекислого 
#газа за весенний период 2013 года по данным измерений методом турбулентной пульсации
# Зададим рабочую дирректорию
setwd("~/R")
#Очистим рабочию память 
rm(list=ls())
#Устанавливаем пакеты
library(rnoaa)
library(tidyverse)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(readr)

#1-ый этап
#Подготовка данных
#Загрузим данные, удавлив строчки 1 и 3, и заменив значения -9999 символом NA
eddypro = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
#Удалим пустую первую строку
eddypro = eddypro [-1,];eddypro
#Просмотрим на переменные, с помощью функции glimpse()
glimpse(eddypro)
#Избавимся от переменной  roll, содержащие NA
eddypro = select(eddypro, -(roll))
eddypro
#Преобразуем переменные типа char, в факторы
eddypro = eddypro %>% mutate_if(is.character, factor)
#С помощью функции str_replace_all заменим ненужные нам символы
names(eddypro) =  str_replace_all(names(eddypro), "[!]","_emph_")
#Избавимся от ненужных символов
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(eddypro)
#Избавимся от строк, где есть значения  NA, с помощью функции drop_na()
eddypro = drop_na(eddypro)
#Отфильтруем данные за весенний период, а именно, с  1 марта (60 день) по 31 мая (151 день)
eddypro = filter(eddypro, DOY >= 60 & DOY <= 151)
#Отфильтруем данные для нoчного периода
eddypro = filter(eddypro, daytime==FALSE)
#Так как функция cor, работает только с численными данными, поэтому 
##для проведения корреляционного анализа отберем переменные типа numeric
#Воспользуемся функциями is.numeric(), и sapply()
sapply(eddypro,is.numeric)
#Полученный вектор вставим в таблицу и и создадим таблицу, состоящую только из колонок
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
#Таблица, содержащая все остальные колонки 
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]
#Переходим к кореляционному анализу
cor_td = cor(eddypro_numeric );cor_td
#Полученную матрицу преобразуем в таблицу и выберем из нее интересующие нас столбец, в котором возьмем знаения 
##коэффициента детерминации больше 0.1
cor_td = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
#Собирем все переменные из вектора с именами переменных в одну формулу для регрессионного анализа
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")); formula
#Создадим произвольные обучающие выборки с использованием команды sample_n из пакета dplyr
teaching_eddypro = sample_n(eddypro, floor(length(eddypro$date)*.7))
testing_eddypro = sample_n(eddypro, floor(length(eddypro$date)*.3))
#Сформируем  непересикающиеся подвыборки 
row_numbers = 1:length(eddypro$date)
teach = sample(row_numbers, floor(length(eddypro$date)*.7))
test = row_numbers[-teach]
teaching_eddypro_unq = eddypro_numeric[teach,]
testing_eddypro_unq = eddypro_numeric[test,]

#2-ой этап
#Построение модели
#Модель 1, по обучающей выборки с учетом всех переменных, отраженных в переменной formula
model1 = lm(formula , data = teaching_eddypro_unq)
#Узнаем информацию о модели 
summary(model1)
#Узнаем  коэффициенты
coef(model1)
#Узнаем остатки 
resid(model1)
#Узнаем доверительный интервал
confint(model1)
#Дисперсионный анализ
anova(model1)
#Построем график модели 
plot(model1,2)
# согласно ДА мы видим какие переменные у нас не значимые: 
# un_h2o_flux, ts_var , h2o_var, w_div_h2o_cov, flowrate 
#Модель 2, создадим модель м добавим в нее переменные, полученные с помощью 
##дисперсионного анализа anova() с коэффициентом значимости меньше 0.01
formula2= co2_flux~rand_err_H+ rand_err_LE +rand_err_co2_flux+
          rand_err_h2o_flux +co2_mole_fraction+air_density +
          co2_mixing_ratio+sonic_temperature+air_temperature+air_molar_volume+
          es+ T_star_+ un_H+un_LE+un_co2_flux +un_h2o_flux+w_spikes +ts_var +h2o_var+
          w_div_ts_cov+ w_div_co2_cov+ w_div_h2o_cov+co2_1 
model2= lm(formula2, data = teaching_eddypro_unq)
#Посмотрим информацию о модели
summary(model2)
#Посмотрим коэффициенты
coef(model2)
#Выведем остатки
resid(model2)
#доверительный интервал
confint(model2)
#дисперсионный анализ
anova(model2) 
# Сравним модели 2 и 1
anova(model2, model1)
#Посмотрим графики модели
plot(model2,2)
# Модель 3 - добавим в неё переменные, полученные при помощии функции anova() с коэффииентом
#значимости меньше 0.001 
formula3= co2_flux~rand_err_H+rand_err_LE+rand_err_co2_flux+rand_err_h2o_flux+
          co2_mole_fraction+air_density+co2_mixing_ratio +sonic_temperature+air_molar_volume+
          es+ T_star_+ un_H+ un_LE+ un_co2_flux+ un_h2o_flux + w_spikes +ts_var +h2o_var+w_div_ts_cov +
          w_div_co2_cov+w_div_h2o_cov+co2_1 
model3= lm(formula3, data = teaching_eddypro_unq)
#Посмотрим информацию о модели
summary(model3)
#Посмотрим коэффициенты
coef(model3)
#Выведем остатки
resid(model3)
#доверительный интервал
confint(model3)
#дисперсионный анализ
anova(model3) 
# Сравним модели 2 и 3
anova(model3, model2)
#Посмотрим графики модели
plot(model3,2)
# Модель 4 - с переменными, полученные при помощии функции anova() с пометками "***" 
formula4=co2_flux~rand_err_H+ rand_err_LE +rand_err_co2_flux+ rand_err_h2o_flux+
         co2_mixing_ratio+ sonic_temperature + air_molar_volume+ es+ T_star_ + un_H +
         un_LE+ un_co2_flux 
model4 = lm(formula4, data = teaching_eddypro_unq)
#Посмотрим информацию о модели
summary(model4)
#Посмотрим коэффициенты
coef(model4)
#Выведем остатки
resid(model4)
#доверительный интервал
confint(model4)
#дисперсионный анализ
anova(model4)
# Сравним модели 4 и 3
anova(model4, model3)
#Посмотрим графики
plot(model4,2)

#3-тий этап.Корреляционный анализ
#Используем только переменные, участвующие в корреляционном анализе
cor_teaching_eddypro = select(teaching_eddypro_unq, rand_err_H, rand_err_LE, rand_err_co2_flux, 
                              rand_err_h2o_flux, co2_mixing_ratio, sonic_temperature, air_molar_volume,es ,T_star_, 
                              un_H, un_LE, un_co2_flux )
#Получаем таблицу коэффициентов корреляции
cor_eddypro = cor(cor_teaching_eddypro) %>% as.data.frame
#Построение графиков по полученной модели
#Построим график co2_flux от co2_flux, использовав значения, полученные на модели 4, и на основе обучающей выборки
qplot(co2_flux, co2_flux, data = teaching_eddypro_unq) + geom_line(aes(y = predict(model4, teaching_eddypro_unq)))
#График расположен под углом 45 градусов и проходит почти через все точки 
#Построим график h2o_flux от h2o_flux, использовав значения, полученные на модели 4, и на основе тестирующей выборки
qplot(co2_flux, co2_flux, data = testing_eddypro_unq) + geom_line(aes(y = predict(model4, testing_eddypro_unq)))
#Для примера выведем несколько графиков зависимостей переменной co2_flux от: sonic_temperature, co2_mixing_ratio, air_molar_volume, 
#un_co2_flux на основе тестирующей модели
qplot(sonic_temperature,co2_flux, data = testing_eddypro_unq) + geom_line(aes(y = predict(model4, testing_eddypro_unq)))
qplot(co2_mixing_ratio,co2_flux, data = testing_eddypro_unq) + geom_line(aes(y = predict(model4, testing_eddypro_unq)))
qplot(air_molar_volume,co2_flux, data = testing_eddypro_unq) + geom_line(aes(y = predict(model4, testing_eddypro_unq)))
qplot(un_co2_flux,co2_flux, data = testing_eddypro_unq) + geom_line(aes(y = predict(model4, testing_eddypro_unq)))
