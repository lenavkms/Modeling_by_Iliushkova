# Илюшкова Елена  - для региона 16 (республика Татарстан) рассчитайте урожайность пшеницы в период 
# в 2011 году взяв для расчета средние суммы активных температур за этот год,
# с 25 ближайших метеостанций, но убирем убираем из рассчета активных температур температуру ниже 10 градусов.
# Зададим рабочую дирректорию
setwd("~/R")
#Очистим рабочию память 
rm(list=ls())
#Устанавливаем пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)
#Скачиваем список метеостанций
station_data=read.csv("station_data.csv")
#Создаем список метеостанций
kazan=data.frame(id="kazan",latitude=55.7331,longitude=49.200)
#Выбираем метеостанцию в Казани и 25 ближайших метеостанций в заданный временной период
kazan_around=meteo_nearby_stations(lat_lon_df = kazan, station_data = station_data,
                                      limit = 25,var = c("TAVG"),
                                      year_min = 2011, year_max = 2011)

# Вычисляем  индентификатор метеостанции Казань 
kazan_id=kazan_around[["kazan"]][["id"]][1]
summary(kazan_id)
kazan_table=kazan_around[[1]]
summary(kazan_table)
#Получим таблицы с ближайших метеостанций
kazan_table = data.frame(kazan_around)
summary(kazan_table)
# Создание цикла, с необходимыми данными с метеостанций 
all_i=data.frame()
# Объект куда скачиваются все данные со всех метеостанций
all_kazan_meteodata = data.frame()
#Работа с данными метеостанций
#Создаем цикл для скачивания данных с 25 метеостанций
for (v in 1:25)
{
  kazan_id = kazan_around[["kazan"]][["id"]][v]
  #
  data = meteo_tidy_ghcnd(stationid = kazan_id ,
                          var="TAVG",
                          date_min="2011-01-01",
                          date_max="2011-12-31")
  all_kazan_meteodata = bind_rows(all_kazan_meteodata, data)
}
#Запишем полученные данные в файл
write.csv(all_kazan_meteodata, "all_kazan_meteodata.csv")
all_kazan_meteodata
#Считаем данные из файла all_kazan_data.csv
all_kazan_meteodata=read.csv("all_kazan_meteodata.csv")
#Посмотрим на полученные данные 
str(all_kazan_meteodata)
#Добавим колонки год, месяц и день 
all_kazan_meteodata = mutate(all_kazan_meteodata, year = year(date), month = month(date), day = day(date))
str(all_kazan_meteodata)
#Выбирем активные температуры больше 10 градусов 
all_kazan_meteodata_tempr =filter(all_kazan_meteodata,tavg>10 ) 
#Проверим результат
str(all_kazan_meteodata_tempr)
#Посчитаем среднию сумму активных температур за месяц, путем деления на 10
all_kazan_meteodata_tempr[,"tavg"]=all_kazan_meteodata_tempr$tavg/10
str(all_kazan_meteodata_tempr) 
#Преобразим все нули в NA
all_kazan_meteodata_tempr[is.na(all_kazan_meteodata_tempr$tavg), "tavg"]=0
summary(all_kazan_meteodata_tempr)


#Сгрупируем по метеостанциям, годам и месецам при помощи фунции group_by
alldays_kazan=group_by(all_kazan_meteodata_tempr,id, year, month)
#Сумма темпереатур по данным группам 
sumT_months_kazan=summarize(alldays_kazan, tsum=sum(tavg))
summary(sumT_months_kazan)
#Cгрупируем данные по месяцам 
groups_kazan_month=group_by(sumT_months_kazan, month);groups_kazan_month
#Расчет среднего по месяцам для всех метеостанций и всех лет
sumT_months=summarize(groups_kazan_month,St=mean(tsum))
sumT_months
#Зададим St
ST=c(0,0,sumT_months$St)
ST
#Зададим данные для расчета урожая по исходной формуле
#Константы для расчета урожайности 
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#Коэффициент для экспозиции склона - примим, что у нас поля ровные
y = 1.0
#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "сумма частей основной и побочной продукции"
Lj = 2.2
#Коэффициент "стандартная влажность культуры"
Ej = 25
#Зададим данные для расчета урожая по исходной формуле
#Константы для расчета урожайности 
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#Коэффициент для экспозиции склона - примим, что у нас поля ровные
y = 1.0
#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "сумма частей основной и побочной продукции"
Lj = 2.2
#Коэффициент "стандартная влажность культуры"
Ej = 25
#Рассчет Fi по месяцам
Fi = afi + bfi * y * ST
Fi
#Рассчет Yi
#sumT_months = mutate(sumT_months, Yi = ((Fi * di) * Kf) / (Qj * Lj * (100 - Ej)))
#? mutate
Yi = ((Fi * di) * Kf) / (Qj * Lj * (100 - Ej))
Yi
#Расчет урожая, как сумму по месяцам
Yield = sum(Yi)
Yield
#Урожайность для Республики Татарстан в 2011 году составила 18.03874 ц/га


