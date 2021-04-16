#Введение в R
#Задание 1
#Загрузим данные
data("iris");iris
#Посчитаем среднее для колонки Sepal.Lenght
mean(iris$Sepal.Length)
#Посчитаем среднее для колонки Sepal.Width
mean(iris$Sepal.Width)
#Посчитаем среднее для колонки Petel.Width
mean(iris$Petal.Width)
#Посчитаем среднее для колонки Petel.Lenght
v4 =mean(iris$Petal.Length)
#Объединим полученные данные в лист 
my.list=list(list1=mean(iris$Sepal.Length), list2=mean(iris$Sepal.Width), list3=mean(iris$Petal.Width), list4=mean(iris$Petal.Length))
#Задание 2
#Удалим колонку Specie
iris$Species<- NULL;iris
#Рассчитаем среднее по строке 
rowMeans(iris)
#Сохраним данные в векторе 
vector=c(rowMeans(iris));vector
#Задание 3
#Создадим 1000 нуклеотидов в векторе DNA путем перемешивания
DNA=factor(rep(c("T","G","С","A"), c(10,25,50,46)))
sample(DNA);DNA
#Создадим выборку больше исходника
sample(DNA, size=176, replace=TRUE)
sampleDNA=sample(c("A","T","G","C"), size=1000, replace=TRUE)
#Посчитаем долю A и Т 
retio=summary(factor(sampleDNA))/length(sampleDNA); retio
retio2=summary(factor(sampleDNA))/length(DNA);retio2
#Запишем в вектор dna_at
dna_at=c(retio); dna_at
#Задание 4
#Cоздадим вектор с латинскими буквами длинной 10000
zadan4=sample(c("L","A","O","C"), size=10000, replace=TRUE);zadan4
#Выведем колличество каждой буквы 
x=factor(zadan4)
summary(x)
summary(zadan4)
#Посчитаем A и назовем gla
gla=sum(zadan4 == "A");gla
#Посчитаем O и назовем glo
glo=sum(zadan4 == "O");glo
#Общая сумма гласных
sum(gla,glo)
#Задание 5
#Загрузим данные
data(iris); iris
#Посмотрим структуру
str(iris)
#Посчитаем среднию длинну лепестка
adat=mean(iris$Petal.Length);adat
#Загрузим пакет dplyr
library(dplyr)
library(ggplot2)
order(iris$Petal.Length)
iris[order(iris$Petal.Lengt),]
iris%>% arrange (Petal.Length)
#Задание 6
#Зададим вектор с именем adat
adat=c(5,4,7,8,10,12);adat
#Посчитаем медиану 
median(adat)
#Задание 7
# Загрузим данные
data(iris); iris
#Построим график зависимоти длины чашелистиков от длины липестков каждого вида 
plot(iris$Sepal.Length, iris$Petal.Length, xlab = "Длина липестков", ylab="Длина чашелистиков", main = "График зависисмости длины чашелистиков от длины липестков в каждом виде", col="green", type= "p")
#Задание 8
#Загрузим пакет ggplot2
library(ggplot2)
#Загрузим данные
data("diamonds"); diamonds
diamonds%>%filter(price>1000)%>%group_by(clarity)%>%
            mutate(carat_price=price/carat)%>%
            summarise(mean_price_carat=mean(carat_price))
  
