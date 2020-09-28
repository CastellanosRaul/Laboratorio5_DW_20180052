Lab 5
================

# Raul Castellanos 20180052

``` r
library(dplyr)
library(readr)
library(lubridate)
library(nycflights13)
library(huxtable)
```

### Parte 1: Predecir un eclipse solar

``` r
fecha_eclipse <- ISOdatetime(2017, 08, 21, 18, 26, 40, "US/Central")
SyMonth <- days(29) + hours(12) + minutes(44) + seconds(3)
Saros <- SyMonth*223
Sig_Ecplipse <- fecha_eclipse + Saros
paste("El siguiente eclipse ocurrirá en la siguiente fecha y hora:", Sig_Ecplipse)
```

    ## [1] "El siguiente eclipse ocurrirá en la siguiente fecha y hora: 2035-09-02 02:09:49"

### Parte 2: Agrupaciones y operaciones con fechas

``` r
operaciones <- read_csv("data_lab5.csv")
names(operaciones) <- c("Fecha_Creacion","Hora_Creacion","Caller_ID",
                        "Cod","Email","SMS","Call","Fecha_Final","Hora_Final")
operaciones <- operaciones[operaciones$Cod != 0,]
operaciones$Fecha_Creacion <- dmy(operaciones$Fecha_Creacion)
operaciones$Fecha_Final <- dmy(operaciones$Fecha_Final)
```

En qué meses existe una mayor cantidad de llamadas por código

``` r
operaciones$month <- month(operaciones$Fecha_Creacion)
operaciones$mes <- ifelse(operaciones$month == 1, "Enero", 
                          ifelse(operaciones$month == 2, "Febrero",
                                 ifelse(operaciones$month == 3, "Marzo",
                                        ifelse(operaciones$month == 4, "Abril",
                                               ifelse(operaciones$month == 5, "Mayo",
                                                      ifelse(operaciones$month == 6, "Junio",
                                                             ifelse(operaciones$month == 7, "Julio",
                                                                    ifelse(operaciones$month == 8, "Agosto",
                                                                           ifelse(operaciones$month == 9, "Septiembre",
                                                                                  ifelse(operaciones$month == 10, "Octubre",
                                                                                         ifelse(operaciones$month == 11, "Noviembre","Diciembre")))))))))))
llamadas_mes <- operaciones %>% select(Cod, Call, mes) %>% group_by(mes,Cod) %>% 
  summarise(Cantidad = sum(Call), .groups = 'drop') %>% arrange(desc(Cantidad))
#5 meses con mas llamadas por el código: Actualización de información
hux(llamadas_mes[1:5,])
```

``` 
             mes         Cod                       Cantidad  
             Marzo       Actualización de               522  
                         Información                         
             Mayo        Actualización de               521  
                         Información                         
             Diciembre   Actualización de               514  
                         Información                         
             Agosto      Actualización de               491  
                         Información                         
             Octubre     Actualización de               487  
                         Información                         
```

Column names: mes, Cod, Cantidad

Dia de la semana más ocupado

``` r
operaciones$dia <- weekdays(operaciones$Fecha_Creacion)
dia_ocupado <- operaciones %>% group_by(dia) %>% 
  summarise(Cantidad = n(), .groups = 'drop') %>% arrange(desc(Cantidad))
#El día más ocupado es el domingo
hux(dia_ocupado[1,])
```

``` 
                           dia       Cantidad  
                           Sunday       35827  
```

Column names: dia, Cantidad

Mes más ocupado

``` r
mes_ocupado <- operaciones %>% group_by(mes) %>% 
  summarise(Cantidad = n(), .groups = 'drop') %>% arrange(desc(Cantidad))
#El mes más ocupado es Octubre
hux(dia_ocupado[1,])
```

``` 
                           dia       Cantidad  
                           Sunday       35827  
```

Column names: dia, Cantidad

Existencia de cocentración o estacionalidad en la cantidad de llamadas

``` r
llamadas_al_mes <- operaciones %>% group_by(mes) %>%
  summarise(Llamadas = sum(Call), .groups = 'drop') %>% arrange(desc(Llamadas))
llamadas_al_dia <- operaciones %>% group_by(dia) %>%
  summarise(Llamadas = sum(Call), .groups = 'drop') %>% arrange(desc(Llamadas))
hux(llamadas_al_mes)
```

``` 
                         mes          Llamadas  
                         Marzo             522  
                         Mayo              521  
                         Diciembre         514  
                         Agosto            491  
                         Octubre           487  
                         Noviembre         486  
                         Julio             481  
                         Enero             476  
                         Abril             447  
                         Junio             447  
                         Septiembre        437  
                         Febrero           416  
```

Column names: mes, Llamadas

``` r
hux(llamadas_al_dia)
```

``` 
                          dia         Llamadas  
                          Wednesday        856  
                          Tuesday          853  
                          Monday           832  
                          Friday           824  
                          Thursday         795  
                          Saturday         791  
                          Sunday           774  
```

Column names: dia,
Llamadas

``` r
#Como podemos observar en las dos tablas, las llamadas por mes y día tienen cantidades cercanas
#con poca dispersión, podríamos decir que no existe alguna concentración o estacionalidad.
```

Duración de llamada promedio

``` r
operaciones$signo <- operaciones$Hora_Final - operaciones$Hora_Creacion
operaciones$duracion <- ifelse(operaciones$signo >= 0, 
                               (operaciones$Hora_Final - operaciones$Hora_Creacion)/60,
                               (((operaciones$Hora_Creacion - operaciones$Hora_Final)/60)-1440)*-1) #Ajuste 24hrs (1440 mins)
llamada_promedio <- operaciones %>% filter(Call == 1) %>% summarise(Minutos_Promedio = round(mean(duracion)))
hux(llamada_promedio)
```

``` 
                            Minutos_Promedio  
                                          15  
```

Column names: Minutos\_Promedio

Tabla de frecuencias con el tiempo de llamada

``` r
llamadas <- operaciones[operaciones$Call==1,]
llamadas_tabla <- llamadas %>% group_by(duracion) %>% summarise(Freq = sum(Call), .groups = 'drop')
hux(llamadas_tabla)
```

``` 
                           duracion      Freq  
                                  0       221  
                                  1       211  
                                  2       173  
                                  3       195  
                                  4       193  
                                  5       184  
                                  6       194  
                                  7       197  
                                  8       212  
                                  9       166  
                                 10       190  
                                 11       197  
                                 12       169  
                                 13       163  
                                 14       203  
                                 15       188  
                                 16       181  
                                 17       178  
                                 18       186  
                                 19       190  
                                 20       179  
                                 21       205  
                                 22       175  
                                 23       192  
                                 24       186  
                                 25       174  
                                 26       157  
                                 27       173  
                                 28       158  
                                 29       171  
                                 30       164  
```

Column names: duracion, Freq

### Parte 3: Signo Zodiacal

``` r
signo <- function(x){
  fecha <- dmy(x)
  dia <- day(fecha)
  mes <- month(fecha)
  
  signo <- ifelse(dia >= 21 & mes == 3 | dia <= 19 & mes == 4, "Aries", 
                  ifelse(dia >= 20 & mes == 4 | dia <= 20 & mes == 5, "Tauro",
                         ifelse(dia >= 21 & mes == 5 | dia <= 20 & mes == 6, "Gemini",
                                ifelse(dia >= 21 & mes == 6 | dia <= 22 & mes == 7, "Cancer",
                                       ifelse(dia >= 23 & mes == 7 | dia <= 22 & mes == 8, "Leo",
                                              ifelse(dia >= 23 & mes == 8 | dia <= 22 & mes == 9, "Virgo",
                                                     ifelse(dia >= 23 & mes == 9 | dia <= 22 & mes == 10, "Libra",
                                                            ifelse(dia >= 23 & mes == 10 | dia <= 21 & mes == 11, "Scorpio",
                                                                   ifelse(dia >= 22 & mes == 11 | dia <= 21 & mes == 12, "Sagittarius",
                                                                          ifelse(dia >= 22 & mes == 12 | dia <= 19 & mes == 1, "Capricorn",
                                                                                 ifelse(dia >= 20 & mes == 1 | dia <= 18 & mes == 2, "Aquarius",
                                                                                        ifelse(dia >= 19 & mes == 2 | dia <= 20 & mes == 3, "Pisces",NA))))))))))))
  return(paste("Tu fecha de nacimiento es:", fecha, ", tu signo zodiacal es:", signo))
}
signo("22-09-1999")
```

    ## [1] "Tu fecha de nacimiento es: 1999-09-22 , tu signo zodiacal es: Virgo"

``` r
signo("21-12-1999")
```

    ## [1] "Tu fecha de nacimiento es: 1999-12-21 , tu signo zodiacal es: Sagittarius"

``` r
signo("08-05-1999")
```

    ## [1] "Tu fecha de nacimiento es: 1999-05-08 , tu signo zodiacal es: Tauro"

``` r
signo("11-01-2000")
```

    ## [1] "Tu fecha de nacimiento es: 2000-01-11 , tu signo zodiacal es: Capricorn"

### Parte 4: Flights

Generar 4 columnas nuevas con formato fecha y hora

``` r
flights <- nycflights13::flights

flights$fecha <- substr(flights$time_hour,1,10)

flights$dep_time_hora <- ifelse(flights$dep_time >=0 &flights$dep_time <10, paste0("00:0",flights$dep_time,":00"),
                           ifelse(flights$dep_time >=10 &flights$dep_time <100, paste0("00:",flights$dep_time,":00"),
                                  ifelse(flights$dep_time >=100 &flights$dep_time <1000, paste0("0",substr(flights$dep_time,1,1),":",substr(flights$dep_time,2,3),":00"),
                                         ifelse(flights$dep_time >=1000 &flights$dep_time <2500, paste0(substr(flights$dep_time,1,2),":",substr(flights$dep_time,3,4),":00"),NA))))

flights$sched_dep_time_hora <- ifelse(flights$sched_dep_time >=0 &flights$sched_dep_time <10, paste0("00:0",flights$sched_dep_time,":00"),
                           ifelse(flights$sched_dep_time >=10 &flights$sched_dep_time <100, paste0("00:",flights$sched_dep_time,":00"),
                                  ifelse(flights$sched_dep_time >=100 &flights$sched_dep_time <1000, paste0("0",substr(flights$sched_dep_time,1,1),":",substr(flights$sched_dep_time,2,3),":00"),
                                         ifelse(flights$sched_dep_time >=1000 &flights$sched_dep_time <2500, paste0(substr(flights$sched_dep_time,1,2),":",substr(flights$sched_dep_time,3,4),":00"),NA))))

flights$arr_time_hora <- ifelse(flights$arr_time >=0 &flights$arr_time <10, paste0("00:0",flights$arr_time,":00"),
                           ifelse(flights$arr_time >=10 &flights$arr_time <100, paste0("00:",flights$arr_time,":00"),
                                  ifelse(flights$arr_time >=100 &flights$arr_time <1000, paste0("0",substr(flights$arr_time,1,1),":",substr(flights$arr_time,2,3),":00"),
                                         ifelse(flights$arr_time >=1000 &flights$arr_time <2500, paste0(substr(flights$arr_time,1,2),":",substr(flights$arr_time,3,4),":00"),NA))))


flights$sched_arr_time_hora <- ifelse(flights$sched_arr_time >=0 &flights$sched_arr_time <10, paste0("00:0",flights$sched_arr_time,":00"),
                           ifelse(flights$sched_arr_time >=10 &flights$sched_arr_time <100, paste0("00:",flights$sched_arr_time,":00"),
                                  ifelse(flights$sched_arr_time >=100 &flights$sched_arr_time <1000, paste0("0",substr(flights$sched_arr_time,1,1),":",substr(flights$sched_arr_time,2,3),":00"),
                                         ifelse(flights$sched_arr_time >=1000 &flights$sched_arr_time <2500, paste0(substr(flights$sched_arr_time,1,2),":",substr(flights$sched_arr_time,3,4),":00"),NA))))

flights$dep_time_hora_1 <- paste(flights$fecha, flights$dep_time_hora)
flights$sched_dep_time_hora_1 <- paste(flights$fecha, flights$sched_dep_time_hora)
flights$arr_time_hora_1 <- paste(flights$fecha, flights$arr_time_hora)
flights$sched_arr_time_hora_1 <- paste(flights$fecha, flights$sched_arr_time_hora)

flights$dep_time_hora <- hms(flights$dep_time_hora)
flights$sched_dep_time_hora <- hms(flights$sched_dep_time_hora)
flights$arr_time_hora <- hms(flights$arr_time_hora)
flights$sched_arr_time_hora <- hms(flights$sched_arr_time_hora)

#Nuevas columnas 
hux(head(flights[,25:28]))
```

dep\_time\_hora\_1 sched\_dep\_time\_h arr\_time\_hora\_1
sched\_arr\_time\_h  
ora\_1 ora\_1  
2013-01-01 2013-01-01 2013-01-01 2013-01-01  
05:17:00 05:15:00 08:30:00 08:19:00  
2013-01-01 2013-01-01 2013-01-01 2013-01-01  
05:33:00 05:29:00 08:50:00 08:30:00  
2013-01-01 2013-01-01 2013-01-01 2013-01-01  
05:42:00 05:40:00 09:23:00 08:50:00  
2013-01-01 2013-01-01 2013-01-01 2013-01-01  
05:44:00 05:45:00 10:04:00 10:22:00  
2013-01-01 2013-01-01 2013-01-01 2013-01-01  
05:54:00 06:00:00 08:12:00 08:37:00  
2013-01-01 2013-01-01 2013-01-01 2013-01-01  
05:54:00 05:58:00 07:40:00 07:28:00

Column names: dep\_time\_hora\_1, sched\_dep\_time\_hora\_1,
arr\_time\_hora\_1, sched\_arr\_time\_hora\_1

Delay
Total

``` r
flights$Delay <- (flights$dep_time_hora - flights$sched_dep_time_hora) + (flights$arr_time_hora - flights$sched_arr_time_hora)
flights$Delay <- period_to_seconds(flights$Delay)/60

#Delay  
hux(head(flights[,29]))
```

``` 
                                      Delay  
                                         13  
                                         24  
                                         35  
                                        -19  
                                        -31  
                                          8  
```

Column names: Delay
