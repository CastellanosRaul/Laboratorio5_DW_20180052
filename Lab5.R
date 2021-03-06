library(dplyr)
library(readr)
library(lubridate)
library(nycflights13)
library(huxtable)

# Parte 1 -----------------------------------------------------------------
#Zona horaria de Norte America utilizada -> CST Time
fecha_eclipse <- ISOdatetime(2017, 08, 21, 18, 26, 40, "US/Central")
SyMonth <- days(29) + hours(12) + minutes(44) + seconds(3)
Saros <- SyMonth*223
Sig_Ecplipse <- fecha_eclipse + Saros
paste("El siguiente eclipse ocurrirá en la siguiente fecha y hora:", Sig_Ecplipse)

# Parte 2 -----------------------------------------------------------------
operaciones <- read_csv("data_lab5.csv")
names(operaciones) <- c("Fecha_Creacion","Hora_Creacion","Caller_ID",
                        "Cod","Email","SMS","Call","Fecha_Final","Hora_Final")
operaciones <- operaciones[operaciones$Cod != 0,]
operaciones$Fecha_Creacion <- dmy(operaciones$Fecha_Creacion)
operaciones$Fecha_Final <- dmy(operaciones$Fecha_Final)

# En qué meses existe una mayor cantidad de llamadas por código -----------
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

# Qué día de la semana es el más ocupado ----------------------------------
operaciones$dia <- weekdays(operaciones$Fecha_Creacion)
dia_ocupado <- operaciones %>% group_by(dia) %>% 
  summarise(Cantidad = n(), .groups = 'drop') %>% arrange(desc(Cantidad))
#El día más ocupado es el domingo
hux(dia_ocupado[1,])


# Qué mes es el más ocupado -----------------------------------------------
mes_ocupado <- operaciones %>% group_by(mes) %>% 
  summarise(Cantidad = n(), .groups = 'drop') %>% arrange(desc(Cantidad))
#El mes más ocupado es Octubre
hux(dia_ocupado[1,])



# Existe una concentración o estacionalidad  ------------------------------
# en la cantidad de llamadas ----------------------------------------------

llamadas_al_mes <- operaciones %>% group_by(mes) %>%
  summarise(Llamadas = sum(Call), .groups = 'drop') %>% arrange(desc(Llamadas))
llamadas_al_dia <- operaciones %>% group_by(dia) %>%
  summarise(Llamadas = sum(Call), .groups = 'drop') %>% arrange(desc(Llamadas))
hux(llamadas_al_mes)
hux(llamadas_al_dia)

#Como podemos observar en las dos tablas, las llamadas por mes y día tienen cantidades cercanas
#con poca dispersión, podríamos decir que no existe alguna concentración o estacionalidad.


# Cuántos minutos dura la llamada promedio --------------------------------
operaciones$signo <- operaciones$Hora_Final - operaciones$Hora_Creacion
operaciones$duracion <- ifelse(operaciones$signo >= 0, 
                               (operaciones$Hora_Final - operaciones$Hora_Creacion)/60,
                               (((operaciones$Hora_Creacion - operaciones$Hora_Final)/60)-1440)*-1) #Ajuste 24hrs (1440 mins)
llamada_promedio <- operaciones %>% filter(Call == 1) %>% summarise(Minutos_Promedio = round(mean(duracion)))
hux(llamada_promedio)


# Tabla frecuencias con el tiempo de llamada ------------------------------
llamadas <- operaciones[operaciones$Call==1,]
llamadas_tabla <- llamadas %>% group_by(duracion) %>% summarise(Freq = sum(Call), .groups = 'drop')
hux(llamadas_tabla)


# Parte 3 -----------------------------------------------------------------
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
signo("21-12-1999")
signo("08-05-1999")
signo("11-01-2000")

# Parte 4 -----------------------------------------------------------------
flights <- nycflights13::flights

# 1.	Genere 4 nuevas columnas para c/variable con fecha y hora ------------

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

# Delay Total -------------------------------------------------------------
flights$Delay <- (flights$dep_time_hora - flights$sched_dep_time_hora) + (flights$arr_time_hora - flights$sched_arr_time_hora)
flights$Delay <- period_to_seconds(flights$Delay)/60

#Delay  
hux(head(flights[,29]))
