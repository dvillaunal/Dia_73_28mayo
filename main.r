## ---- eval=FALSE, include=TRUE-----------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: Manejo de Fechas en R (Lubridate)
## 
##  4. Fuentes:
##     https://www.rdocumentation.org/packages/datasets/versions/3.6.2"
## 


## ----------------------------------------------------------------------------
#Instalar lubridate paquete con el comando:
install.packages("lubridate", dependencies = T)

# Llamar el paquete a la sesión de trabajo.
library(lubridate)


## ----------------------------------------------------------------------------
# Ver la config de la fecha como la zona horaria
Sys.getlocale("LC_TIME")


## ----------------------------------------------------------------------------
# Devuele la fecha de hoy:
hoy <- today()
print(hoy)


## ----------------------------------------------------------------------------
# Devuelve el año
a <- year(hoy)
print(a)

# Devuelve el mes
m <- month(hoy)
print(m)

# devuelve el dia
d <- day(hoy)
print(d)

# Creamos una matriz para visualizar mejor lo dicho:
mhoy <- rbind(a,m,d)
cat("Matriz despues de dividir la fecha por año, mes y dia\n")
print(mhoy)
write.table(mhoy, file = "FechaActual.txt", row.names = T)


## ----------------------------------------------------------------------------
# Para obtener feccha y hora utlizamos la funcion now():
ahora <- now()
cat("Fecha con Hora\n")
print(ahora)


## ----------------------------------------------------------------------------
# Extraemos la hora de "ahora"
hr <- hour(ahora)

# Extraemos los minutos de "ahora"
minu <- minute(ahora)

# Extraemos los segundos de "ahora"
seg <- second(ahora)

# Creamos una matriz para visualizar mejor las cosas:
mahora <- rbind(hr, minu, seg)
cat("Hora, minutos y segundos de \"Ahora\"\n")
print(mahora)
write.table(mahora, file = "HoraActual.txt", row.names = T)


## ----------------------------------------------------------------------------
# Volveremos la fecha actual a date:
hoy <- "2021-06-01"
print(hoy)
class(hoy)

# Aqui ya no es character, pasa a ser date:
hoy <- ymd(hoy)
print(hoy)
class(hoy)


## ----------------------------------------------------------------------------
#escogemos a "ahora" para sumarle 12 h, 15 min y 10 seg:
ahora <- update(ahora, hours = 12, minutes = 15, seconds = 10)


## ----------------------------------------------------------------------------
# importamos la base de datos:
nata <- read.csv(file = "natalidad.csv", header = T, sep = ",")


# Pasamos a fecha los datos de nacimiento y el dia de la cita del registro:
nata$nacimiento <- dmy(nata$nacimiento)

nata$registro <- dmy(nata$registro)


## ----------------------------------------------------------------------------
#Creamos una funcion que ingresen las fechas y depsues nos salga el tiempo e vida de cada bebe:

PeriodoDeVida <- function(n, r){
    "Recibe dos columnas de misma longitud, para calcular el delta de fecha r-n"
    #Retorna las dos fechas (en este caso con la misma zona horaria)
    i <- interval(n, r)
    # Hacemos el resto es decir el delta de las dos fechas:
    delta <- as.period(i)
    x <- 1:length(delta)
    p <- paste("El bebe Numero: ", x, "Lleva :", delta, "de nacido")
    dp <- data.frame("Dias de vida, para la registraduria:" = p)
    return(dp)
}

# Probamos la funcion:
PeriodoDeVida(nata$nacimiento, nata$registro)

# exportamos el resultado:
write.table(PeriodoDeVida(nata$nacimiento, nata$registro), file = "PeriodoDeVida.txt", row.names = F)


## ----------------------------------------------------------------------------
# Importamos la base de datos:
mefia <- read.csv(file = "mefia.csv", header = T, sep = ",")

# Convertimos los datos de fecha a date:
mefia$Fecha <- dmy(mefia$Fecha)


## ----------------------------------------------------------------------------
# Vamos a crear una funcion que nos retorne un texto:
"La Persona ~X~, sino se atrasa en sus pagos estara terminando la deuda en ~DATE~"

ultimopago <- function(x){
    "Recibe un data frame para generar el texto anterior"
    # Como no hay una funcion que sume semanas, sino dias, entonces convertiremos semanas a dias:
    "En flamingo existe la politica de que para no complicarse con temas del mes de 28, 31 o 30 días han decidido hacer las cuotas semanales (estandarizadas a 7 días)"
    dias <- x[[2]] * 7
    
    cuotafinal <- x[[3]] + days(dias)
    p <- paste("El Usuario", x[[1]],"sino se atrasa en sus pagos estara terminando la deuda en", cuotafinal)
    
    p <- data.frame("Cuota Final X Persona"= p)
    return(p)
}

# Observamos el resultado:
ultimopago(mefia)

# Exportamos el resultado:
write.table(ultimopago(mefia), file = "CuotaFinal.txt", row.names = F)