#Autor: Lavinia Lavin
#Bioestadistica y visualizacion de datos. 8 de marzo 2025
#Paso 1
#Primero vamos a cargar las liberias necesarias
library(modeest)
library(plotrix)
library(stats) 
library(readxl)
#Paso 2
#Cargamos los datos crudos: Vamos a tener el oxigeno consumido, especies (2)
Copia_de_Oxygen_consumption <- read_excel("Copia de Oxygen consumption.xls")
#La naturaleza de nuestros datos: ¿Como se comportan?
#Paso 3
# Determinar que variables tenemos, adjuntar el set de datos que vamos a trabajar 
names<-Copia_de_Oxygen_consumption 
names 
attach(Copia_de_Oxygen_consumption)
#Paso 4
#Creamos un objeto con los datos de consumo de oxigeno (n total)
consumption<-Copia_de_Oxygen_consumption$consumption
#Paso 5
#Obtener nuestros primeros valores (Promedio, Varianza, Error estandar)
mean(consumption)
var(consumption)
sd(consumption)
#Paso 6
#Ver como se comportan mis datos visualmente (Histograma)
hist(consumption, col= "red")
#Paso 7
#Comprobar la normalidad de mis datos (Shapiro test, qqplot)
shapiro.test(consumption)
qqnorm(consumption, col="blue")
#Paso 8
#Valores y caracteristicas de mis datos: Valores mas frecuentes, totalidad de mis datos 
mlv(consumption, method="mfv")
length(consumption)
#Paso 9
#Crear un objeto con mis datos (<-) para comenzar con las pruebas estadisticas parametricas 
d<-Copia_de_Oxygen_consumption
#Primera preuba estadistica: T de Student 
t.test(d)
#Si lo requerimos podemos hacer sub-conjuntos de nuestros datos con la funcion "subset"
sp1<-subset(d, species==1)
sp2<-subset(d, species==2) 
print(sp1)
print(sp2)
#Paso 10
#Mi primera visualizacion de datos: Boxplot
boxplot(consumption~species, col="green", main="Oxygen consumption per species")
#¿Como cambiamos los nombres de los ejes X y Y? xlab y ylab

#Existe relacion entre nuestras variables? Para esto podemos aplicar una preuba de ChiSQ (X2)
chisq.test(consumption)
#Existen diferencias entre que tanto varian nuestros datos? Podemos aplicar una preuba de F de Fisher, primero se tiene que hacer el calculo del valor estadistico
#Suponemos que nuestros datos tienen un nivel de confianza del 95%, esto se traduce a un  alfa de 0.05
alfa <- 0.05
n <- 143
d<-47
#Calculamos el valor critico de F con la siguiente formula:
valor_critico_F <- qf(1 - alfa, df1 = n, df2 = d) 
#Para ver nuestros resultados:
print(valor_critico_F)
#Existen diferencias entre nuestras especies y su consumo de oxigeno? Podemos aplicar una ANOVA
#Primero creamos el objeto y con la funcion "aov" determinamos que variables se quieren 
anova<-aov(species~consumption) 
#Para ver nuestros resultados utilizamos la funcion "summary"
summary(anova)

