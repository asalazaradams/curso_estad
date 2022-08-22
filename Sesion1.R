#Introduccion a R

#En la ventana de script escribimos las instrucciones que queremos ejecutar
#lo que se escribe despues del simbolo "#" es ignorado por R
#Podemos decirle a R que nos haga operacipones matemáticas y el resultado aparecerá
#en la consola
#suma
2+2
#resta
3-2
#la multiplicación es con "*"
5*3
3+4*6-4/5

#raíz cuadrada
sqrt(6)

#Elevar un numero a una potencia con "**" o "^"
3**2
3^2
2^3


#operadores logicos:
2<3
pi==4

#ejercicio 1: calcule la media o promedio de: 3,9,5.3,6,-2 y 4

#R es un lenguaje orientado a objetos

#A traves del operador "=" asignamos el valor de 2 al objeto "x"
x=2
#También se puede con el operador "<-"
x<-2

#

y=5
x+y
x*y

#OJO: no confundir operador logico "==" con operador de asignacion "="
#Vectores: Permite capturar varios datos en un objeto


#R distingue entre mayúsculas y minúsculas. "X" y "x" son objetos distintos

x=2
X=5

x
X

X+x


#Tambien podemos asignar varios valores en objeto denominado "vector"
#vamos a crear un vector llamado "w" que contenga 3 valores numericos

w<-c(2,4,6)

# "c" es una función de R que indica que hay que "combinar" 
# valores para formar un vector
#Si escribimos el nombre del vector, nos aparecen los valores:

w

#Recupera el valor de un vector señalando su posicion:
a[2]




#se pueden hacer operaciones con los elementos de los vectores
a/3
a/y

#calcula la media o promedio del vector a a traves de la funcion mean
#

mean(a)

#Ejercicio2: calcula la media o promedio de los siguientes valores:
# 87.64379, 108.53079,  98.23150, 102.94917,  71.63592,  96.52769, 126.88023, 102.70724,
# 96.62241, 125.84505, 102.38597,  93.96054,  81.60932,  96.94412, 128.40992, 106.3934,
# 91.63853, 107.91700, 112.16106, 124.03230,  86.88841,  76.84956, 114.98078,  83.61206,
# 103.06134, 108.34766, 102.09580 , 79.93712,  78.88568



# colocar ? antes de una funcion permite obtener informacion sobre esta
?mean

#argumentos de la funcion: x, na.rm , trimmed 
z=c(103.06134, 108.34766, 102.09580 ,86.88841, 79.93712,  78.88568, NA)
mean(z,na.rm = TRUE)
g=c(103.06134, 108.34766, 102.09580 , 79.93712,  78.88568, 125.84505,
    102.38597,  93.96054,  81.60932,2500)
mean(g)
mean(g, trim=.1)

#ejercicio 3. calcule la sumatoria y mediana de los datos del ejercicio 2 y del vector z y el vector g
#median()
#sum()


a <- c(3,9,5.3,6,-2,4) 
# vector numerico

b <- c("uno","dos","tres") 
# vector caracter

c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) 
#vector logico

class(a)
class(b)
class(c)

#Tablas de datos

#matrices, solo para valores numéricos
h <- c(1,2,3,4)
i <- c(5,6,7,8)
j<- c(9,10,11,12)
k=cbind(h,i,j)
class(k)
k[2,]


#Data Frames: permiten organizar datos de diferente tipo
id <- c(1,2,3,4)
color <- c("rojo", "blanco", "rojo", NA)
nuevo <- c(TRUE,TRUE,TRUE,FALSE)
mis.datos <- data.frame(id,color,nuevo)
mis.datos