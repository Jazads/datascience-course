v=c()
nu=c(0.5,0.6)
l1=c(TRUE, FALSE, TRUE)
l2=c(T,F)
ch=('a')
it=9:29
co=c(1+0i, 2+4i)
v=vector('numeric')
v[1]=5
print(v)
y=c(1.7, 'a')
y
y=c(TRUE,2)
y
y=c(FALSE,2)
it
y=c(TRUE,'a')
x=0:6
class(x)
x=as.numeric(x)
x
x=as.logical(x)
x
x=as.character(x)
x
x=c('a','b', 'c')
x=as.numeric(x)
x
x=as.logical(x)
x
m=matrix(1:6, nrow=2, ncol = 3)
m
dim (m)
m[2,1]
m[3,1]
x=1:3
y=10:12
z=cbind(x,y)
z
w=rbind(x,y)
w
x= list(1,'a',TRUE, 1+4i)
x
x[1]
x[[1]]
x=factor(c('yes','no','yes','no'))
x
table(x)
x=factor (c('yes','yes','no','yes','no'), levels=c('yes','no'))
x
x=c(1,2,NA,10,3)
is.na(x)
x=data.frame(c1=1:5, c2=c(T,T,F,F,T), c3=c('a','b','c','d','e'))
x
nrow(x)
ncol(x)
dim(x)
names(x)
x=10
y=0
if(x>3){
y=10
} else {
  y=50
}
y
x=11
if(x==10) {
  print('El valor')
}



x=data.frame(c1=4:6, c2=18:20)
for(i in seq_len(nrow(x))) {
  print(x[i,'c1'])
  print(x[i,2])
}
x=data.frame (c1=1:2, c2=10:11)
x
mult=function(x,c) {
  return (x*c)
}
apply(x,2, mult,5)
x=data.frame(c1=1:2, c2=10:11)
area_circulo= function(r,pi){
  return(pi*r^2)
}
sapply(x[,'c1'], area_circulo,3.14)
x=data.frame(c1=1:5, c2=10:14)
x
rowSums(x)
colSums(x)
data=read.csv(file='becal-cobertura.csv', header=T, stringsAsFactors=F)
write.csv(x,'leccion4.csv', row.names = F)
library(datasets)
autos=mtcars
hist(autos$mpg, col='green', main='Distribución de las millas por galón', 
     xlab='Millas por galón', ylab='Frecuencia')
boxplot(autos$hp, col='red', main='Distribución de caballos de fuerza', 
        ylab='Caballos de fuerza')
barplot(table(autos$am), col='green', xlab='Tipo de transmisión',
        main='Nro. de vehículos por tipo de transmisión')
plot(presidents, ylab = 'Porcentage de aprobación (%)', xlab='Año',
     main = 'Aprobación (1er cuatrimestre) Presidentes de EEUU')
presidents
plot(autos$mpg, autos$wt, col='blue', xlab='Millas por galón', ylab='Peso (libras)',
     main='Relación entre peso del vehículo y millas recorridas por galón')
p=presidents
x = data.frame('var1'=sample(1:3),'var2'=sample(6:8),'var3'=sample(11:13))
x
x[(x$var1 <= 3 & x$var3 > 11),]
x[(x$var1 <= 3 | x$var3 > 15),]
sort(x$var1,decreasing = TRUE)
sort(x$var1)
becal=read.csv(file='becal2017.csv', header=T, stringsAsFactors=F)
becal[1:5, 'Sexo']
tolower(becal[,'Sexo']) (1:5)
becal[,'Sexo']=tolower(becal[,'Sexo'])