5+3
5-3
5/3
5*3
5^3
5*(10-3)
sqrt(25)
x<-5-3
print(x)
x<-4
print(x)
print(X)
ls()
w <- "ejemplo"
print(w)
w/2
class(w)
x<- c(1,2,3,5)
print(x)
x[4]
x[-4]
division <- x/2
print(division)
proporcion <- x/x[3]
print(proporcion)
incremento<-x[-1] -x[-4]
porcentajeincre<- (incremento/x[-4]) * 100
print(porcentajeincre)
porcentajeincre [c(1,2)] <- c(7,8)
porcentajeincre
length(x)
min(x)
max(x)
range(x)
mean(x)
sum(x)
sum(x) / length(x)
year<-seq(from=1950, to=1980, by=5)
print(year)
year2<- seq(from=1980, to=1950, by=-5)
print(year2)
1980:1950
names(x)
names(x)<-"numeros"
names(x)
my.summary <- function(x){ 
  s.out <- sum(x)
  l.out <- length(x)
  m.out <- s.out / l.out
  out <- c(s.out, l.out, m.out) 
  names(out) <- c("sum", "length", "mean") 
  return(out) 
}
my.summary(x)
getwd()
becal2017<-read.csv("becal2017.csv")
class(becal2017)
View(becal2017)
names(becal2017)
ncol(becal2017)
nrow(becal2017)
dim(becal2017)
summary(becal2017)
becal2017$Sexo
becal2017[,"Tipo.de.Beca..segÃºn.Programa.aprobado."]
becal2017[1,]
mean(x, na.rm = TRUE)
install.packages("extrafont")
library(extrafont)
font_import()
fonttable()
plot(matriculacnc$X__1,matriculacnc$X__2,type="l", 
     main="Matricula CNC: 1940-2014",font.main=9,
     ylab="Matriculación",
     xlab="Años", font.lab=9, col="blue",font=9, xlim= c(1940,2014))
help(par)
