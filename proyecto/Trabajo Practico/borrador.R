# Escribir aquí el código de análisis de la distribución de becas por 
# tipo (maestría/doctorado) y disciplina



summary(becal_completo$mesesdeduraciondeestudios)

hist(becal_completo$mesesdeduraciondeestudios, main = "Meses de duración de estudio BECAL", 
     freq = FALSE, ylim=c(0,0.1), xlab = "meses")

hist(becal_completo$mesesdeduraciondeestudios, main = "Meses de duración de estudio BECAL", 
     freq = FALSE, ylim=c(0,0.1), xlab = "meses")

bp1<-barplot(prop.table(table(becal_completo$categoriauni)), 
        col = c("yellow", "blue", "orange", "red", "green"), ylim=c(0,0.9), 
        main="Ranking universidades becarixs BECAL", ylab ="")



pie(table(becal_completo$condicion),
    main="Completitud de Estudios becarixs",
    col=terrain.colors(5, alpha = .6),
    radius = 0.9)

rm(bp1)

summary(becal_completo$totalgeneral)
sd(becal_completo$totalgeneral)
hist(becal_completo$totalgeneral)
table(becal_completo$totalgeneral)

# Escribir aquí el código de análisis que calcular la correlación 
# entre montos de cobertura y ranking de universidades

plot(becal_completo$categoriauni, becal_completo$totalgeneral, pch=20,col="red")


chisq.test(table(becal_completo$edad,becal_completo$sectorlaboral))

chisq.test(table(becal_completo$categoriauni, becal_completo$tipoestudio))

class(becal_completo$sexo)
class(becal_completo$edad)
class(becal_completo$tiempodecobertura)
class(becal_completo$sectorlaboral)

summary(aov(edad ~ sectorlaboral, data=becal_completo))

summary(aov(mesesdeduraciondeestudios ~ lugarresidencia, data=becal_completo))

summary(aov(sexo~ mesesdeduraciondeestudios , data=becal_completo))

class(becal_completo$categoriauni)
class(becal_completo$tipoestudio)

lugarresidencia = function (departamentoderesidencia) { 
  
  if (departamentoderesidencia=="Capital" | departamentoderesidencia=="Asunción")
    
    return ("capital")
  
  else 
    
  if (departamentoderesidencia=="Central")
    
    return ("central")
  
  else 
    
  if (departamentoderesidencia=="Alto Paraná" | departamentoderesidencia=="Amambay" | departamentoderesidencia=="AMAMBAY" | departamentoderesidencia=="Boquerón" | departamentoderesidencia=="Caaguazu"| departamentoderesidencia=="Caaguazú" | departamentoderesidencia=="Caazapá" | departamentoderesidencia=="Canindeyú"| departamentoderesidencia=="Concepción" | departamentoderesidencia=="Cordillera" | departamentoderesidencia=="Guairá"| departamentoderesidencia=="Itapuá" | departamentoderesidencia=="Misiones"| departamentoderesidencia=="Ñeembucú" | departamentoderesidencia=="Paraguari" | departamentoderesidencia=="Paraguarí" | departamentoderesidencia=="San Pedro")
    
    return ("interior")
  
  else
  
    return ("no aplica")
}

becal_completo$lugarresidencia = sapply(becal_completo$departamentoderesidencia, lugarresidencia)

class(becal_completo$lugarresidencia)

table(becal_completo$paisdedestino)

destinomas = subset(becal_completo, becal_completo$paisdedestino=="España" | becal_completo$paisdedestino=="ESPAÑA" | becal_completo$paisdedestino=="Argentina" | becal_completo$paisdedestino=="Francia" | becal_completo$paisdedestino=="Australia" | becal_completo$paisdedestino=="Reino Unido")

class(destinomas$sexo)
class(destinomas$paisdedestino)

chisq.test(table(destinomas$paisdedestino, destinomas$sexo))

summary(aov(edad ~ sectorlaboral, data=becal_completo)) 

chisq.test(table(becal_completo$edad, becal_completo$sectorlaboral))

t.test(becal_completo$edad, becal_completo$sectorlaboral, alternative = "two.sided")

chisq.test(table(becal_completo$categoriauni, becal_completo$tipoestudio))

chisq.test(table(becal_completo$sexo, becal_completo$tiempodecobertura))



summary(lm(categoriauni ~ tipoestudio, data=becal_completo))

chisq.test(table(destinomas$sexo, destinomas$paisdedestino))

chisq.test(table(destinomas$sexo,droplevels(destinomas$paisdedestino)))




