# Carga los datos en la sesión de trabajo

  datos <- read.table(file.choose(),header=TRUE, sep=",") #Leer los datos del archivo externo

#OBTENER MAS INFORMACIÓN DE LOS DATOS
  summary(datos)
  
  par(mfrow=c(1,2))
  hist(datos$Fresh)		# histograma de frescos
  boxplot(datos$Fresh)  # boxplot de frescos
  
  par(mfrow=c(1,2))
  hist(datos$Milk)		# histograma de lácteos
  boxplot(datos$Milk)	# boxplot de lácteos
 
  par(mfrow=c(1,2))
  hist(datos$Grocery)		# histograma de comestibles
  boxplot(datos$Grocery)	# boxplot de comestibles
  
  par(mfrow=c(1,2))
  hist(datos$Frozen)	# histograma de congelados
  boxplot(datos$Frozen)	# boxplot de congelados
  
  par(mfrow=c(1,2))
  hist(datos$Detergents_Paper)		# histograma de detergentes/papel
  boxplot(datos$Detergents_Paper)	# boxplot de detergentes/papel
 
  par(mfrow=c(1,2))
  hist(datos$Delicassen)	# histograma de rotisería
  boxplot(datos$Delicassen)	# boxplot de rotisería
  

# 1) kmeans
d1 = kmeans(datos, 1)
d2 = kmeans(datos, 2)
d3 = kmeans(datos, 3)
d4 = kmeans(datos, 4)
d5 = kmeans(datos, 5)
d6 = kmeans(datos, 6)
d7 = kmeans(datos, 7)
d8 = kmeans(datos, 8)
d9 = kmeans(datos, 9)
d10 = kmeans(datos, 10)

# El valor pedido se almacena en la variable withinss

d1$withinss
d2$withinss
d3$withinss
d4$withinss
d5$withinss
d6$withinss
d7$withinss
d8$withinss
d9$withinss
d10$withinss

# 2)

# Se debe definir un criterio: 
# pararemos cuando la mejora sea menor a un % del total inicial, conocida como regla del codo
# o "elbow method"

porcentaje = 0.10

# tot.withinss entrega la suma de variaciones entre grupos

tots = c(d1$tot.withinss,
          d2$tot.withinss,
          d3$tot.withinss,
          d4$tot.withinss,
          d5$tot.withinss,
          d6$tot.withinss,
          d7$tot.withinss,
          d8$tot.withinss,
          d9$tot.withinss,
          d10$tot.withinss)
diferencias = tots[1:9] - tots[2:10]
porcentajes = diferencias/tots[1]

min_errores = porcentajes[porcentajes < porcentaje][1]
k = match(min_errores, porcentajes)

# Vemos que distintos criterios darían distintos k. 
plot(porcentajes)
lines(rep(0.1, 10), type = 'l', col = 'red') # 10 %
lines(rep(0.05, 10), type = 'l', col = 'blue') # 5 %
lines(rep(0.01, 10), type = 'l', col = 'green') # 1 %

# 3)

fresh = datos$Fresh
groc = datos$Grocery
plot(fresh, groc, col = c("red", "blue", "green")[d3$cluster])

# Para obtener subgrupo, usamos k = 3 por regla de 10 %
indices = d3$cluster
subgrupo = subset(datos, indices == 1)
summary(subgrupo)
