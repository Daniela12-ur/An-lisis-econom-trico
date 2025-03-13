library(dplyr)
library()
# Filtrar datos de mujeres en edad de trabajar (>15 años) en el área seleccionada
datos_mujeres <- Caracteristicas %>%
  filter(AREA == "5", P3271 == "2", P6040 > 15)

# Calcular experiencia y su cuadrado
datos_mujeres <- datos_mujeres %>%
  mutate(Exp = P6040 - P3042S1 - 6,
         Exp2 = Exp^2)

# Identificar hijos menores de 6 y entre 6 y 18 años
hijos_por_hogar <- Caracteristicas %>%
  mutate(hijo_0_6 = ifelse(P6040 <= 6, 1, 0),
         hijo_6_18 = ifelse(P6040 > 6 & P6040 <= 18, 1, 0)) %>%
  group_by(DIRECTORIO) %>%
  summarise(n_hijos_0_6 = sum(hijo_0_6, na.rm = TRUE),
            n_hijos_6_18 = sum(hijo_6_18, na.rm = TRUE))

# Calcular el ingreso total del hogar
ingreso_hogar <- Ocupados %>%
  group_by(DIRECTORIO) %>%
  summarise(ingreso_hogar = sum(INGLABO, na.rm = TRUE))

# Identificar mujeres que trabajan
ocupadas <- Ocupados %>%
  select(DIRECTORIO, ORDEN, INGLABO) %>%
  mutate(trabaja = ifelse(INGLABO > 0, 1, 0))

# Unir todas las bases de datos
datos_finales <- datos_mujeres %>%
  left_join(hijos_por_hogar, by = "DIRECTORIO") %>%
  left_join(ingreso_hogar, by = "DIRECTORIO") %>%
  left_join(ocupadas, by = c("DIRECTORIO", "ORDEN")) %>%
  mutate(trabaja = ifelse(is.na(trabaja), 0, trabaja))

# Seleccionar variables relevantes
datos_modelo <- datos_finales %>%
  select(trabaja, P3042S1, Exp, Exp2, n_hijos_0_6, n_hijos_6_18, ingreso_hogar, P6040, P2057) %>%
  mutate(NIVEL_EST = factor(P3042S1, levels = 1:13, 
                            labels = c("NINGUNO", "PREESCOLAR", "BASICA PRIMARIA", "BASICA SECUNDARIA", "BASICA MEDIA", 
                                       "MEDIA TECNICA", "NORMALISTA", "TECNICA PROFESIONAL", "TECNOLOGIA", "UNIVERSIDAD", 
                                       "ESPECIALIZACION", "MAESTRIA", "DOCTORADO")),
         Campesina = factor(P2057, levels = c(1, 2)))

# Modelo Probit
probit <- glm(trabaja ~ NIVEL_EST + Exp + Exp2 + n_hijos_0_6 + n_hijos_6_18 + ingreso_hogar + Campesina, 
              data = datos_modelo, 
              family = binomial(link = 'probit'))

summary(probit)
datos <- model.frame(probit)
########### Probabilidades marginales ######

##### variables discretas ######

#### hijo menor a 6 años en cada hogar ####

#### efecto marginal promedio #######

x0= model.matrix(probit)
x0[,13]= 0
x0

x1= model.matrix(probit)
x1[,13]= 1
x1

z0= x0 %*% coef(probit)

z1 = x1 %*% coef(probit)

eft=pnorm(z1)-pnorm(z0)
mean(eft)

##### Efecto marginal en el promedio #####

x= model.matrix(probit)
centros= apply(x, 2, mean)
centros

centros0=centros
centros0[13] = 0
centros0
centros1= centros 
centros1[13]=1
centros1

z1= centros1 %*% coef(probit); z1
z0= centros0 %*% coef(probit);z0

pnorm(z1)-pnorm(z0)

#### campesina #####

### efecto marginal Promedio ##### 
x0= model.matrix(probit)
x0[,16]= 0
x0

x1= model.matrix(probit)
x1[,16]= 1
x1

z0= x0 %*% coef(probit)

z1 = x1 %*% coef(probit)

eft=pnorm(z1)-pnorm(z0)
mean(eft)

#### efecto marginal en el promedio #####

x= model.matrix(probit)
centros= apply(x, 2, mean)
centros

centros0=centros
centros0[16] = 0
centros0
centros1= centros 
centros1[16]=1
centros1

z1= centros1 %*% coef(probit); z1
z0= centros0 %*% coef(probit);z0

pnorm(z1)-pnorm(z0)

###### Nivel educativo #####
###### efecto marginal promedio #####
x0= model.matrix(probit)
x0[,6]= 0
x0

x1= model.matrix(probit)
x1[,10]= 1
x1

z0= x0 %*% coef(probit)

z1 = x1 %*% coef(probit)

eft=pnorm(z1)-pnorm(z0)
mean(eft)

#### efecto marginal en el promedio #####

x= model.matrix(probit)
centros= apply(x, 2, mean)
centros

centros0=centros
centros0[6] = 0
centros0

centros1= centros 
centros1[10]=1
centros1

z1= centros1 %*% coef(probit); z1
z0= centros0 %*% coef(probit);z0

pnorm(z1)-pnorm(z0)
###### variable continua  ingreso hogar ######
#efecto marginal promedio#

coef(probit)

probit= glm(Regresion, data=datos, 
            family= binomial(link='probit'), x=TRUE)
x= model.matrix(probit)
x

z = x %*% coef(probit)
ef= dnorm(z)*coef(probit)[15]

mean(ef)
summary(ef)



###### prueba de hipótesis compuesta ####
## Prueba de hipótesis compuestas
# Ho: El nivel educativo no afecta en la probabilidad de trabajar de la mujer en el hogar 

# Modelo restringido 

regresion_1 = trabaja ~Exp + Exp2 + n_hijos_0_6 + n_hijos_6_18 + ingreso_hogar + Campesina

probit_rest<- glm(regresion_1,data=datos_modelo, family=binomial(link='probit'))
summary(probit_rest)

# Prueba de razón de verosimilitud
-2*(logLik(probit_rest) - logLik(probit))

# Valor crítico con 95% de confianza
qchisq(.95,1)



##### matriz de confusion #####
datos= model.frame(probit)

pred= predict(probit, type= 'response')
pred
pred_class = ifelse(pred > 0.5, 1, 0)
pred_class
tab=table(pred_class, datos$trabaja)
tab
sum(diag(tab))/nrow(datos) 
