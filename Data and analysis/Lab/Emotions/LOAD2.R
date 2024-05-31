#######LOAD EJECUTAR MANUAL

data_webA <- load_webA();
data_webB <- load_webB();
data_video <- load_video();
pairs <- GET_PAIR();
data_pension <- load_pension();

result_df <- data.frame()


for (i in seq_along(path_pair)) {
  print(i);
  print(path_pair[i]);
  result <- SUM(path_pair[i])
  result_df <- rbind(result_df, result)
}

# Llamar a la función transformar_a_wide el dataset
dataset_transformado <- transformar_a_wide(result_df)

# dataset con resta WEBA - WIX
dataset_con_diferencias <- calcular_diferencias(dataset_transformado)

# dataset con resta WIX - WEBA
dataset_con_diferencias2 <- calcular_diferencias2(dataset_transformado)

# Agregar nuevas columnas (Tratamiento, Perfil, Sitio, Video, t_perfil y t_video)
dataset_con_columnas <- agregar_columnas(dataset_con_diferencias2)


#' @details Regresiones 
#' Regresión Lineal en sus diferentes combinaciones
#' 
#' #' Regresión Lineal Dependiente = Diferencia entre valence WIX y WebA. 
#' Independiente = Treatment
#' 

# Realizar la regresión lineal Valence
modelo_valence2 <- lm(Diff2_Valence ~ treatment, data = dataset_con_columnas)

# Obtener los resultados del modelo Valence
summary(modelo_valence2)

# Realizar la regresión lineal Arousal 
modelo_arousal2 <- lm(Diff2_Arousal ~ treatment, data = dataset_con_columnas)

# Obtener los resultados del modelo Arousal
summary(modelo_arousal2)

#' 
#' #' #' #' Regresión Lineal Dependiente = Diferencia entre valence WIX y WebA. 
#'Independiente = sitio (Perfil, VideoPerfil, Video, Baseline)
#'

# Realizar la regresión lineal Valence
modelo_valence2 <- lm(Diff2_Valence ~ sitio, data = dataset_con_columnas)

# Obtener los resultados del modelo Valence
summary(modelo_valence2)

# Realizar la regresión lineal Arousal
modelo_arousal2 <- lm(Diff2_Arousal ~ sitio, data = dataset_con_columnas)

# Obtener los resultados del modelo Arousal
summary(modelo_arousal2)


#' 
#' #' #' #' Regresión Lineal Dependiente = Diferencia entre valence WIX y WebA. 
#'Independiente = video (0,1)


# Realizar la regresión lineal Valence 
modelo_valence2 <- lm(Diff2_Valence ~ video, data = dataset_con_columnas)

# Obtener los resultados del modelo Valence
summary(modelo_valence2)

# Realizar la regresión lineal Arousal
modelo_arousal2 <- lm(Diff2_Arousal ~ video, data = dataset_con_columnas)

# Obtener los resultados del modelo Arousal
summary(modelo_arousal2)

#' 
#' #' #' #' Regresión Lineal Dependiente = Diferencia entre valence WIX y WebA. 
#'Independiente = t_perfil (Perfil, Producto)


# Realizar la regresión lineal Valence
modelo_valence2 <- lm(Diff2_Valence ~ t_perfil, data = dataset_con_columnas)

# Obtener los resultados del modelo Valence
summary(modelo_valence2)

# Realizar la regresión lineal Arousal
modelo_arousal2 <- lm(Diff2_Arousal ~ t_perfil, data = dataset_con_columnas)

# Obtener los resultados del modelo Arousal
summary(modelo_arousal2)

#' 
#' #' #' #' Regresión Lineal Dependiente = Diferencia entre valence WIX y WebA. 
#'Independiente = t_video (texto, video)


# Realizar la regresión lineal Valence 
modelo_valence2 <- lm(Diff2_Valence ~ t_video, data = dataset_con_columnas)

# Obtener los resultados del modelo Valence
summary(modelo_valence2)

# Realizar la regresión lineal Arousal
modelo_arousal2 <- lm(Diff2_Arousal ~ t_video, data = dataset_con_columnas)

# Obtener los resultados del modelo Arousal
summary(modelo_arousal2)


#' 
#' #' Regresión Lineal Valance2 (solo sitios WIX) Dependiente y Treatment Independiente


# Realizar la regresión lineal Valence 
modelo_valence2 <- lm(Valence2 ~ treatment, data = dataset_con_columnas)

# Obtener los resultados del modelo Valence
summary(modelo_valence2)


#' #' Regresión Lineal Valance2 (solo sitios WIX) Dependiente y sitios Independiente

# Realizar la regresión lineal Valence 
modelo_valence2 <- lm(Valence2 ~ sitio, data = dataset_con_columnas)

# Obtener los resultados del modelo Valence
summary(modelo_valence2)


#' #' Regresión Lineal Valance2 (solo sitios WIX) Dependiente y video Independiente

# Realizar la regresión lineal Valence 
modelo_valence2 <- lm(Valence2 ~ video, data = dataset_con_columnas)

# Obtener los resultados del modelo Valence
summary(modelo_valence2)


#' #' Regresión Lineal Valance2 (solo sitios WIX) Dependiente y t_perfil Independiente

# Realizar la regresión lineal Valence 
modelo_valence2 <- lm(Valence2 ~ t_perfil, data = dataset_con_columnas)

# Obtener los resultados del modelo Valence
summary(modelo_valence2)


#' #' Regresión Lineal Valance2 (solo sitios WIX) Dependiente y t_video Independiente

# Realizar la regresión lineal Valence 
modelo_valence2 <- lm(Valence2 ~ t_video, data = dataset_con_columnas)

# Obtener los resultados del modelo Valence
summary(modelo_valence2)


# Exportar el dataset a un archivo CSV
write.csv(dataset_con_columnas, file = "dataset_con_columnas.csv", row.names = FALSE)