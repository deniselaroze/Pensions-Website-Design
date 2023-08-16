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



# Exportar el dataset a un archivo CSV
write.csv(dataset_con_columnas, file =paste0(path_datos, "dataset_con_columnas.csv"), row.names = FALSE)