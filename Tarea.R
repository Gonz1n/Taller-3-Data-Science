# Cargar librerías necesarias
library(tidyverse)
set.seed(123)

#Generar datos aleatorios
num_ventas <- 1000
id_venta <- 1:num_ventas
id_producto <- sample(1:100, num_ventas, replace = TRUE)
nombre_producto <- paste("Producto", id_producto)
categorias <- c("Electrónica", "Accesorios", "Hogar", "Juguetes")
categoria <- sample(categorias, num_ventas, replace = TRUE)
precio_unitario <- round(runif(num_ventas, 5, 500), 2)
cantidad_vendida <- sample(1:20, num_ventas, replace = TRUE)

#Crear data frame
ventas <- data.frame(
  id_venta,
  id_producto,
  nombre_producto,
  categoria,
  precio_unitario,
  cantidad_vendida
)

#Guardar en archivo CSV
write.csv(ventas, "ventas.csv", row.names = FALSE)
#Cargar el archivo CSV
ventas <- read.csv("ventas.csv")


str(ventas)

sum(is.na(ventas))

#Convertir tipos de datos 
ventas$categoria <- as.factor(ventas$categoria)

#total de ventas por categoría
total_ventas_categoria <- ventas %>%
  group_by(categoria) %>%
  summarise(total_ventas = sum(precio_unitario * cantidad_vendida))

print(total_ventas_categoria)

#precio promedio por categoría
precio_promedio_categoria <- ventas %>%
  group_by(categoria) %>%
  summarise(precio_promedio = mean(precio_unitario))

print(precio_promedio_categoria)

#productos más vendidos
productos_mas_vendidos <- ventas %>%
  group_by(nombre_producto) %>%
  summarise(total_cantidad = sum(cantidad_vendida)) %>%
  arrange(desc(total_cantidad)) %>%
  head(5)

print(productos_mas_vendidos)

#productos menos vendidos
productos_menos_vendidos <- ventas %>%
  group_by(nombre_producto) %>%
  summarise(total_cantidad = sum(cantidad_vendida)) %>%
  arrange(total_cantidad) %>%
  head(5)

print(productos_menos_vendidos)

#visualización de la distribución de precios por categoría
ggplot(ventas, aes(x = categoria, y = precio_unitario)) +
  geom_boxplot() +
  labs(title = "Distribución de Precios por Categoría",
       x = "Categoría",
       y = "Precio Unitario") +
  theme_minimal()

#Guardar los resultados en un archivo CSV
write.csv(total_ventas_categoria, "total_ventas_categoria.csv", row.names = FALSE)
write.csv(precio_promedio_categoria, "precio_promedio_categoria.csv", row.names = FALSE)
write.csv(productos_mas_vendidos, "productos_mas_vendidos.csv", row.names = FALSE)
write.csv(productos_menos_vendidos, "productos_menos_vendidos.csv", row.names = FALSE)

