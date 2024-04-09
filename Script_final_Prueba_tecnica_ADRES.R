# direccion de trabajo 
# pc 1 C:/Users/bcriv/OneDrive/Escritorio/Prueba_tecnica
# pc 2 C:/Users/bcriv/Desktop/Prueba_tecnica/Prueba_tecnica_ADRES
setwd('C:/Users/bcriv/OneDrive/Escritorio/Prueba_tecnica')
# carga de datos 
library(openxlsx)# lectura de bases de datos
library(readxl)
library(sqldf)# SQLDF para trabajar desde la base de datos
library(ggplot2)# graficacion
library(dplyr)# manipulacion
library(sf)
library(viridis)
library(scales)

### lectura de datos 
municipios <- read.xlsx("Municipios.xlsx")
prestadores <- read.xlsx("Prestadores.xlsx")

## limpieza de datos pre cargado 
# caracteres extraños 

municipios$Departamento <- gsub("[^[:alnum:]\\s]", "", municipios$Departamento)
municipios$Municipio <- gsub("[^[:alnum:]\\s]", "", municipios$Municipio)

# minimizar y quitar tildes

prestadores$depa_nombre <- tolower(prestadores$depa_nombre)
prestadores$muni_nombre <- tolower(prestadores$muni_nombre)
prestadores$depa_nombre <- iconv(prestadores$depa_nombre, to = "ASCII//TRANSLIT")
prestadores$muni_nombre <- iconv(prestadores$muni_nombre, to = "ASCII//TRANSLIT")


municipios$Departamento <- tolower(municipios$Departamento)
municipios$Municipio <- tolower(municipios$Municipio)
municipios$Departamento <- iconv(municipios$Departamento, to = "ASCII//TRANSLIT")
municipios$Municipio <- iconv(municipios$Municipio, to = "ASCII//TRANSLIT")

# espacios en blanco 

prestadores$depa_nombre  <- gsub("\\s+", "", prestadores$depa_nombre)
prestadores$muni_nombre  <- gsub("\\s+", "", prestadores$muni_nombre)

# eliminar "y providencia"

prestadores$depa_nombre <- gsub("yprovidencia", "", prestadores$depa_nombre)


# cargue de los datos a SQLite
conect <- dbConnect(SQLite(), dbname = "datos.db")
dbWriteTable(conect, name = 'prestadores', value = prestadores, row.names = FALSE, overwrite = TRUE)# creacion tabla vacia de prestadores
dbWriteTable(conect, name = 'municipios', value = municipios, row.names = FALSE, overwrite = TRUE)# creacion tabla vacia de municipios

# estandarizacion de municipios y departamtentos que presentan errores de tipeo
#estandarizar bogota 

dbExecute(conect, "UPDATE municipios
          SET Departamento = 'bogotad.c'
          WHERE Departamento = 'bogotadc';")
dbExecute(conect, "UPDATE municipios
          SET Municipio = 'bogotad.c'
          WHERE Municipio = 'bogotadc';")
dbExecute(conect, "UPDATE prestadores
          SET muni_nombre = 'bogotad.c'
          WHERE muni_nombre = 'bogota';")

#corregimos errores de tipeo en los municipios y departamentos 
depa_municipios <- as.vector(unique(dbGetQuery(conect,"SELECT * FROM municipios")$Departamento))
depa_prestadores <- as.vector(unique(dbGetQuery(conect,"SELECT * FROM prestadores")$depa_nombre))
c(depa_municipios[!(depa_municipios %in% depa_prestadores)], depa_prestadores[!(depa_prestadores %in% depa_municipios)])# cambiar por algo mas descri

dbExecute(conect,"UPDATE prestadores
SET depa_nombre = CASE 
WHEN depa_nombre = 'barranquilla' THEN 'atlantico'
WHEN depa_nombre = 'buenaventura' THEN 'valledelcauca'
WHEN depa_nombre = 'cali' THEN 'valledelcauca'
WHEN depa_nombre = 'cartagena' THEN 'bolivar'
WHEN depa_nombre = 'santamarta' THEN 'magdalena'
ELSE depa_nombre
END;")


# preprando datos para analsiis temporal de las fechas de radicacion

dbExecute(conect, 'ALTER TABLE prestadores ADD COLUMN ano_radi INTEGER')
dbExecute(conect, 'ALTER TABLE prestadores ADD COLUMN mes_radi INTEGER')
dbExecute(conect, 'ALTER TABLE prestadores ADD COLUMN dia_radi INTEGER')

dbExecute(conect,"UPDATE prestadores
    SET 
    ano_radi = CAST(SUBSTR(fecha_radicacion, 1, 4) AS INTEGER),
    mes_radi = CAST(SUBSTR(fecha_radicacion, 5, 2) AS INTEGER),
    dia_radi = CAST(SUBSTR(fecha_radicacion, 7, 2) AS INTEGER);")


# caratecrizacion de los datos
# proporcion de las clase de persona

clase <-dbGetQuery(conect,"SELECT  clase_persona,
                        COUNT(*) AS Tipo,
                        COUNT(*) * 100.0 / (SELECT COUNT(*) FROM prestadores) AS porcentaje
                        FROM prestadores
                        GROUP BY clase_persona;")
clase

ggplot(clase, aes(x = "", y = porcentaje, fill = clase_persona)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de Clase de Personas") +
  scale_fill_manual(values = c("#FF9999", "#66CC99")) +  # Colores personalizados
  theme_minimal() +
  theme(legend.position = "right") 


# agragar tasa o estadisticos
# objeto social 
tipo_org <- dbGetQuery(conect,"SELECT  clpr_nombre,
                        COUNT(*) AS cantidad,
                        COUNT(*) * 100.0 / (SELECT COUNT(*) FROM prestadores) AS porcentaje
                        FROM prestadores
                        GROUP BY clpr_nombre;")
tipo_org

# dependencia de entidades privadas 
tipo_privacidad <- dbGetQuery(conect,"SELECT  naju_nombre,
                        COUNT(*) AS cantidad,
                        COUNT(*) * 100.0 / (SELECT COUNT(*) FROM prestadores) AS porcentaje
                        FROM prestadores
                        GROUP BY naju_nombre;")
# cantidad de tipos de estableciomientos 
ggplot(tipo_privacidad, aes(x = "", y = porcentaje, fill = naju_nombre)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de Tipos de Establecimientos de Salud") +
  scale_fill_manual(values = c("#FF9999", "#66CC99", "#9999FF")) +  # Colores personalizados
  theme_minimal() +
  theme(legend.position = "right") 

# departamentos que mas solicitaron prestadores 
# departamentos mas importantes
departamentos_imp <- dbGetQuery(conect,"SELECT depa_nombre, COUNT(*) AS cantidad
           FROM prestadores
           GROUP BY depa_nombre
           ORDER BY cantidad DESC
           LIMIT 5;")
# todos los departamentos para analsiis espacial
conti_departamento <- dbGetQuery(conect,"SELECT depa_nombre, COUNT(*) AS cantidad
           FROM prestadores
           GROUP BY depa_nombre
           ORDER BY cantidad DESC;")

# relacion entre la numero de prestadores y la poblacion del departamentos 
comp_poblacion <- dbGetQuery(conect,"SELECT p.depa_nombre, COUNT(*) AS cantidad, m.poblacion AS poblacion_total 
          FROM prestadores p
          JOIN (
            SELECT departamento, SUM(poblacion) AS poblacion
            FROM municipios
            GROUP BY departamento
          ) m ON m.Departamento = p.depa_nombre
          GROUP BY p.depa_nombre
          ORDER BY cantidad DESC;")

# complementacion grafica de distribucion 
comp_poblacion$porcentaje_de_la_poblacion <- comp_poblacion$cantidad/comp_poblacion$poblacion_total

hist(comp_poblacion$porcentaje_de_la_poblacion,xlab = 'Porcentaje de poblacion', ylab = 'Frecuencia',main = 'distribucion del porcentaje de prestadores por departamento')
mean(comp_poblacion$porcentaje_de_la_poblacion)


dbGetQuery(conect,"SELECT p.depa_nombre, COUNT(*) AS cantidad, m.poblacion AS poblacion_total 
          FROM prestadores p
          JOIN (
            SELECT departamento, SUM(poblacion) AS poblacion
            FROM municipios
            GROUP BY departamento
          ) m ON m.Departamento = p.depa_nombre
          GROUP BY p.depa_nombre
          ORDER BY cantidad
          LIMIT 5;")

# numero de prestadores por region
dbGetQuery(conect,"SELECT m.Region , COUNT(*) AS num_prestadores
                FROM prestadores p
                JOIN (
                SELECT Departamento, Region
                FROM municipios
                GROUP BY Departamento, Region
                ) m ON m.Departamento = p.depa_nombre
                GROUP BY m.Region
                ORDER BY num_prestadores DESC;")
# numeor de contratos por cada año registrado 
#por dapartamento
resultados <- dbGetQuery(conect, "SELECT depa_nombre, ano_radi, mes_radi, COUNT(*) as conteo 
                         FROM prestadores 
                         GROUP BY depa_nombre, ano_radi, mes_radi")
ggplot(resultados, aes(x = as.Date(paste(ano_radi, mes_radi, "01", sep = "-")), y = conteo, color = depa_nombre)) +
  geom_line() +
  labs(x = "Fecha", y = "Cantidad de Datos", title = "Avance en el Tiempo por Departamento") +
  theme_minimal()

# por region

dbExecute(conect,"ALTER TABLE prestadores ADD COLUMN region VARCHAR(255);")

dbExecute(conect,"UPDATE prestadores
SET region = (
    SELECT municipios.region
    FROM municipios
    WHERE prestadores.depa_nombre = municipios.Departamento and municipios.Municipio = prestadores.muni_nombre 
);")
datos_por_region <- dbGetQuery(conect, "SELECT region, ano_radi, mes_radi, COUNT(*) as conteo 
                         FROM prestadores 
                         GROUP BY region, ano_radi, mes_radi")

sum(datos_por_region$conteo)

ggplot(datos_por_region, aes(x = as.Date(paste(ano_radi, mes_radi, "01", sep = "-")), y = conteo, color = region)) +
  geom_line() +
  labs(x = "Fecha", y = "Cantidad de Datos", title = "Avance en el Tiempo por Departamento") +
  theme_minimal()

## mapas de conteos
library(dplyr)
### 
conti_departamento <- conti_departamento %>%
  mutate(depa_nombre = case_when(
    depa_nombre == "amazonas" ~ "AMAZONAS",
    depa_nombre == "antioquia" ~ "ANTIOQUIA",
    depa_nombre == "arauca" ~ "ARAUCA",
    depa_nombre == "atlantico" ~ "ATLÁNTICO",
    depa_nombre == "bogotad.c" ~ "BOGOTÁ, D.C.",
    depa_nombre == "bolivar" ~ "BOLÍVAR",
    depa_nombre == "boyaca" ~ "BOYACÁ",
    depa_nombre == "caldas" ~ "CALDAS",
    depa_nombre == "caqueta" ~ "CAQUETÁ",
    depa_nombre == "casanare" ~ "CASANARE",
    depa_nombre == "cauca" ~ "CAUCA",
    depa_nombre == "cesar" ~ "CESAR",
    depa_nombre == "choco" ~ "CHOCÓ",
    depa_nombre == "cordoba" ~ "CÓRDOBA",
    depa_nombre == "cundinamarca" ~ "CUNDINAMARCA",
    depa_nombre == "guainia" ~ "GUAINÍA",
    depa_nombre == "guaviare" ~ "GUAVIARE",
    depa_nombre == "huila" ~ "HUILA",
    depa_nombre == "laguajira" ~ "LA GUAJIRA",
    depa_nombre == "magdalena" ~ "MAGDALENA",
    depa_nombre == "meta" ~ "META",
    depa_nombre == "narino" ~ "NARIÑO",
    depa_nombre == "nortedesantander" ~ "NORTE DE SANTANDER",
    depa_nombre == "putumayo" ~ "PUTUMAYO",
    depa_nombre == "quindio" ~ "QUINDIO",
    depa_nombre == "risaralda" ~ "RISARALDA",
    depa_nombre == "sanandres" ~ "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA",
    depa_nombre == "santander" ~ "SANTANDER",
    depa_nombre == "sucre" ~ "SUCRE",
    depa_nombre == "tolima" ~ "TOLIMA",
    depa_nombre == "valledelcauca" ~ "VALLE DEL CAUCA",
    depa_nombre == "vaupes" ~ "VAUPÉS",
    depa_nombre == "vichada" ~ "VICHADA",
    TRUE ~ depa_nombre
  ))
tabla_espacial <- read_sf('MGN_DPTO_POLITICO.shp')

tabla_espacial <- tabla_espacial %>%
  left_join(conti_departamento,by=c("DPTO_CNMBR"="depa_nombre"))

tabla_espacial %>% 
  ggplot(mapping = aes(fill = cantidad)) +
  geom_sf() +
  scale_fill_viridis_c(
    trans = "log10",
    breaks = trans_breaks(trans = "log10",
                          inv = function(x) round(10 ^ x, digits = 0))
  ) +
  labs(fill = "Cantidad de prestadores") +
  theme_minimal() +  # Puedes cambiar a theme_bw(), theme_gray(), u otra
  theme(panel.grid = element_line(color = "gray", linetype = "dotted"))
#dbDisconnect(conect)
