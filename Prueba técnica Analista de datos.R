# direccion de trabajo 
setwd('C:/Users/bcriv/OneDrive/Escritorio/Prueba_tecnica')
# carga de datos 
library(openxlsx)
library(readxl)
library(sqldf)
## lectura
municipios <- read.xlsx("Municipios.xlsx")
prestadores <- read.xlsx("Prestadores.xlsx")
### limpieza de los datos
municipios$Departamento <- gsub("[^[:alnum:]\\s]", "", municipios$Departamento)
municipios$Municipio <- gsub("[^[:alnum:]\\s]", "", municipios$Municipio)
## normalizacion de formato 
municipios$Departamento <- paste0(
  toupper(substr(municipios$Departamento, 1, 1)),  # Convertir la primera letra en mayúscula
  tolower(substr(municipios$Departamento, 2, nchar(municipios$Departamento)))  # Convertir el resto en minúsculas
)
municipios$Municipio <- paste0(
  toupper(substr(municipios$Municipio, 1, 1)),  # Convertir la primera letra en mayúscula
  tolower(substr(municipios$Municipio, 2, nchar(municipios$Municipio)))  # Convertir el resto en minúsculas
)
### datos descriptivos 
unique(prestadores$gerente)
prestadores1 <- sqldf("select depa_nombre , muni_nombre, nits_nit, clpr_codigo, clpr_nombre, ese, nivel, caracter, habilitado, fecha_radicacion, fecha_vencimiento, clase_persona, dv, naju_codigo, naju_nombre
                     from prestadores
                     ")

# cargamos los datos en las bases de SQLite
library(readxl)
library(DBI)
library(RSQLite)
# 
conect <- dbConnect(SQLite(), dbname = "datos.db")


# 
municipios <- read.xlsx("Municipios.xlsx")
prestadores <- read.xlsx("Prestadores.xlsx")
dbWriteTable(conect, name = 'prestadores', value = prestadores, row.names = FALSE, overwrite = TRUE)
dbWriteTable(conect, name = 'municipios', value = municipios, row.names = FALSE, overwrite = TRUE)

resultados <- dbGetQuery(con,"select depa_nombre , muni_nombre, nits_nit, clpr_codigo, clpr_nombre, ese, nivel, caracter, habilitado, fecha_radicacion, fecha_vencimiento, clase_persona, dv, naju_codigo, naju_nombre
                     from prestadores
                     ")
### descripcion de los prestadores

resultados <- dbGetQuery(con,"SELECT clase_persona, COUNT(*) AS cantidad FROM prestadores GROUP BY clase_persona;")

# numero y porcentaje del tipo de persona la cual presto su servicio
resultados <- dbGetQuery(con,"SELECT  clase_persona,
                        COUNT(*) AS clase_persona,
                        COUNT(*) * 100.0 / (SELECT COUNT(*) FROM prestadores) AS porcentaje
                        FROM prestadores
                        GROUP BY clase_persona;")

# numero de contratos asociados 


