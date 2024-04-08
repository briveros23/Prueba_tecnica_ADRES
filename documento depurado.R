# direccion de trabajo 
# pc 1 C:/Users/bcriv/OneDrive/Escritorio/Prueba_tecnica
# pc 2 C:/Users/bcriv/Desktop/Prueba_tecnica/Prueba_tecnica_ADRES
setwd('C:/Users/bcriv/Desktop/Prueba_tecnica/Prueba_tecnica_ADRES')
# carga de datos 
library(openxlsx)
library(readxl)
library(sqldf)
library(ggplot2)


### lectura de datos 
municipios <- read.xlsx("Municipios.xlsx")
prestadores <- read.xlsx("Prestadores.xlsx")

# caracteres extraños 

municipios$Departamento <- gsub("[^[:alnum:]\\s]", "", municipios$Departamento)
municipios$Municipio <- gsub("[^[:alnum:]\\s]", "", municipios$Municipio)

#espacios en blanco

r <- gsub("\\s+", "", df$columna_a_limpiar)
# minimizar y quitar tildes

prestadores$depa_nombre <- tolower(prestadores$depa_nombre)
prestadores$muni_nombre <- tolower(prestadores$muni_nombre)
prestadores$depa_nombre <- iconv(prestadores$depa_nombre, to = "ASCII//TRANSLIT")
prestadores$muni_nombre <- iconv(prestadores$muni_nombre, to = "ASCII//TRANSLIT")


municipios$Departamento <- tolower(municipios$Departamento)
municipios$Municipio <- tolower(municipios$Municipio)
municipios$Departamento <- iconv(municipios$Departamento, to = "ASCII//TRANSLIT")
municipios$Municipio <- iconv(municipios$Municipio, to = "ASCII//TRANSLIT")

# cargue de los datos a SQLite
conect <- dbConnect(SQLite(), dbname = "datos.db")
dbWriteTable(conect, name = 'prestadores', value = prestadores, row.names = FALSE, overwrite = TRUE)
dbWriteTable(conect, name = 'municipios', value = municipios, row.names = FALSE, overwrite = TRUE)

#estandarizar bogota 

dbExecute(conect, "UPDATE municipios
          SET Departamento = CASE 
          WHEN Departamento = 'bogotadc' THEN 'bogota d.c'
          ELSE Departamento END
          WHERE Departamento = 'bogotadc';")

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
dbGetQuery(conect,"SELECT  clase_persona,
                        COUNT(*) AS clase_persona,
                        COUNT(*) * 100.0 / (SELECT COUNT(*) FROM prestadores) AS porcentaje
                        FROM prestadores
                        GROUP BY clase_persona;")

# objeto social 
dbGetQuery(conect,"SELECT  clpr_nombre,
                        COUNT(*) AS cantidad,
                        COUNT(*) * 100.0 / (SELECT COUNT(*) FROM prestadores) AS porcentaje
                        FROM prestadores
                        GROUP BY clpr_nombre;")

# dependencia de entidades privadas 
dbGetQuery(conect,"SELECT  naju_nombre,
                        COUNT(*) AS cantidad,
                        COUNT(*) * 100.0 / (SELECT COUNT(*) FROM prestadores) AS porcentaje
                        FROM prestadores
                        GROUP BY naju_nombre;")

# departamentos que mas solicitaron prestadores 
dbGetQuery(conect,"SELECT depa_nombre, COUNT(*) AS cantidad
           FROM prestadores
           GROUP BY depa_nombre
           ORDER BY cantidad DESC
           LIMIT 5;")

# relacion entre la numero de prestadores y la poblacion del departamentos 
dbGetQuery(conect,"SELECT p.depa_nombre, COUNT(*) AS cantidad, m.poblacion AS poblacion_total 
          FROM prestadores p
          JOIN (
            SELECT departamento, SUM(poblacion) AS poblacion
            FROM municipios
            GROUP BY departamento
          ) m ON m.Departamento = p.depa_nombre
          GROUP BY p.depa_nombre
          ORDER BY cantidad DESC
          LIMIT 5;")

# como extraer 
dbGetQuery(conect,"SELECT m.region, COUNT(P.id_prestador) AS num_prestadores
                FROM Departamentos D
                JOIN Prestadores P ON D.id_departamento = P.id_departamento
                GROUP BY D.region;")
dbGetQuery(conect,"SELECT m.region, COUNT(P.id_prestador) AS num_prestadores
                FROM Departamentos D
                JOIN Prestadores P ON D.id_departamento = P.id_departamento
                GROUP BY D.region;")
dbGetQuery(conect,"SELECT  Departamento, Region 
           FROM municipios
           GROUP BY Departamento")
xd <- dbGetQuery(conect,"SELECT m.Region, COUNT(p.nits_nit) AS num_prestadores
FROM prestadores p
JOIN (
    SELECT Departamento, Region
    FROM municipios
    GROUP BY Departamento, Region
) m ON not m.Departamento = p.depa_nombre
GROUP BY m.Region;")

#dbDisconnect(conect)
#

