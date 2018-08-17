c("#698B22")
library(readxl)
library(magrittr)
library(dplyr)
library(lubridate)
library(RPostgreSQL)
library(readr)
library(readxl)

options(scipen = 999)

Graduados <- read_excel("data/Graduados-I-2017.xls")
Cursos <- read_excel("data/Cursos-I-2017.xls")
Estudiantes <- read_excel("data/Estudiantes-I-2017.xls")

# Pone todos los nombres de las variables en minusculas
names(Graduados) %<>% tolower()
names(Cursos) %<>% tolower()
names(Estudiantes) %<>% tolower()


# Arregla el carne en los casos en los que no esta bien:

# OBVIAR HASTA SIGUIENTE MARCA

# Selcción cursos únicos por cada estudiante. Elimina cursos duplicados.
#u <- unique(Cursos[, 2:5])
# Reordenamiento por carne, nombre, 
#u <- u[, c(1, 3, 2, 4)]
#  Reordena por Carne, Nombre completo.
#a <- Estudiantes[, 3:6]
# Toma estudiantes que no matricularon cursos orden Apellido1, Apellido2, Nombre.
#b <- anti_join(a, u)

#
#
#b1 <- anti_join(b[, 2:4], u[, 2:4])
#bf <- anti_join(b, b1)
#bf[, 1] <- inner_join(bf[, 2:4], u)[, 4]
#for (i in 1:nrow(bf)) {
#  num <- which(Estudiantes$APELLIDO1 %in% bf[i, 2] & 
#                 Estudiantes$APELLIDO2 %in% bf[i, 3] & 
#                 Estudiantes$NOMBRE %in% bf[i, 4])
#  Estudiantes[num, 3] <- bf[i, 1]
#}

## ESTUDIANTES==================================================================

# Agrega el año de ingreso a la UCR con base en el carne:
# s Son los años de ingresos de cada estudiantes
s <- substr(Estudiantes$CARNE, start = 1, stop = 2)
#estudiantes con carne numerico
s[which(!is.na(as.numeric(s)))] %<>% paste("19", ., sep = "")
#estudiantes con carne  alfanumerico
s %<>% gsub("A", "200", .)
s %<>% gsub("B", "201", .)

# Se utilizó el siguiente código para los estudianes que no tenían año ingreso.

#num <- which(is.na(s == Estudiantes$ANO_INGRESO_UCR))
# Estudiantes[num, 2] <- s[num]

# Años de ingreso en formato numérico a la tabla de estudiantes.
Estudiantes$ANO_INGRESO_UCR <- as.numeric(s)

# Ordena las columnas para que año de ingreso quede de segundo.
Estudiantes <- Estudiantes[, c("ID", "ANO_INGRESO_UCR", names(Estudiantes)[-c(1, 13)])]

#Cambiar el nombre de año por ANO para evitar la ñ.
names(Cursos)[11] <- "ANO"




#Graduados$fecha_juramentacion %<>% ymd()

Cursos$nombre_curso %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
Estudiantes$canton %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
Estudiantes$distrito %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)

conn <- dbConnect(PostgreSQL(), host = "localhost", user = "postgres", 
                  dbname = "estudiantes", password = "admin01")

postgresqlpqExec(conn, "SET client_encoding = 'windows-1252'")

# Cambio de nombre para facilidad de manipulacion (datos nuevos)
est <- Estudiantes

# Carga de los datos de la tabla de estudiantes de la base de datos a R
estudiantes <- dbGetQuery(conn, "SELECT * FROM estudiantes;")

# Verificar cuales estudiantes son realmente nuevos (no están en la base de datos)
est. <- anti_join(est, estudiantes, by = "carne")

# Solamente extrae las variables que interesan en el orden deseado
est. <- select(est., "ano_ingreso_ucr", "carne", "apellido1", "apellido2", 
               "nombre", "provincia", "canton", "distrito", "genero", 
               "fecha_nacimiento", "nombre_colegio", "promedio_admision")

#Finalmente, exportando estudiantes a la base de datos
dbWriteTable(conn, "estudiantes", value = est., append = TRUE, 
             row.names = FALSE)

## CURSOS=======================================================================

# Eliminar tildes de nombres de cursos y ponerlos en mayuscula
Cursos$nombre_curso %<>% toupper() %>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)

# Cambia codigos alfanumericos por 0 en las notas obtenidas
Cursos$nota_ordinaria_num %<>% as.numeric()
Cursos$nota_ordinaria_num[is.na(Cursos$nota_ordinaria_num)] <- 0

# Agregamos el periodo y el año
Cursos$periodo <- "1"
Cursos$ano <- 2017

# Cargamos la tabla 'cursos' de la base de datos a R
n <- dbGetQuery(conn, "SELECT * FROM cursos")

# Escogemos los cursos matriculados por estudiantes que no estén todavia en la base de datos
notas. <- anti_join(Cursos, n, by = c("carne", "sigla", "ano", "periodo"))

# Escogemos el ultimo id que esta en la base de datos
M <- dbGetQuery(conn, "SELECT max(id) FROM cursos")
# Lo hacemos numerico
M %<>% as.numeric()
# Asignamos un id a cada entrada nueva para la base de datos
if(nrow(notas.) > 0) {
  notas.$id <- seq(M + 1, M + nrow(notas.))
}

# Acomodamos las columnas en el orden de la base de datos
notas. <- select(notas., "id", "carne", "sigla", "nombre_curso", 
                 "nota_ordinaria_num", "creditos", "periodo", "ano")	

# Agregamos los registros nuevos a la base de datos
dbWriteTable(conn, "cursos", value = notas., append = TRUE, 
             row.names = FALSE)



# DEPURACION GRADUADOS  ============================================

#names(Graduados)
# Ahora viene el año de juramentacion.
names(Graduados)[5]<-"ano_juramentacion"



# Elimina las tildes en el nombre.
Graduados$apellido1 %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
Graduados$apellido2 %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)
Graduados$nombre %<>% chartr('ÁÉÍÓÚÑ','AEIOUN', .)

# Verificar el código del título para cada estudiante graduado.
# Graduados$cod_titulo
Graduados$cod_titulo %<>% as.character()


#Graduados$CARNE

s <- substr(Graduados$carne, start = 1, stop = 2)
#estudiantes con carne numerico
s[which(!is.na(as.numeric(s)))] %<>% paste("19", ., sep = "")
#estudiantes con carne  alfanumerico
s %<>% gsub("A", "200", .)
s %<>% gsub("B", "201", .)



grad <- dbGetQuery(conn, "SELECT carne, 
                                EXTRACT(year FROM fecha_juramentacion) AS ano_juramentacion 
                          FROM graduados;")



# Selecciona graduados nuevos por ingresar a la base de datos.
grad. <- anti_join(Graduados, grad)

grad.$ano_juramentacion %<>% paste("1-1-", ., sep = "")
grad.$ano_juramentacion <- ymd(dmy(grad.$ano_juramentacion))

#names(grad.)
names(grad.)[5]<-"fecha_juramentacion"



grad. <- select(grad., "carne", "apellido1", "apellido2", 
              "nombre", "fecha_juramentacion", "cod_titulo", 
              "descripcion_titulo")

dbWriteTable(conn, "graduados", value = grad., append = TRUE, 
             row.names = FALSE)


##FIN===============================================================================================

