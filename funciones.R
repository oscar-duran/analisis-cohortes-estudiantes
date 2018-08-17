analisis <- function(connection, inicio = 2016, fin = NULL) {
  require(RPostgreSQL)
  require(magrittr)
  require(xlsx)
  require(dplyr)
  
  #inicio <- 2016
  #fin <-2017
  #connection = conn
  
  # 'est' es la matriz completa de los estudiantes que han matriculado Estadística Introductoria I
  
  est <- dbGetQuery(connection, "SELECT carne, ano, nombre_curso, sigla 
                    FROM cursos 
                    WHERE sigla = 'XS1110' OR sigla = 'XS0111' ;")
  
  # Codificación apropiada de la base de datos
  
  postgresqlpqExec(connection, "SET client_encoding = 'windows-1252'")
  
  # 'est1' es la agrupación de los carnés según el primer año en que matricularon Estadística Introductoria I 
  # Tambien puede ser visto como el año en que ingresaron a Estadística
  
  est1 <- aggregate(est$ano, by = list(est$carne), FUN = min)
  
  # 'u' corresponde a los años únicos en los cuales al menos un estudiante se ha matriculado en Estadística
  
  u <- unique(est$ano)
  
  # Ahora se ordena 'u' de menor a mayor
  
  u <- u[order(u)]
  
  # Lo siguiente son verificaciones de rango de años para el cálculo de las tablas:
  
  if(is.null(fin)) {
    fin <- max(u)
  }
  if(inicio > fin) {
    stop(paste("La fecha de inicio (", inicio, 
               ") es mayor a la fecha de finalización (", fin, ")", sep = ""))
  }
  if(inicio < min(u)) {
    message(paste("La fecha de inicio (", inicio, 
                  ") es menor al primer año disponible (", min(u),
                  ") por lo que se empezará desde el ", min(u), sep = ""))
    inicio <- min(u)
  }
  if(fin > max(u)) {
    message(paste("La fecha de finalización (", fin, 
                  ") es mayor al último año disponible (", max(u),
                  ") por lo que se terminará en el ", max(u), sep = ""))
    fin <- max(u)
  }
  
  # 'M' es la cantidad de años en los cuales se ha matriculado al menos una persona en Estadística
  
  M <- length(u)
  
  # Creacion de la matriz vacia que vamos a llenar con los datos, tiene M columnas y filas, para los estudiantes matriculados en cada año,
  # una columna extra para graduados y una fila extra para retiros:
  
  df <- data.frame(matrix(0, ncol = M + 1, nrow = M + 1), 
                   row.names = c(as.character(u), "Retiro"))
  
  # Los nombres de las columnas sean el año correspondiente y 'graduados'
  
  colnames(df) <- c(as.character(u), "Graduados")
  
  # Esta es el listado de siglas de todos los cursos en el plan de estudios (se puede hacer una tabla en la base de datos)
  
  l.c <- c("XS0111", "XS0113", "XS0121", "XS0211", "XS0212", "XS0220", "XS0221", 
           "XS0222", "XS0223", "XS0312", "XS0313", "XS0314", "XS0321", "XS0322",
           "XS0323", "XS0324", "XS0411", "XS0412", "XS0413", "XS0414", "XS0421",
           "XS0422", "XS0423", "XS0424", "MA0001", "XS1110", "LM1030", "XE0156",
           "XS1130", "MA1021", "XS2210", "XS2310", "XS2110", "MA1004", "XS2230",
           "XS2330", "XS2130", "MA1023", "XS3150", "XS3110", "XS3310", "XS3010",
           "XS3170", "XS3130", "XS3210", "XS3510", "XS4410", "XS4510", "XS4110",
           "XS4010", "XS4430", "XS4530", "XS4050", "XS4030", "MA0213", "MA0232",
           "MA0313", "XS3220", "XP5028", "XP5033", "PS0001", "CP1212", "CP1500",
           "FS0107", "FS0101", "FS0115", "B 0350", "LM2003", "LM2004", "LM1032",
           "LM1031", "MA0125")
  
  # Para cada uno de los años de interés:
  
  for(j in 1:M) {
    
    # 'est.' es el carné de las personas que ingresaron a Estadística en el j-ésimo año
    
    est. <- subset(est1, x == u[j])$Group.1
    
    # 'l' es una lista vacía que la vamos a llenar con los cursos matriculados para cada uno de los estudiantes
    
    l <- list()
    
    # Para cada una de las personas de interés se hace:
    
    for(i in 1:length(est.)) {
      
      # Para cada estudiante seleccionado, se consulta de la base de datos todos los cursos matriculados posterior al año de ingreso
      
      l[[i]] <- dbGetQuery(connection, paste("SELECT carne, ano, sigla 
                                             FROM cursos 
                                             WHERE carne = '",
                                             est.[i],
                                             "' AND ano >= ", 
                                             u[j], 
                                             ";", 
                                             sep = ""))
      
      # Se seleccionana solo los cursos que pertenecen al plan de estudios
      
      l[[i]] <- subset(l[[i]], l[[i]]$sigla %in% l.c)
    }
    
    # Para cada estudiante seleccionado:
    
    for(i in 1:length(l)) {
      
      # Le caemos encima a la lista con solamente el listado de los años en los que matriculo un curso en Estadística
      
      l[[i]] <- unique(l[[i]]$ano)
    }
    
    # Convertimos 'l' en un vector
    
    l %<>% unlist()
    
    # Una tabla que tiene, para cada año, la cantidad de estudiantes que matricularon un curso de estadística, 
    # de los estudiantes seleccionados
    
    l %<>% table()
    
    # Para cada año de interés
    
    for(i in 1:length(l)) {
      
      # Selecciona la columna con el año especificado
      
      w <- which(colnames(df) == names(l)[i])
      
      # Para un año de ingreso a Estadística (j) se agregan a la matriz df las personas, de los seleccionados, que matricularon al menos
      # un curso del plan de estudios en ese año
      
      df[j, w] <- l[i]
    }
  }
  
  # Ya con lo anterior rellenamos la matriz pero sin graduados o retiros
  
  # 'grad' es una lista con los carnés de las personas que se graduaron
  
  grad <- dbGetQuery(connection, "SELECT carne FROM graduados;")
  
  # Para cada uno de los estudiantes graduados:
  
  for(i in 1:nrow(grad)) {
    
    # Si el carné empieza con un número, es decir, ingresó a la UCR antes del 2000:
    
    if(grepl("^[0-9]", grad[i, 1])) {
      
      # Se pone como un NA, para no tomarlo en cuenta en el análisis
      
      grad[i, 1] <- NA
    }
  }
  
  # Se elimina de 'grad' las personas que entraron a la UCR antes del 2000
  
  grad %<>% na.omit()
  
  # Para cada uno de los graduados que entraron a la UCR después del 2000:
  
  for(i in 1:nrow(grad)) {
    
    # Calcula el año de ingreso a la UCR (se podría sacar de la tabla 'estudiantes')
    
    grad[i, 1] %<>% gsub("A", "200", .) %>%
      gsub("B", "201", .) %>%
      substr(., 1, 4)
  }
  
  # 'gradl' es una tabla con la cantidad de personas que se han graduado, según el año de ingreso a la UCR
  
  gradl <- table(grad)
  
  # Para cada año de interés
  
  for(i in 1:length(gradl)) {
    
    # Llenamos la columna de graduados con la cantidad de graduados que ingresaron a la UCR en ese año
    
    df[which(rownames(df) == names(gradl)[i]), ncol(df)] <- as.numeric(gradl[i])
  }
  
  # Acorta la matriz a los años solicitados (ineficiente)
  
  df %<>% select(., paste(inicio:fin), "Graduados")
  df <- df[c(paste(inicio:fin), "Retiro"), ]
  
  # Calcula la nueva cantidad de filas de la matriz acortada
  
  M1 <- nrow(df)
  
  # 'cs' es la suma de estudiantes matriculados en estadística para cierto año
  # En apply 1: es filas y 2: es columnas
  
  cs <- apply(df, 2, sum)
  
  # Para todos los año (todas las columnas menos graduados):
  
  for(i in 1:(ncol(df) - 1)) {
    
    # En el primer año no pueden haber retiros
    
    if(i == 1) {
      df[M1, 1] <- 0
      
      # Si es otro año distinto al primero:
      
    } else {
      
      # La cantidad de retiros del año i, con i > 1, se calcula como los estudiantes matriculados en el año i-1 más los estudiantes que
      # ingresaron en el año (es decir, la cantidad de estudiantes que deberían haber si no hubiera retiros) y se le resta la cantidad de
      # estudiantes que efectivamente matricularon en el año i (es decir, la resta corresponde a los estudiantes que matricularon en el
      # año i-1 pero no matricularon en el año i, es decir, retiros)
      
      df[M1, i] <- cs[i - 1] + df[i, i] - cs[i]
    }
  }
  
  # Escribir los resultados en una tabla de Excel
  
  write.xlsx(df, paste("tabla de cohortes ", inicio, "-", fin, ".xlsx", sep = ""))
  message(paste("Los resultados se salvaron en el archivo '" , 
                paste("tabla de cohortes ", inicio, "-", fin, ".xlsx", sep = ""), 
                "'", sep = ""))
  return(df)
}

