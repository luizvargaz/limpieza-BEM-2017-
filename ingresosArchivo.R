# To call the libraries 
library(readxl)
library(dplyr)
library(stringr)
library(stringi)


##### To set the work directory
setwd('C:/Users/LVARGAS/Documents/CIMMYT/dataBase/2018/limpieza datos 2017')

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

##### Obtener las bases de datos
utilidadRaw <- read.csv('utilidad_2017.csv')
dim(utilidadRaw)
riegos <- as.data.frame(read_excel('EXPORTAR_2017.xlsx', '20_riegos_Descripcion'))


rendimientoRaw <- as.data.frame(read_excel('EXPORTAR_2017.xlsx', '24_rendimiento'))

#regiones <- read_excel('C:/Users/LVARGAS/Documents/CIMMYT/dataBase/INEGI municipios/regionesOficiales.xlsx', 1)

#### Eliminar registros NA, los registros de Áreas de Impacto y los duplicados
valoresNA <- is.na(utilidadRaw[,1])
utilidadNA <- utilidadRaw[!valoresNA,]
dim(utilidadNA)
utilidadDupl <- utilidadNA[!duplicated(utilidadNA[,19]),]
dim(utilidadDupl)

names(utilidadDupl)
unique(utilidadDupl$nb.Tipo.Parcela)

utilidad <- utilidadDupl[utilidadDupl$nb.Tipo.Parcela != 'Parcela Área de Impacto',]
dim(utilidad)


#### Agregar el tipo de produccion de acuerdo al conteo de número de riegos
tipobitRiegos <- unique(riegos$`ID de tipo de bitácora (clave foránea)`)
dim(riegos)
length(tipobitRiegos)

# Buscar si el id de la bitacora se encuentra en la lista de bitacoras con riego
# El resultado se almacena en un vector con los valores TRUE y FALSE

count = 0
for(tipoBitacora in utilidad$id.Tipo.Bitacora){
        resultado = any(tipobitRiegos == tipoBitacora)
        #print(resultado)
        if(count == 0){
                
                sumResultado = resultado
        }else{
                
                sumResultado = c(sumResultado, resultado)  
        }
        
        count = count + 1
        
}

length(sumResultado)
#length(renCultivoProductoUnidadv1$`ID de tipo de bitácora (clave foránea)`)
#### convertir los valores de vector en  Riego o Temporal

tipoProduccion <- transform(sumResultado,sumResultado = ifelse(sumResultado == 'FALSE','Temporal','Riego'))

length(tipoProduccion$sumResultado )

#### Agregar el tipo de produccion en la tabla utilidad
dim(utilidad)
utilidad$tipoProduccion <- tipoProduccion$sumResultado


# Agregar los valores de utilidad a todos los valores de rendimiento
dim(rendimientoRaw)
valoresNA <- is.na(rendimientoRaw[,1])
rendimientoNA <- rendimientoRaw[!valoresNA,]
rendimientoDupl <- unique(rendimientoNA)
dim(rendimientoDupl)

rend <- rendimientoDupl[rendimientoDupl$`Tipo de parcela (testigo o innovación)` != 'Parcela Área de Impacto',]

names(rend)[2] <- 'id Tipo Bitacora'
dim(rend)
subRend <- rend$`id Tipo Bitacora`
rendimiento <- unique(subRend)
length(rendimiento)

names(utilidad)[19] <- 'id Tipo Bitacora'
dim(utilidad)


utilidadRen <- utilidad <- utilidad[utilidad[,19] %in% rendimiento,]
dim(utilidadRen) #$$$$$$$$$$$$$$$$$$$


# Construir la función para encontrar valores extremo superior y extremo inferior
extremos <- function(vectorDatos, rendimiento = 'NO'){ # Si es analisis de rendimiento, colocar SI al usar la funcion, para evitar obtener un valor minimo negativo
        q75 <- quantile(vectorDatos, 0.75)
        q25 <- quantile(vectorDatos, 0.25)
        ric <- q75 - q25
        valorMaximo <- q75 + (ric * 1.5)
        valorMaximo <- as.vector(valorMaximo)
        if(rendimiento == 'SI'){
                valorMinimo = 0
        }else{
                valorMinimo <- q25 - (ric * 1.5)
                valorMinimo <- as.vector(valorMinimo)
        }
        
        valores <- c(valorMaximo, valorMinimo)
        print("Los valores maximo y minimo son..................")
        print(valores)
}


# Código que obtiene subconjuntos de datos de acuerdo al cultivo, producto, unidad y tipo de producción. Obtiene los valores atípicos 
# de cada subconjunto y construye un nuevo conjunto de datos sin dichos valores. Después por cada estado construye un boxplot de con 
# los valores de precio del producto.

# NOTA: En este caso solo se omiten los valores atípicos inferiores, ya que en la práctica si es posible que algunos productores obtengan 
# un precio alto por la venta del producto de interés económico cosechado.


utRenSinNA <- utilidadRen
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
names(utRenSinNA)

vectorTipo <- unique(utRenSinNA$tipoProduccion)

conteoParaArchivo <- 0

for(tipo in vectorTipo){
        utRenTipOut <- utRenSinNA[utRenSinNA$tipoProduccion == tipo, ]
        dim(utRenTipOut)
        unique(utRenTipOut$tipoProduccion)
        
        valoresExtremos <- extremos(utRenTipOut$INGRESOS....ha.)
        
        # ````````````````````````````````````````````````````````````````````````````````````````
        # Encontrar los outliers de un vector de datos, despues almacenalos en una nueva variable
        # ````````````````````````````````````````````````````````````````````````````````````````
        ## Validar si un valor es un outlier, guardar T o F en un vector
        count = 0       
        for(i in utRenTipOut$INGRESOS....ha.){
                if(count == 0){
                        if(i > valoresExtremos[1] | i < valoresExtremos[2]){
                                esOutlier = TRUE
                                printEsOutlier = "VALOR ATIPICO"
                        }else{
                                esOutlier = FALSE
                                printEsOutlier = "No atipico"
                        }
                        
                }else{
                        if(i > valoresExtremos[1] | i < valoresExtremos[2]){
                                esOutlier = c(esOutlier, TRUE)
                                printEsOutlier = "VALOR ATIPICO"
                        }else{
                                esOutlier = c(esOutlier, FALSE)
                                printEsOutlier = "No atipico"
                        }
                }
                count = count + 1
                leyenda <- paste(i,"--", printEsOutlier)
                print(leyenda)
                
        }
        
        
        ## Crear una nueva columna en el set de datos con los valores V o F de outliers
        utRenTipOut$INGRESOS_outlier <- esOutlier
        dim(utRenTipOut)
        utRenTip <- utRenTipOut[utRenTipOut$INGRESOS_outlier == FALSE, ]
        dim(utRenTip)
        # ````````````````````````````````````````````````````````````````````````````````````````
        
        # ````````````````````````````````````````````````````````````````````````````````````````
        # Almacenar los subset de datos sin outliers para que al final se escriban en un archivo
        
        if(conteoParaArchivo == 0){
                
                UtilidadFinal <- utRenTip
                
        }else{
                
                UtilidadFinal <- rbind(UtilidadFinal, utRenTip)
                
        }
        
        conteoParaArchivo <- conteoParaArchivo + 1
        # Fin de almacenar los subset de datos sin outliers para que al final se escriban en un archivo
        # ````````````````````````````````````````````````````````````````````````````````````````
        
        
}

dim(UtilidadFinal)
names(UtilidadFinal)
utilidadLimpia <- UtilidadFinal[,c(1, 19, 2:18, 20:27, 49, 52)]
names(utilidadLimpia)[1] <- 'ID de la bitácora'
names(utilidadLimpia)[2] <- 'ID de tipo de bitácora'


if(!dir.exists('./salida')){dir.create('./salidaIngresos')}

nombreArchivo <- paste('./salidaIngresos/', 'ingresos2017.csv')
nombreArchivo <- str_replace_all(nombreArchivo, pattern=" ", repl="")
write.csv(utilidadLimpia, file = nombreArchivo, row.names = FALSE)


############################################################################
##################################### PROMEDIOS ############################
names(UtilidadFinal)

promediosMunicipio <- with(UtilidadFinal, aggregate(`COSTOS PRODUCCION ($/ha)`, by = list(`nb Estado`, `nb Municipio`, Anio, `nb Ciclo`, tipoProduccion), FUN = function(`COSTOS PRODUCCION ($/ha)`) c(Promedio = mean(`COSTOS PRODUCCION ($/ha)`), Conteo = length(`COSTOS PRODUCCION ($/ha)`) )))

names(promediosMunicipio) <- c('Estado', 'Municipio', 'Año', 'Ciclo agrónomico', 'Tipo de producción', 'Costos de produccion ($/ha)')

promediosMunicipio <- promediosMunicipio[,c(3, 4, 1, 2, 5, 6)]


promediosEstado <- with(UtilidadFinal, aggregate(`COSTOS PRODUCCION ($/ha)`, by = list(`nb Estado`, Anio, `nb Ciclo`, tipoProduccion), FUN = function(`COSTOS PRODUCCION ($/ha)`) c(Promedio = mean(`COSTOS PRODUCCION ($/ha)`), Conteo = length(`COSTOS PRODUCCION ($/ha)`) )))

names(promediosEstado) <- c('Estado', 'Año', 'Ciclo agrónomico', 'Tipo de producción', 'Costos de produccion ($/ha)')

promediosEstado <- promediosEstado[,c(3, 4, 1, 2, 5)]


promediosHub <- with(UtilidadFinal, aggregate(`COSTOS PRODUCCION ($/ha)`, by = list(`nb Hub`, Anio, `nb Ciclo`, tipoProduccion), FUN = function(`COSTOS PRODUCCION ($/ha)`) c(Promedio = mean(`COSTOS PRODUCCION ($/ha)`), Conteo = length(`COSTOS PRODUCCION ($/ha)`) )))

names(promediosHub) <- c('Hub', 'Año', 'Ciclo agrónomico', 'Tipo de producción', 'Costos de produccion ($/ha))')

promediosHub <- promediosHub[,c(3, 4, 1, 2, 5)]


#####################################################################################
#####################################################################################


#### Almacenar los datos de rendimiento obtenidos 

if(!dir.exists('./salidaCostos')){dir.create('./salidaCostos')}

nombreArchivoMunicipio <- paste('./salidaCostos/', cultivo,'_Costos_Municipio','.csv')
nombreArchivoMunicipio <- str_replace_all(nombreArchivoMunicipio, pattern=" ", repl="")
write.csv(promediosMunicipio, file = nombreArchivoMunicipio, row.names = FALSE)

nombreArchivoEstado <- paste('./salidaCostos/', cultivo,'_Costos_Estado','.csv')
nombreArchivoEstado <- str_replace_all(nombreArchivoEstado, pattern=" ", repl="")
write.csv(promediosEstado, file = nombreArchivoEstado, row.names = FALSE)

nombreArchivoHub <- paste('./salidaCostos/', cultivo,'_Costos_Hub','.csv')
nombreArchivoHub <- str_replace_all(nombreArchivoHub, pattern=" ", repl="")
write.csv(promediosHub, file = nombreArchivoHub, row.names = FALSE)

nombreArchivoCom <- paste('./salidaCostos/', cultivo,'_Costos_Completa','.csv')
nombreArchivoCom <- str_replace_all(nombreArchivoCom, pattern=" ", repl="")
write.csv(UtilidadFinal, file = nombreArchivoCom, row.names = FALSE)

#eliminadas <- paste('Se eliminaron', numObservacionesTotal - numObservacionesFinal, 'observaciones, de un total de', numObservacionesTotal)
#print(eliminadas)
