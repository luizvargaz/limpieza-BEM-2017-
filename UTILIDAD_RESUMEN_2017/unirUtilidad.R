#datos <- data.frame(a = c(1,3,6,7), b = c('e', 'a', 't', 'y'), c = c('age', 'sex', 'age', 'sex'))

library('tools')
library(readxl)

setwd('C:/Users/LVARGAS/Documents/CIMMYT/dataBase/2018/limpieza datos 2017/UTILIDAD_RESUMEN_2017')

archivos <- list.files()

conteo <- 1

for(archivo in archivos){
        
        print(paste('... Leyendo el archivo', archivo))
        extension <- file_ext(archivo)
        print(paste('El archivo tiene la extension', extension))
        
        if(extension == 'xlsx'){
                
                tablaDatos <- read_excel(archivo)
                dimension <- dim(tablaDatos)
                print(paste('Registros:', dimension[1]))
                
                if(conteo == 1){
                        
                        datos <- tablaDatos
                        
                }else{
                        
                        datos <- rbind(datos, tablaDatos)
                        
                }
                
                conteo = conteo + 1
                
        }
        
} 


dim(datos)

write.csv(datos, 'utilidad_2017.csv', row.names = FALSE)

