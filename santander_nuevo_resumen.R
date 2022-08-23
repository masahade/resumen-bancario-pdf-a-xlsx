library(pdftools)
library(stringr)
library(tidyverse)
library(dplyr)
library(lubridate)
library("writexl")
library(purrr)

# eliminar esta linea si se posee distinta ruta de trabajo
setwd("C:/projectoR/resumen_santander_depdf_a_xlsx")


file='./data/Resumen de Cuenta 31-03-2022.pdf'

txt <- pdf_text(file)


destino="./output/resumenes_1.txt"

#write(texto_EXTRACT_2,"./output/resumenes_4.txt",append=TRUE)

write(txt,destino,append=TRUE)

texto_completo=readLines(destino)


# quita las lineas por encima y por debajo de los limites de movimientos

pattern='.*Fecha.*Comprobante.*Movimiento'



texto_EXTRACT_0=str_which(texto_completo,pattern)


minimo=min(texto_EXTRACT_0)+1

#pattern='\\s+Saldo total\\s+\\$'

pattern='.Saldo total'

texto_EXTRACT_0=str_which(texto_completo,pattern)
maximo=max(texto_EXTRACT_0)-1


texto_EXTRACT_1=texto_completo[minimo:maximo]



# quita las lineas vacias
pattern='^[:space:]{60,}.*'

FILTRO=str_detect(texto_EXTRACT_1,pattern)

texto_EXTRACT_2=texto_EXTRACT_1[!FILTRO]
#------ - && ELIMINA LAS CABECERAS 

pattern='.*Fecha.*Comprobante.*Movimiento'
FILTRO=str_detect(texto_EXTRACT_2,pattern)
texto_EXTRACT_3=texto_EXTRACT_2[!FILTRO]

#------SE ACTIVA SOLAMENTE CUANDO QUEREMOS QUE NO FIGURE LOS TRASPASOS ENTRE CUENTAS
#pattern='.*Traspaso de saldo entre cuentas'

#FILTRO=str_detect(texto_EXTRACT_3,pattern)
texto_EXTRACT_4=texto_EXTRACT_3
#texto_EXTRACT_4=texto_EXTRACT_3[!FILTRO]

#------

pattern=''

FILTRO=str_detect(texto_EXTRACT_4,pattern)
texto_EXTRACT_1=texto_EXTRACT_4[FILTRO]

write(texto_EXTRACT_1,"output/resumen_sdf1.txt")



#-----------------
# CREA EL DATAFRAME A PARTIR DE LOS RENGLONES DE TEXTO
archivo=read_fwf('output/resumen_sdf1.txt',
                 fwf_widths(c(8,15,45,120),c("FECHA","VARIOS","DETALLE","IMPORTES"))) 
# CREA UNA COLUMNA INDICE DONDE VA EL ORDEN EN QUE APARECEN EN ARCHIVO DE TEXTO
archivo=archivo %>% mutate(orden=seq(1,nrow(archivo)))

# CONVIERTE LA COLUMNA DE IMPORTES EN COLUMNA CON VECTOR PARA EXTRAER IMPORTES A FORMATO NUMERICO
df4 = archivo %>% mutate( listado = sapply(IMPORTES,str_extract_all, pattern='-?\\$ ?([0-9]+\\.?)?([0-9]+\\.?)?[0-9]+,[0-9]{2}' ))

# extract(archivo,
#           col = "IMPORTES" ,
#           into = c("A_1","A_2","A_3","A_4") ,
#           regex = "(3D).*?(\\d+)$" ,
#           remove = FALSE
#          )


# ---------$ elimina los registros con NA en columnas importe y detalles de forma conjunta

df4 = subset(df4, !is.na(df4$DETALLE) &  !is.na(df4$IMPORTES) ) 

# ---------$
a_num <- function(arg1) {
  
  sapply(arg1,a_numero, simplify = TRUE, USE.NAMES = FALSE)
  
  
}


a_numero <- function(arg1) {
  
  if (is.na(arg1)) {arg1=NA}
  else {
    if ( str_detect(arg1, '\\$ ')) {arg1= str_replace_all(arg1, '\\$ ', '')}
    else { arg1 = arg1}    
    
    if ( str_detect(arg1, '\\.')) {arg1= str_replace_all(arg1, '\\.', '')}
    else { arg1 = arg1}
    
    if ( str_detect(arg1, ','))   {arg1= str_replace(arg1, ',', '.')}
    else { arg1 = arg1}    
    
    if (str_detect(arg1, '-'))   {arg1= str_replace(paste('-',str_replace(arg1, '-', '')),' ','')}  
    else { arg1 = arg1}  }
  
  return(arg1) }





# crea una columna list3 donde pasa a nuemrico los elementos de vectores de calores en columna listado
df4 <- df4 %>% mutate( list3 = sapply(listado,a_num))


# elimina el formto vector dentro de columna list3 y lo pasa a formato cadena de texto
df4 <- df4 %>%
  mutate(score = map_chr(list3, toString))


localizar <- function (arg1) {
  
  pattern='([0-9]+\\.?)?([0-9]+\\.?)?[0-9]+,[0-9]{2}-?'
  b=str_locate_all(arg1,pattern)
  c=b[[1]][2]-b[[1]][3]
  
  return(c)
}

# crea columna posicion (con indice en el campo IMPORTES) para de acuedo al valor saber a que columna corresponde (deb/haber/saldo)
df4 <- df4 %>% mutate(posicion= sapply(IMPORTES,localizar))
#df4 <- df4 %>% mutate(posicion= sapply(IMPORTES,localizar))


df4 <-  df4 %>% fill(FECHA)

df4$score <-  sapply(df4$score,gsub,pattern='NA',replacement='0')

# ----------

comas <- function (arg1) {
  
  if (str_count(arg1,',')==0) { arg1=paste(arg1,',,')}
  else if (str_count(arg1,',')==1) { arg1=paste(arg1,',')}
  else {arg1}
}


df4$score <-  sapply(df4$score,comas)


ejem2 <- function (arg1) {
  
  x=df4$score
  y=df4$posicion
  z=paste(x,',',as.character(y))
  return(z)
  
} 

df6 <- df4 %>% mutate( prueba = ejem2(score))
#df6 <- df4 %>% mutate( prueba = sapply(score,ejem2))


#¿#------------ esta funcion ejem3 asigna los importes a la columna que corresponda
# cuenta corriente / caja de ahorros o saldo per dejandolo separado por una coma

ejem3 <- function (arg1) {

  
  y=arg1
  f=unlist(strsplit(y,','))
  g=suppressWarnings(as.numeric(f))
  
  v0= paste(c(g[1],g[2],g[3]),collapse=",")
  v1=paste(c(NA,g[1],g[2]),collapse=",")
  v2=paste(c(g[1],NA,g[2]),collapse=",")
  
  
  return(
    case_when(
      length(g)==5 ~ v0,
      length(g)==4 & is.na(g[4])  ~ v2,
      length(g)==4 & g[4]<0 & !is.na(g[4])  ~ v0,      
      length(g)==4 & g[4]>=18 & !is.na(g[4]) ~ v2,
      length(g)==4 & g[4]>0 & g[4]<18 & !is.na(g[4]) ~ v1
    )
  )
  
} 

# aplica la funcion 

df6$coco=''


for (x in 1:nrow(df6)) {
  df6$coco[x]=ejem3(df6$prueba[x])
  
}



df7 <- df6 %>% separate(coco, c("CAJA_","CTA_CTE",'SALDO'), ",", convert = TRUE) #%>% str

#---------
df7 <- df7 %>% fill(SALDO)

df7$CAJA_ = df7$CAJA_ %>% replace_na(0)
df7$CTA_CTE  = df7$CTA_CTE %>% replace_na(0)
df7$SALDO = df7$SALDO %>% replace_na(0)


file.remove("output/resumen_sdf1.txt") 
file.remove("output/resumenes_1.txt") 

df7 <- df7 %>% select(orden,FECHA ,VARIOS,DETALLE,CAJA_,CTA_CTE,SALDO)

# crea una columna de control para saber si los saldos coinciden
df7$control <- round(df7$CAJA_+df7$CTA_CTE+lag(df7$SALDO, 1)-df7$SALDO,2)



df8 <- extract(df7,
               col = "DETALLE" ,
               into = c('COMERCIO') ,
               regex = "\\d+ +(Pago comercios.*)" ,
               remove = FALSE
)

rm(df4,df6,df7)

df8 <- df8 %>% 
         mutate( DETALLE = if_else( is.na(COMERCIO), DETALLE, COMERCIO)) %>% 
         select(-COMERCIO)

df9 <- extract(df8,
               col = "DETALLE" ,
               into = c('num','cheque') ,
               regex = "^(\\d+) +(\\w.*)" ,
               remove = FALSE
)

df9 <- df9 %>% 
  mutate( DETALLE = if_else( (is.na(cheque) & is.na(num)), DETALLE, cheque)) 

df9 <- df9 %>% 
  mutate( VARIOS = if_else( is.na(num) , VARIOS, as.numeric( paste0(as.character(VARIOS),as.character(num))) )) %>%
  select(-c(num,cheque))

MAXIMO_VARIOS=max(df9 %>% filter(str_detect(DETALLE,'Pago comercios')) %>% select(VARIOS))

df10 <- df9 %>% 
  mutate( VARIOS = if_else( !str_detect(DETALLE,'Pago comercios') , VARIOS, MAXIMO_VARIOS )) 

rm(df8,df9)

#------

write_xlsx(df10 , 'output/texto_nvo_1.xlsx')



