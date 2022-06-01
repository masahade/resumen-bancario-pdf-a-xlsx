library(pdftools)
library(stringr)
library(tidyverse)
library(dplyr)
library(lubridate)
library("writexl")
library(purrr)

file='./data/Resumen de Cuenta 14-01-2022.pdf'

txt <- pdf_text(file)


destino="./output/resumenes_1.txt"


write(txt,destino,append=TRUE)

texto_completo=readLines(destino)


# quita las lineas por encima y por debajo de los limites de movimientos

pattern='.*Fecha.*Comprobante.*Movimiento'



texto_EXTRACT_0=str_which(texto_completo,pattern)


minimo=min(texto_EXTRACT_0)+1

pattern='.Saldo total'

texto_EXTRACT_0=str_which(texto_completo,pattern)
maximo=max(texto_EXTRACT_0)-1


texto_EXTRACT_1=texto_completo[minimo:maximo]



# quita las lineas vacias
pattern='^[:space:]{60,}.*'

FILTRO=str_detect(texto_EXTRACT_1,pattern)

texto_EXTRACT_2=texto_EXTRACT_1[!FILTRO]
#------
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

archivo=read_fwf('output/resumen_sdf1.txt',
                 fwf_widths(c(8,15,45,100),c("FECHA","VARIOS","DETALLE","IMPORTES"))) 

archivo=archivo %>% mutate(orden=seq(1,nrow(archivo)))

df4 = archivo %>% mutate( listado = sapply(IMPORTES,str_extract_all, pattern='-?\\$ ?([0-9]+\\.?)?([0-9]+\\.?)?[0-9]+,[0-9]{2}' ))
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






df4 <- df4 %>% mutate( list3 = sapply(listado,a_num))

df4 <- df4 %>%
  mutate(score = map_chr(list3, toString))


localizar <- function (arg1) {
  
  pattern='([0-9]+\\.?)?([0-9]+\\.?)?[0-9]+,[0-9]{2}-?'
  b=str_locate_all(arg1,pattern)
  c=b[[1]][2]-b[[1]][3]
  
  return(c)
}

df4 <- df4 %>% mutate(posicion= sapply(IMPORTES,localizar))

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

df6 <- df4 %>% mutate( prueba = sapply(score,ejem2))


#------------

ejem3 <- function (arg1) {
  #arg1=df6$prueba[2]
  
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


#------

write_xlsx(df7 %>% select(orden,FECHA ,VARIOS,DETALLE,CAJA_,CTA_CTE,SALDO), 'output/texto_nvo_1.xlsx')
