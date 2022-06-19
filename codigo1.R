#1.	Carga los ficheros de datos en R y encuentra la localidad con más nacimientos en casa del año 2015 (1)

datos<-read.csv('fichero_final_nac_2015.csv',  sep=';')

library(dplyr)

localidad<-datos%>%
  group_by(CODMUN_INS)%>%
  filter(PAE_LNAC==1)%>%
  count(CODMUN_INS)%>%
  arrange(desc(n))%>%
  head(1)
localidad


