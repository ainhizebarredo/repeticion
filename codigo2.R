#2.	Automatiza la carga anterior y genera una función en la que utilizando como parámetros el año y el mes te devuelva lo siguiente: “La localidad con más nacimientos en casa del mes MM del año YYYY es XXXX” (2)
rm(list = ls())
datos96<-read.csv('fichero_final_nac_1996.csv', sep=';')
datos97<-read.csv('fichero_final_nac_1997.csv', sep=';')
datos98<-read.csv('fichero_final_nac_1998.csv', sep=';')
datos99<-read.csv('fichero_final_nac_1999.csv', sep=';')
datos00<-read.csv('fichero_final_nac_2000.csv', sep=';')
datos01<-read.csv('fichero_final_nac_2001.csv', sep=';')
datos02<-read.csv('fichero_final_nac_2002.csv', sep=';')
datos03<-read.csv('fichero_final_nac_2003.csv', sep=';')
datos04<-read.csv('fichero_final_nac_2004.csv', sep=';')
datos05<-read.csv('fichero_final_nac_2005.csv', sep=';')
datos15<-read.csv('fichero_final_nac_2015.csv', sep=';')
datos14<-read.csv('fichero_final_nac_2014.csv', sep=';')
datos13<-read.csv('fichero_final_nac_2013.csv', sep=';')
datos12<-read.csv('fichero_final_nac_2012.csv', sep=';')
datos11<-read.csv('fichero_final_nac_2011.csv', sep=';')
datos10<-read.csv('fichero_final_nac_2010.csv', sep=';')
datos09<-read.csv('fichero_final_nac_2009.csv', sep=';')
datos07<-read.csv('fichero_final_nac_2007.csv', sep=';')
datos08<-read.csv('fichero_final_nac_2008.csv', sep=';')
datos06<-read.csv('fichero_final_nac_2006.csv', sep=';')
datos<-rbind(datos00,datos01,datos02,datos03,datos04,datos05,datos06,datos07,datos08,datos09,datos10,datos11,datos12,datos13,datos14,datos15,datos96,datos97,datos98,datos99)

funcion<-function(ano, mes){
  ej2<-datos%>%
    group_by(CODMUN_INS)%>%
    filter(PAE_MESN==mes & PAE_ANON==ano & PAE_LNAC==1)%>%
    count(CODMUN_INS)%>%
    arrange(desc(n))%>%
    head(1)
  print(paste('La localidad con más nacimientos en casa del mes',mes,'del año', ano,'es', ej2$CODMUN_INS))
}
funcion(2000,2)
