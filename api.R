#*@apiName Nacimientos en diferentes anos
#*@apiTitle Nacimientos en Euskadi
#*@apiDescription Ploteamos los nacimientos 
#*y las localidades 


#* Localidad mas nacimientos casa
#* @param mes
#* @param year
#* @get /localidad

f1<- function(mes="", year="") {
  library(dplyr)
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
  
  df_filt<- datos %>%
    group_by(CODMUN_INS) %>%
    filter(PAE_LNAC == 1 &  PAE_ANON==year & PAE_MESN==mes) %>%
    count(CODMUN_INS) %>%
    arrange(desc(n)) %>%
    head(1)
  
  print(paste('La localidad con mas nacimientos en casa del mes', mes ,'del ano', year, 'es', df_filt$CODMUN_INS))
  
}

#* Nacimientos por year
#* @param yeari
#* @param yearf
#* @serializer png
#* @get /grafico

f2<-function(yeari="", yearf ="") {
  library(ggplot2)
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
  
  grafi<-ggplot(datos, aes(x=PAE_ANON))+
    geom_bar()+
    scale_x_continuous(limits=c(as.numeric(yeari)-1,as.numeric(yearf)+1),breaks =seq(as.numeric(yeari),as.numeric(yearf),1))+labs(x='year',y='cantidad')
  
  print(grafi)
}