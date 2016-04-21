# Este script tranforma una matriz de genotipos con el siguiente formato:
# ID Multiplex/Panel Marker1.Size1 Marker1.Size2 Marker2.Size1 Marker2.Size2 ... Marker.n
# a formato MsatAllele
# SampleName Panel Marker Size1 Size2
df2msatallele<-function(placa.multiplex){
  # La entrada de la función es la dirección del archivo *.csv que tiene los genotipos
  read.csv(placa.multiplex,header=T)->placa.p
  ######## Reordenar los marcadores para que queden hacia abajo
  (length(colnames(placa.p))-2)->n.primers 
  # cantidad de primers en la placa
  
  length(rownames(placa.p))->n.indi 
  #Extrae la cantidad de individuos
  
  GMdata.temp<-array(data = c(NA),dim = c(n.indi,5),
                     dimnames = c("Sample Name","Panel","Marker","Size1","Size2"))
  # Este es un archivo temporal donde se van a guardar los alelos de cada marcador
  # verticalmente
  as.data.frame(GMdata.temp)->GMdata.temp
  # Para que se puedan meter datos debe tranformarse en un marco de datos
  
  GMdata<-NULL
  # Este es el archivo final donde se van a guardar todos los primers del
  # multiplex i
  ######################################################################################
  seq(from=3,to=(n.primers+2),by = 2)->order # Con este orden no va a leer las 2 primeras columnas
  for(r in order)
  {
    placa.p[,r]->GMdata.temp[,4]
    placa.p[,r+1]->GMdata.temp[,5]
    # Mete los alelos en las columnas correspondientes
    
    rep(colnames(placa.p)[r],n.indi)->GMdata.temp[,3]
    # Repite el nombre del marcador en la columna 3
    
    as.vector(placa.p[,2])->GMdata.temp[,2]
    as.vector(placa.p[,1])->GMdata.temp[,1]
    # Guarda los ID y el nombre del multiplex en el lugar que pide MSatAllele
    
    GMdata<-rbind(GMdata,GMdata.temp)
    # Con este comando uno hacia abajo (en vertical) cada marcador
    
  }
  rm(r)
  as.data.frame(GMdata)->GMdata
  colnames(GMdata)<-c("SampleName","Panel","Marker","Size1","Size2")
  GMdata
}
