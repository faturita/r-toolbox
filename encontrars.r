#
# Apéndice B
#
# Aprendizaje Automático
# TP 1 - ENCONTRAR-S
#
# Implementación en R+
# 
# Cargar los fuentes con: source('encontrars.r')
#
#
# Verificar con  "encontrars()" la reproducción de la traza.
# Verificar con  "promediocasosencontrars()" el promedio de casos requeridos hasta 
#   encontrar el concepto.
# Verificar con  "evaluateencontrars()" la cantidad de conceptos aprendidos correctamente
#   según la cantidad de ejemplos del conjunto D
# 

## Función de ayuda para mostrar vectores
show<- function( K, h ){
  
  repr<-c('')
  for(i in 1:6)
  {
    repr<-paste ( repr,K[[i]][h[i]+1] )
    if ( i != 6)
      repr<-paste ( repr, ',')
  }
  
  if (length( h )==7)
  {
    if ( h[7]==1)
      repr<-paste ( repr, ':','+')
    else
      repr<-paste ( repr, ':','-')
  }
  
  cat( repr )
  cat( '\n')
}

## Función de ayuda para calcular la longitud de una lista.
getlength<-function( D ) {
  sums<-0
  for(kd in D)
  {
    if (!is.null(kd))
    {
      sums<-(sums+1)
    }
  }
  return(sums)
}


#
# EncontrarSFull
#
# 
encontrarsfull<- function( D, objects ) {
  
  # Dominios para los atributos.
  k1<-c('?','soleado','nublado','calido')
  k2<-c('?','calida','fria')
  k3<-c('?','normal','alta')
  k4<-c('?','fuerte','debil')
  k5<-c('?','calida','fria')
  k6<-c('?','igual','cambiante')
  
  K<-list(k1,k2,k3,k4,k5,k6)
  
  domains<-objects$domains
  h<-objects$h

  # D contiene la lista de ejemplos.
  for(ci in D)
  {
    cat( 'd:')
    cat( ci )
    cat( '\n')
    show( K,ci )
    
    # Verificar sobre todos los elementos
    for(i in 1:6)
    {
      if ( h[i] != ci[i] && ci[7] == 1 )
      {
        if (h[i] == -1)
        {
          # La primera especificación, el elemento encontrado tal cual.
          h[i] = ci[i]
          
          # Registramos que este elemento del dominio apareció
          domains[[i]][[ci[i]]]=1;

        }
        else
        {

          # El caso es positivo, y aparecio un segundo,tercero,etc elemento del dominio
          domains[[i]][[ci[i]]]=1;

          # Si todos los elementos del dominio ya aparecieron, generalizar.
          if ( getlength(domains[[i]])+1 == length( K[[i]] )  )
          {
            # Todos los elementos encontrados, colocar ANY (?)
            h[i] = 0;
            
          }

        }
      }
    }
    
    # Mostrar el H parcial
    cat( "h:")
    cat( h )
    show( K,h)
  }
  
   
  
  objects$h<-h
  objects$domains<-domains
  
  return (objects)
  
}


#
# Encontrars:
# 
# Verificación de la traza de aprendizaje.
#
encontrars<-function() {
  # Ejemplos.
  c1<-c(1,1,1,1,1,1,1)
  c2<-c(1,1,2,1,1,1,1)
  c3<-c(2,1,2,1,1,2,0)
  c4<-c(1,1,2,1,2,2,1)
  
  D<-list(c1,c2,c3,c4)
  
  # Arranca con todos los elementos en vacío.  De entrada no se cumple para ninguno.
  h<-c(-1,-1,-1,-1,-1,-1)
  domains<-list(list(),list(),list(),list(),list(),list())
  objects<-list()
  objects$h<-h;
  objects$domains<-domains;
  
  
  objects<-encontrarsfull( D,objects )
  
}


#
# evaluateencontrars:
# 
# Evaluación de la cantidad de casos donde la función objetivo es aprendida
# correctamente según cantidad de muestras.
#
evaluateencontrars<-function() {
  
  dataset<-list()
  
  for(case in 1:100)
  {
    dataset[[case]]<-0
    for(repetition in 1:100)
    {
      # Arranca con todos los elementos en vacío.  De entrada no se cumple para ninguno.
      h<-c(-1,-1,-1,-1,-1,-1)
      domains<-list(list(),list(),list(),list(),list(),list())
      objects<-list()
      objects$h<-h;
      objects$domains<-domains;
      
      for (s in 1:case)
      {
        c1<-c(sample(1:3,size=1,replace=TRUE), sample(1:2,size=1,replace=TRUE), sample(1:2,size=1,replace=TRUE), sample(1:2,size=1,replace=TRUE), sample(1:2,size=1,replace=TRUE), sample(1:2,size=1,replace=TRUE))
        
        if ( c1[1] == 1 && c1[2] == 1)
        {
          # Func
          c2<-c(c1,1)
        }
        else
        {
          c2<-c(c1,0)
        }
        
        D<-list(c2)
        
        objects<-encontrarsfull( D, objects )
        
      }
      
      # Done.  Se ejecuta el algoritmo ENCONTRAR-S con 'case' ejemplos.
      if ( objects$h[1] == 1 && 
        objects$h[2] == 1 && 
        objects$h[3] == 0 &&
        objects$h[4] == 0 &&
        objects$h[5] == 0 &&
        objects$h[6] == 0)
      {
        # Func
        dataset[[case]]<-dataset[[case]]+1
      }
    }
  }
  
  plot(1:100,dataset,type='l', xlab='# Cardinal de D (tamaño de la muestra)',ylab='Cantidad de casos dónde se aprendió la F objetivo',main='¿ Cuántos ejemplos son requeridos para aprender la función objetivo (soleado , calida , ? , ? , ? , ?) ?')
  
}

#
# promediocasosencontrars:
# 
# Promedio de muestras requeridas hasta aprender la función objetivo.
#
promediocasosencontrars<-function() {
  
  dataset<-list()
  
  for(case in 1:20)
  {
    dataset[[case]]<-0

    # Arranca con todos los elementos en vacío.  De entrada no se cumple para ninguno.
    h<-c(-1,-1,-1,-1,-1,-1)
    domains<-list(list(),list(),list(),list(),list(),list())
    objects<-list()
    objects$h<-h;
    objects$domains<-domains;
    
    for (s in 1:10000)
    {
      c1<-c(sample(1:3,size=1,replace=TRUE), sample(1:2,size=1,replace=TRUE), sample(1:2,size=1,replace=TRUE), sample(1:2,size=1,replace=TRUE), sample(1:2,size=1,replace=TRUE), sample(1:2,size=1,replace=TRUE))
      
      if ( c1[1] == 1 && c1[2] == 1)
      {
        # Func
        c2<-c(c1,1)
      }
      else
      {
        c2<-c(c1,0)
      }
      
      D<-list(c2)
      
      objects<-encontrarsfull( D, objects )
      
      # Done.  Se ejecuta el algoritmo ENCONTRAR-S con 'case' ejemplos.
      if ( objects$h[1] == 1 && 
        objects$h[2] == 1 && 
        objects$h[3] == 0 &&
        objects$h[4] == 0 &&
        objects$h[5] == 0 &&
        objects$h[6] == 0)
      {
        # Func
        dataset[[case]]<-s
        break;
      }
        

    }
  }
  
  str<-paste( 'Cantidad de ejemplos promedio hasta aprender la función objetivo (soleado , calida , ? , ? , ? , ?) = ',mean(unlist(dataset)))
  
  plot(1:20,dataset,type='h', xlab='# Repeticiones',ylab='Ejemplos utilizados hasta aprender la F objetivo',main=str)
  
}

