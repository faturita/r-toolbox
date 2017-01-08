#
# Apéndice A
#
# Aprendizaje Automático
# TP 1 - Juego Tateti
#
# Implementación en R
# 
# Cargar los fuentes con: source('tateti.r')
#
#
# Jugar con "playtateti()"
# 
# (Reemplazar stdin por el correspondiente archivo de entrada estandard en la plataforma)

input<-"random"
typeofgame<-"random"
win<-0

#
# Selecciona en base al juego actual 'tateti' y la matriz de pesos 'W'
# la mejor estrategia (la celda en la grilla de tateti) a jugar.
playcomputer<-function(tateti, W)
{
  maxi<-0
  maxj<-0
  # Valor mínimo 
  Vbmax<-(-9)
  for (j in 1:3)
    for (i in 1:3)
    {
      if (tateti[i,j]==0)
      {
        tatetiv<-tateti
        tatetiv[i,j]=1
        
        id<-c(1,1,1)
        trasposemirror<-rbind(c(0,0,1),c(0,1,0),c(1,0,0))
        colvector<-tatetiv %*% id
        rowvector<-t(tatetiv) %*% id
        
        
        X<-c(0,sum(colvector),sum(rowvector), sum(diag(tatetiv)),sum(diag((tatetiv %*% trasposemirror))) )
        
        # estimator.
        Vb<-sum(colvector) + sum(rowvector) + sum(diag(tatetiv)) + sum(diag((tatetiv %*% trasposemirror)))
        Vb<-W %*% X
        
        if (Vb >= Vbmax)
        {
          Vbmax<-Vb
          maxi<-i
          maxj<-j
        }
        
      }
    }
  
  return (c(maxi,maxj))
}


# Juega al azar o espera entrada del usuario.
play<-function(input) {
  if (input == "random")
    return (  c(sample(1:3,size=1,replace=TRUE),sample(1:3,size=1,replace=TRUE)  ) )
  else
    return ( scan(con,n=2) )
}


# Determina cuando es el final del juego y retorna el valor de Vtran.
#
endofgame<-function( tateti ) {
  id<-c(1,1,1)
  trasposemirror<-rbind(c(0,0,1),c(0,1,0),c(1,0,0))
  colvector<-tateti %*% id
  rowvector<-t(tateti) %*% id
  
  # estimator.
  Vb<-sum(colvector) + sum(rowvector) + sum(diag(tateti)) + sum(diag((tateti %*% trasposemirror)))
  
  print("Vb Value")
  print(Vb)
  
  if ( colvector[1]==3 || colvector[2]==3 || colvector[3]==3 || 
    rowvector[1]==3 || rowvector[2]==3 || rowvector[3]==3 ||
    sum(diag(tateti))==3 || sum(diag((tateti %*% trasposemirror))) == 3) 
  {
    print ("Positive (Circle) Wins !")
    win<<-1
    print(tateti)
    return(+3)
  }
  
  if ( colvector[1]==-3 || colvector[2]==-3 || colvector[3]==-3 || 
    rowvector[1]==-3 || rowvector[2]==-3 || rowvector[3]==-3 ||
    sum(diag(tateti))==-3 || sum(diag((tateti %*% trasposemirror)))==-3) 
  {
    print ("Negative (Cross) Wins !")
    win<<--1
    print(tateti)
    return(-3)
  }
  
  if ( sum (abs(tateti))==9  )
  {
    print ("Draw !! Nobody wins, neither loss...Draw")
    win<<-0
    print(tateti)
    #cat("",file="game.out",append=TRUE, sep="\n")
    return (0)
  }
  
  return(1)
}

# Actualiza los pesos sinápticos del array W
# 
updateMatrix<-function( W, tateti, Vtrain) {
  id<-c(1,1,1)
  trasposemirror<-rbind(c(0,0,1),c(0,1,0),c(1,0,0))
  colvector<-tateti %*% id
  rowvector<-t(tateti) %*% id
  X<-c(0,sum(colvector),sum(rowvector), sum(diag(tateti)),sum(diag((tateti %*% trasposemirror))) )
  # estimator.
  Vb<-sum(colvector) + sum(rowvector) + sum(diag(tateti)) + sum(diag((tateti %*% trasposemirror)))
  
  
  Vb<-W %*% X
  W<-(W + (0.0001)*(Vtrain - Vb) * X)
  W<-(1/sum(W)) * W
  
  cat('Vtrain:',Vtrain,'{',Vb,'}\n' )
  
  return(W)
}


#
# Juega tateti Recursivamente.
#
# who     1/-1  Quien juega
# input   'random' '/dev/stdin'
# tateti  Matriz con el juego.
# obj     Objeto con el valor de la matriz de pesos W y el valor de Vtrain
#
learningtatetifull<-function(who,input,tateti, obj)
{
  W<-obj$W
  
  print( tateti )
  Vtrain<-endofgame(tateti)
  
  # Si Vtrain = 1 no se alcanzó un estado final y hay que seguir recursando.
  if (Vtrain == 1)
  {
    cat(" (",who,") Play (fila,columna):\n")
    
    # Reintenta hasta que se selecciona una celda vacía
    while (TRUE)
    {
      if (typeofgame == 'match' && who==1)
        movement<-playcomputer(tateti, W)
      else
        movement<-play(input)
      cat('Movimiento:');print(movement)
      if ( tateti[movement[1],movement[2]] == 0 ) 
      {
        break
      } else {
        cat("That location is already used !\n")
      }
    }
    tateti[movement[1],movement[2]]=who
    #cat(movement, file="game.out", append=TRUE, sep="\n")
    obj$Vtrain<-Vtrain
    
    obj<-learningtatetifull(who*(-1),input,tateti,obj)
    
    Vtrain<-obj$Vtrain
    

  }
  
  W<-obj$W
    
  # Actualizar la matriz de pesos sinápticos con lo aprendido.
  cat ( 'Update:' ) 
  print ( W )
  if (who == 1) W<-updateMatrix( W, tateti, Vtrain)
  cat ( 'Update after:' )
  print  ( W )
  
  obj$W<-W
  obj$Vtrain<-Vtrain
  
  return( obj )
  
}


#
# LearningTateti
#
# Jugando se aprende !  Cada juego, se aprende lo nuevo.
# RTateti siempre juega como (+1)
#
# input               'random' ó '/dev/stdin' 
# maxrepetitions      Cantidad de repeticiones.
# tries               Cantidad de intentos en cada repeticion
# typeofmatch         'random' ó 'match', 'match' corresponde al juego de la computadora
# showGraph           Muestra un plot con los juegos ganados/empatados por repeticion.
#
#
learningtateti<-function(input,maxrepetitions,tries,typeofmatch,showGraph) 
{
  typeofgame<<-typeofmatch
  if (input != "random")
    con<<-file(input,"r")
  
  if (!file.exists("matrix"))
  {
    print("New weight matrix")
    W<-c(0,0,0,0,0)
  }
  else
  {
    # Leer la matriz de la "memoria"
    W<-scan(n=5, file="matrix")
  }

  print( W )

  # Contador de resultados....
  dataset<-list()
  
  obj<-list()
  
  for (repetition in 1:maxrepetitions)
  {
    dataset[[repetition]]<-0
    for ( games in 1:tries)
    {
        obj$W<-W
        obj$Vtran<-0
        
        obj<-learningtatetifull ( +1,input ,rbind(c(0,0,0),c(0,0,0),c(0,0,0)) ,obj )
        
        # Actualizar la cuenta de exitos o empates
        if ( win == 1 || win == 0)
        {
          dataset[[repetition]]<-dataset[[repetition]]+1
        }
        
        W<-obj$W
    }
  }
  
  print ( W )
  
  # Solo grabar la matriz cuando se haya jugado una partida.
  if (typeofgame=='match')
    cat (W, file="matrix")
  
  if (showGraph == T)
    plot(1:maxrepetitions,dataset,type='l', xlab='Experiencias',ylab='Partidas ganadas o empatadas',main='Performance del R-Tateti')
  
  
}

playtateti<-function() {
  print("Escriba, 1 (ENTER) 2 (ENTER) 3 (ENTER) y Juegue !")
  learningtateti('/dev/stdin',1,1,'match',F)
}



playtateti()

