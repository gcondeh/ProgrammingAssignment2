# getwd()
# rm(list=ls())

## Esta función crear una serie de funciones que almacenan los objetos en el entorno
## En realidad es un constructor de funciones que se invocarán desde la resolución de la cache

makeCacheMatrix <- function(x = matrix()) {
  #Incializamos el valor
  inversa <- NULL
  
  # Asingamos valor a la matriz, dejando en blanco la inversa
  set <- function(matriz) {
    x <<- matriz
    inversa <<- null
  }
  
  # Funciones para dar valores y recuperar valores de la inversa
  set_inversa <- function (inv)  inversa <<- inv
  get_matriz <- function() x
  get_inversa <- function() inversa

  # Devolvemos una lista de funciones
  list(set = set, set_inversa=set_inversa, get_matriz=get_matriz, get_inversa=get_inversa)
}

## Resuelve la inversa. Si está en Cache la captura, si no está en cache la genera

cacheSolve <- function(x, ...) {
  inv <- x$get_inversa()
  if (is.null(inv))
      {inv <- solve(x$get_matriz())
      message("Cargamos los datos en cache")
       x$set_inversa(inv)   }
    else
      {message("Datos en cache")
        }
  inv
}




## Datos de prueba

# Creamos la matriz
A <- matrix(c(1,4,9,0,-3,2,2,7,8),3,3)
B <- matrix(c(22,12,19,71,5,-4,23,26,0),3)
x<-1000
c <- matrix (sample(x^2),x)

# Creamos la cache de la matriz
M1 <- makeCacheMatrix(A)
M2 <- makeCacheMatrix(B)
M3 <- makeCacheMatrix(c)

M1$get_matriz()  ## Nos da la matriz
M1$get_inversa() ## Hasta que no generamos la caché debería ser nulo

cacheSolve(M1) -> I
cacheSolve(M2) -> I

# Prueba de la traspuesta
B %*% I

# Para comprobar la mejora de rendimiento, en la creación de la matriz vs la recuperación desde la cache
system.time(cacheSolve(M3))
