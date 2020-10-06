## Put comments here that give an overall description of what your
## functions do

## Establece la matriz y la inversa en un entorno
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inversa <- NULL       ##crea la matriz inversa como nula
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x   ## funcion para obtener la matriz x
  setinverse <- function(inverse) inversa <<- inverse
  getinverse <- function() inversa 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversa <- x$getinverse()
  if(!is.null(inversa)) {
    message("getting cached matrix inverse")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinverse(inversa)
  inversa
}

