
## makeCacheMatrix function creates a special "matrix" which is
## a list of functions which:
##   1. set a value of the matrix
##   2. get a value of the matrix
##   3. set inverse of the matrix
##   4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  setMatrix<-function(y){
    x<<-y
    s<<-NULL
  }
  getMatrix<-function() x
  setinverse<-function(inverse) s<<-inverse
  getinverse<-function() s
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function finds the inverse of the special "matrix"
## created in the first function, but first checks if the inverse
## has already been calculated. If yes it just shows the cached
## inverse. If not, it finds the inverse and sets it in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$getMatrix()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}