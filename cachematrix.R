## Modified by Todd Bigelow using rdpeng's initial stem as a starting point. 
## This set of functions uses environment (more specifically, parent envorinments)
## to "cache" the inverse of a matrix.  It is assumed that the input matrix is 
## always invertible.  

## makeCacheMatrix creates a list of functions with associated variables that
## get saved in the environment.  These functions can be used to set or retrieve
## the inverse of the matrix passed into the makeCacheMatrix function.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  print(environment())
  evn <- environment()
  print(parent.env(evn))
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x    #return input matrix x
  setinverse <- function(inverse) m <<- solve(x)  #set inverse of input matrix x
  getinverse <- function() m     #find inverse of input matrix x
  getevn<- function() environment()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getevn = getevn)
}

## cacheSolve checks to see if the inverse of the matrix is stored in the parent
## environment.  If it is, it returns the inverted matrix.  If not, it inverts 
## and returns it.  

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {           #cache is available
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
