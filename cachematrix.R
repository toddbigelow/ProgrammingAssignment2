## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix <- function(x = matrix()) {

#}


## Write a short comment describing this function

#cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#}


makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  print(environment())
  evn <- environment()
  print(parent.env(evn))
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve(x)
  getinverse <- function() m
  getevn<- function() environment()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getevn = getevn)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
