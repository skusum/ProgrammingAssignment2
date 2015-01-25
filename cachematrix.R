##In this example we introduce the <<- operator which 
##can be used to assign a value to an object in an environment that is different
##from the current environment. Below are two functions that are used to create a 
##special "matrix"(square invertible) object that stores a matrix and cache's its inverse. 

## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m  <<- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve)  s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This second function computes the inverse of the special "matrix" 
##(assuming the given matrix is invertible)
##returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <-solve(data, ...)
  x$setinverse(s)
  s   ## Return a matrix that is the inverse of 'x'
}        


