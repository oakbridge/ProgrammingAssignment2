## Put comments here that give an overall description of what your
## functions do

# This function creates a list of functions
# that allow us to get, set and store a matrix
# and its inverse
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  if (!is.matrix(x)) {
    x <- NULL
  }
  set <- function(newMatrix){
    if(!is.matrix(newMatrix)) {
      x <<- NULL
    } else {
      x <<- newMatrix 
    }
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix){
    if(!is.matrix(inverseMatrix)) {
      cachedInverse <<- NULL
    } else {
      cachedInverse <<- inverseMatrix 
    }
  }
  getInverse <- function(){
    return(cachedInverse)
  }
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



# The cacheSolve gets as parameter x which is a 
# result of the makeCacheMatrix
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("Returning cached value")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- tryCatch ({
    inverseMatrix <- solve(data, ...)
  }, error = function(e) {
    NULL
  }, warning = function(e) {
    NULL
  })
  x$setInverse(inverseMatrix)
  inverseMatrix
}
