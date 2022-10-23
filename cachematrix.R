## Make 'special' matrix which has our own defined functionalities,
## then it can be inversed

## 'special' matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  get <- function() x
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getInverse <- function() inv
  
  setInverse <- function(inverse) inv <<- inverse
  
  list(
    get = get, set = set,
    getInverse = getInverse, setInverse = setInverse
  )
}


## If the 'special' matrix input has its inverse, just return it;
## no need to calculate it again, otherwise, compute its inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message('Getting cached data')
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}
