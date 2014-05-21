## Calculates the inverse of a matrix and caches it

## Done as part of Coursera "R-Programming" (assignment #2)
## 
## Usage:
## # create a regular matrix, for example
## m1 <- rbind(c(1, -1/4), c(-1/4, 1))  
## # create a special matrix from the regular matrix
## m2 <- makeCacheMatrix(m1)
## # calculate the inverse
## cacheSolve(m2)


## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(y) {
    x <<- y
    cm <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cm <<- inverse
  getinverse <- function() cm
  # return the special matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## calculate the inverse of a special matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    # getting cached value
    return(m)
  }
  # not in cache... yet...
  data <- x$get()
  # `solve` is the R function that calculates the inverse of a matrix
  m <- solve(data, ...)
  # store results in cache
  x$setinverse(m)
  # and return it
  m
}
