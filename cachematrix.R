## the function created below will cache the inverse of a matrix

## the first function will create a list of
## functions containing set, setinverse, get, getinverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## the second function will return the cached inverse
## matrix if it exists and if it doesn't, it will calculate on the spot the inverse matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

## to confirm the functions work properly
## n <- matrix(1:4,2,2)
## V1  V2
## 1    3
## 2    4
## test <- makeCacheMatrix(n)
## test$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## test$getinverse()
## NULL
## cacheSolve(test)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(test)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## test$getinverse()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


