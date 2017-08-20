## the function created below will cache the inverse of a matrix

## the first function will create a list of
## functions containing set, setinverse, get, getinverse.

makeCacheMatrix <- function(x = matrix()) {
  #create functions and set variables
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse 
  getinverse <- function() m #set the getinverse function, if no cache exists, return null
  list(set = set, get = get, #create the list of 4 functions created
       setinverse = setinverse,
       getinverse = getinverse)
}

## the second function will return the cached inverse
## matrix if it exists and if it doesn't, it will calculate on the spot the inverse matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #set up m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } #if cache exists, the message will appear and return inverse of a matrix
  data <- x$get()
  m <- solve(data) #if not, solve will compute the inverse of a matrix instead
  x$setinverse(m)
  m #returning the result
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


