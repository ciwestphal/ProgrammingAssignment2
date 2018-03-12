## The following two functions are created to cache the inverse of a matrix, in order to not have to compute the inverse repeatedly.

## "MakeCacheMatrix" creates a unique matrix object that can cache its inverse.  It essentially is a list containing a function to: set the value of the matrix, get the value of the matrix, set the value of the matrix inverse, and get the value of the matrix inverse.  
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## "CacheSolve" solves for the inverse of the unique matrix object created with "MakeCacheMatrix".  It first checks to see if the matrix inverse has already been solved for, and if so, it obtains the matrix inverse from the cache and skips the computation.  If not, however, it calculates the matrix inverse on its own and sets the value of the matrix inverse in the cache with the "setinverse" function.  
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

originalMatrix<-matrix(c(1:4),2,2)
cachedMatrix <- makeCacheMatrix(originalMatrix)
cacheSolve(cachedMatrix)
##  Solution:   
##    [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
