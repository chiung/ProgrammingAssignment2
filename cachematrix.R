## The following are functions to creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

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


## cacheSolve : This function calculates the inverse of a matrix, 
## or if the inverse has been cached, retrieves the cached results. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data #this calls the function solve to generate the inverse of the matrix
  x$setinverse(m)
  m
}

## To verify, do the following:
## 1. Create the two functions above
## 2.  m <- matrix(rnorm(25), 5)    # Create a 5x5 matrix of random numbers
## 3. cache <- makeCacheMatrix(m)   # Create the cache list
## 4. cache$get()                   # Returns the original matrix
## 5. cacheSolve(cache)             # Computes the matrix inverse
## 6. cacheSolve(cache)             # Again, but from cache.  Will see output
#                                   "getting cached data"

## To check on performance, run the following commands
## x <- 2000
## m <- matrix(rnorm(x^2), x)
## c <- makeCacheMatrix(m)
## t <- system.time(cacheSolve(c))
## t <- system.time(cacheSolve(c))


