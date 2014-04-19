## An inverse of an invertible matrix is solved through the
## implementation of two functions makeCacheMatrix and cacheSolve in
## the following. No attempt is made to check if the matrices are
## invertible. 


## makeCacheMatrix function creates a special "vector". which is
## really a list containing a function to
## set the value of a matrix
## get the value of a matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function:
## first, checks if the inverse already exists for a given matrix
## if it exists then it does not calculate the inverse
## rather it gets the data from cache and prints the value
## if the inverse does not exist then inverse is calculated and printed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
