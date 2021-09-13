## Caching the Inverse of a Matrix



makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ## define the get fucntion - returns value of the matrix argument
  
  setInverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getInverse <- function() inv     ## gets the value of inv where called
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## The solution :

## source("cachematrix.R")
## my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## now we get the matrix by my_matrix$get()
## matrix is shown

##then

## my_matrix$getInverse()  gives NULL
##cacheSolve(my_matrix)   inverse the matrix and shows the result
