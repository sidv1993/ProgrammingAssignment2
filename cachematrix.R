## To increase efficiency in Matrix computation, one can write functions for caching
## the inverse of a matrix instead of computing it repeatedly. The following two functions
## illustrate how this can be achieved.

## makeCacheMatrix() is a function that creates a list containing elements
## which are based on the following functions to exxecute different operations.
## 1. set() is a fucntion to set the value of the matrix
## 2. get() is a function to get the value of the matrix
## 3. setinv() is a function to set the value of inverse of the matrix
## 4. getinv() is a function to get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse)  inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cachesolve() function below, returns the inverse of the input matrix. 
## It first checks if the inverse has already been computed. 
## If so, it gets the cached result and skips the computation. 
## If not, it computes the result and stores it in the cache by the setinv() function.
## This function works under the assumption that the input matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv 
}

## Sample Output:

## > x <- matrix(2:5, 2, 2)

## > x
##       [,1] [,2]
## [1,]    2    4
## [2,]    3    5

## > y <- makeCacheMatrix(x)

## > y$get()
##       [,1] [,2]
## [1,]    2    4
## [2,]    3    5

## > cacheSolve(y)
##       [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1

## In the above case, there is no cached data so inverse is computed and stored.

## > cacheSolve(y)
## getting cached matrix data
##       [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1

## The second time, the inverse is not computed and the result is directly taken from the cached value.