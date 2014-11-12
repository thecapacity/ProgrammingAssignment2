## makeCacheMatrix: This function creates a special 'matrix' object 
##                 that can cache its inverse.
## cacheSolve: This function computes the inverse of the special 'matrix'
##                 returned by makeCacheMatrix above. If the inverse has already been 
##                 calculated (and the matrix has not changed), then the cachesolve 
##                 should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## Function builds the psuedo-matrix object, which is really a list of functions to:
##  1. set the matrix value
##  2. get the matrix value
##  3. set the matrix inverse value
##  4. get the matrix inverse value
makeCacheMatrix <- function(m = matrix()) {
    m_inverse <- NULL
    set <- function(new_m) {
        m <<- new_m
        m_inverse <<- NULL
    }
    get <- function() m
    setinv <- function(m_inv) m_inverse <<- m_inv
    getinv <- function() m_inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Calculates the inverse of the special "matrix" created with makeCacheMatrix()
## Tt first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the value from the cache and skips the computation. 
## Otherwise, it calculates the data and sets the value in the cache 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' either via cache or computation
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    c_inv <- solve(data, ...)
    x$setinv(c_inv)
    c_inv
}

## Example Usage:
##  c <- rbind(c(1, -1/4), c(-1/4, 1))
##  m1 <- makeCacheMatrix(c)
##  cacheSolve(m1)
##      [,1]      [,2]
##      [1,] 1.0666667 0.2666667
##      [2,] 0.2666667 1.0666667
##  cacheSolve(m1)
##      getting cached data
##      [,1]      [,2]
##      [1,] 1.0666667 0.2666667
##      [2,] 0.2666667 1.0666667