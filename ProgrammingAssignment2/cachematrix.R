######################################
## Caching the Inverse of a Matrix ##
#####################################

## The following functions create a matrix that cache the inverse and 
## compute the inverse of a square matrix with the "solve" function,
## assuming that the matrix supplied is always invertible.

## A. makeCacheMatrix: This function creates a special "matrix" object that 
##    can cache its inverse, which is really a list containing a function to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m.inverse <- NULL
        set <- function(y) {
                x <<- y
                m.inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m.inverse <<- inv
        getinv <- function() m.inverse
        list(set = set, get = get,
             setinv = setinv, 
             getinv = getinv)
}

## B. cacheSolve: This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been 
##    calculated (and the matrix has not changed), then the cachesolve should 
##    retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m.inverse <- x$getinv()
        if(!is.null(m.inverse)) {
                message("getting cached data")
                return(m.inverse)
        }
        data <- x$get()
        m.inverse <- solve(data, ...)
        x$setinv(m.inverse)
        m.inverse # Return a matrix that is the inverse of 'x'
}

## C. Testing the functions
cacheSolve(makeCacheMatrix(matrix(c(2, 1, 3, 0, 1, 7, 1, -4, -3), 3, 3)))

##              [,1]       [,2]        [,3]
##  [1,]  0.46296296  0.1296296 -0.01851852
##  [2,] -0.16666667 -0.1666667  0.16666667
##  [3,]  0.07407407 -0.2592593  0.03703704
