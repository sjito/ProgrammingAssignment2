## These two functions are used to create a special object that stores
## a matrix and cache's its inverse.

## This function creates a list containing four functions to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCacheMatrix <- function(InvMatrix) m <<- InvMatrix
        getCacheMatrix <- function() m
        list(set = set, get = get,
             setCacheMatrix = setCacheMatrix,
             getCacheMatrix = getCacheMatrix)
}


## This function returns the inverse of the matrix.  However, it first 
## checks if the inverse has already been calculated.  If it has, then 
## it returns the value of the inverse and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the 
## value of the inverse of the matrix in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getCacheMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setCacheMatrix(m)
        m
}
