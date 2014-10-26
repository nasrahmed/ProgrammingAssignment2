## The following functions will help tp cache the inverse of a matrix

## makeCacheMatrix is a function used for creating a special matrix, which is a list 
## containing a function to
## (1) Set the values of a matrix
## (2) Get the values of a matrix
## (3) Set the values of the inverse of the matrix
## (4) Get the values of the inverse of the matrixk

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) mat <<- solve
    getsolve <- function() mat
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve is a function calculates the inverse of a matrix, but it first checks to see
## if the inverse of the matrix is cached or not and if not it will set te inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat <- x$getsolve()
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setsolve(mat)
    mat
}
