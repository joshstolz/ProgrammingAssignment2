## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        invertmatrix <- function(invertedmx) m <<- invertedmx
        getinvert <- function() m
        list(set = set, get = get,
             invertmatrix = invertmatrix,
             getinvert = getinvert)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$invertmatrix(m)
        m
}
