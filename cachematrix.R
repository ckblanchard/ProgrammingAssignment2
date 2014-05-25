## These functions create a matrix and creates methods to retrieve the matrix
## as well as cache any computations done to the matrix

## makeCacheMatrix creates a matrix and creates methods to access the matrix and its cache
## calling "set" allows user to set the matrix
## calling "get" returns the matrix
## calling "cacheMatrix" caches a computation done on the matrix
## calling "getCache" retrieves the cached data

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    cacheMatrix <- function(matrix) m <<- matrix
    getCache <- function() m
    list(set = set, get = get, cacheMatrix = cacheMatrix, getCache = getCache)
}

## Write a short comment describing this function
## cacheSolve takes a given matrix and any additional arguments
## and first checks if there is a cached version of the matrix
## by calling "getCache" from the makeCacheMatrix function
## If there is a chace, it returns the cache, if not, it returns the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getCache()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$cacheMatrix(m)
    m
}
