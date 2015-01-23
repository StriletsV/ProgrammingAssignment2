## First function creates a object, list, that contains functions to cache given matrix, get it from cache,
## cache it's inverse matrix, and get return this inverse matrix
## Second function takes this list, gets matrix and inverses it, then caches inverse matrix with help of setinv

## Creates a special object that can cache its value, give it back, cashe it's inverse value and get it back. 
## This object is a list with metods: set, get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Use object created by makeCacheMatrix to get matrix, count it's inverse and caches it with help of function setinv
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}