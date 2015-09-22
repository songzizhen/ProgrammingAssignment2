## only calculates the inverse once

## How does it work:
##  M <- matrix(c(3,5,6,7),2,2)
##  a <- makeCacheMatrix(M)
##  cacheSolve(a)

## set() set x to be y and m to be NULL
## get() gets the x matrix
## getinverse()  if the inverse has already been calculated it, then get the inverse directly, else it is NULL
## setinverse(solve(x))  sets to calculat the inverse

## Create a cacheMatrix object for an invertale matrix.
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse<- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Return the inverse of an cacheMatrix object
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}