## Put comments here that give an overall description of what your
## functions do
#the following functions are useful whenver the matrices are very large and one needs to avoid spending computing time

## Write a short comment describing this function
#creates and sets the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(ginv) m <<- ginv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#this function makes the operation "inverse", ie, computes the inverse of the matrix that has been cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    #implementing the solve function, which returns the inverse of a matrix (provided it's a square matrix)
    m <- solve(data, ...)
    x$setinv(m)
    m
}
