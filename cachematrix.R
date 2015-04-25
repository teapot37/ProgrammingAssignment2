## functions dealing with an invertable matrix
## two values are cached:
## x is the value of the matrix
## inv is the value of the inverse of x

makeCacheMatrix <- function(n, ...) {
    ## initialize the value of x to what was passed in the call and clear the inverse
    x <- n
    inv <- NULL
    ## this function will reset the value of the matrix and clear the inverse
    set <- function(y){
          x <<- y
          inv <<- NULL
    }
    ## "get" is a retrieval function that returns the value of the original matrix
    get <- function() x
    ## "setinv" sets the value of the cached inverse to whatever is passed to it
    setinv <- function(inverse) inv <<- inverse
    ## "getinv" returns whatever value is cached for the inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## this is a helper function to actually calculate the inverse that is
## stored in the makeCacheMatrix object

cacheSolve <- function (x, ...){
    ## retrieve whatever value is presently stored as the inverse of the matrix
    inv <- x$getinv()
    ## if the inverse has already been set in the makeCacheMatrix object
    ## then retrieve that value and return it without recalculating
    if(!is.null(inv)){
          message("getting cached inverse data")
          return(inv)
    }
    ## otherwise, calculate the inverse of the matrix, set it in the 
    ## makeCacheMatrix object, and return that value
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
