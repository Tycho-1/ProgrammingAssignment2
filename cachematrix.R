## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(A = matrix()) {
    Inv <- NULL
    set <- function(B) {
        A <<- B
        Inv <<- NULL
    }
    get <- function() A
    setInv <- function(Inverse) Inv <<- Inverse
    getInv <- function() Inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function


cacheSolve <- function(A, ...) {
    Inv <- A$getInv()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- A$get()
    Inv <- solve(data, ...)
    A$setInv(Inv)
    Inv
}