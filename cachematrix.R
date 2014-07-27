## The following functions compute the inverse of a matrix by caching its
## value from previous computations, rather than solving repeatedly for the
## same calculation. The aim is to reduce time when inversing of a matrix is
## required to be done many times (e.g. in a loop) and the matrix itself
## remains  unchanged.


## The first function creates a "special matrix", which is really a list
## consisting of a function that sets the value of the matrix, gets the value 
## of the matrix, sets the value of the inverse matrix, gets the value of the 
## inverse matrix.


makeCacheMatrix <- function(A = matrix()) {
    Inv <- NULL

## Setting the value of the matrix in an environment inside the function
## that is different from the current one

    set <- function(B) {
        A <<- B
        Inv <<- NULL
    }

## Getting the value of the already stored matrix, which could be used from ## the second function 

    get <- function() A

## Storing the value of the inverse which could be  computed from the second 
## function

    setInv <- function(Inverse) Inv <<- Inverse

## Getting the inverse already calculated and stored, which can be used from
## the second function

    getInv <- function() Inv

## Making the output of the function a list, so that the second function can
## access the already stored values.

    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## The second function calculates the inverse of the "special matrix" created 
## from the first function. Before it proceeds with calculations it first
## checks, whether the inverse has already been calculated and stored by the
## first function. If so, the function returns the extracted value without
## actually calculating it, thereby saving time.


cacheSolve <- function(A, ...) {

## Getting the previously computed inverse (if there is one)

    Inv <- A$getInv()

## Checking if the inverse has been calculated

    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }

## Getting the actual data from the resulting list of the first function

    data <- A$get()

## Doing the inversion (if it has not been done already)

    Inv <- solve(data, ...)

## Storing the just computed inverse into the resulting list from the first
## function, so that it can be used later

    A$setInv(Inv)
    Inv
}