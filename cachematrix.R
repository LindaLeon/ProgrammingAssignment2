## makeCachematrix and cacheSolve are two functions that store lists of functions 
## associated with the cache and retrieval of a matrix and its inverse

## makeCacheMatrix has three functions in its list: get (which returns the matrix stored in the function),
## getinverse (which returns the inverse stored in the function),
## and setinverse (which stores a calculated matrix inverse in the function)

makeCacheMatrix <- function(x = matrix()) {
    inv<<-NULL
    m<<-x
    get <- function() m  ##Return the current matrix
    setinverse <- function(matrixinv) inv<<-matrixinv
    getinverse <- function() inv ##Return the inverse matrix that has been calculated for the current matrix
    list(get=get, getinverse=getinverse, setinverse=setinverse)
}


## CacheSolve calculates the inverse of a matrix if it does not already exist. 
## If the inverse has already been calculated for the given matrix, then it is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<- x$getinverse()
        if(!is.null(m)){return(m)
        }
    data<-x$get()
    m<- solve(data)
    x$setinverse(m)
    m
}
