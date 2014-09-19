## Matrix inversion is usually a costly computation. makeCacheMatrix and 
## cacheSolve is a pair of functions that cache and return the inverse of 
## a matrix so that it does not need to be computed repeatedly  

## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get function will return the original matrix
        get <- function() x
        
        ## setInverseM function cache the inverse matrix
        setInverseM <- function(matrix) m <<- matrix
        
        ## getInervseM will return the inverse matrix
        getInverseM <- function() m
        list(set = set, get = get,
             setInverseM = setInverseM,
             getInverseM = getInverseM)
}


## CacheSolve is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## if getInverseM is not NULL, return the cached inverse matrix
        m <- x$getInverseM()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        ## if cache does not exist, inverse the matrix, then cache and return
        ## the inverse matrix
        data <- x$get()
        if(nrow(data)!=ncol(data)){
                stop("not a square matrix...")
        }else{
                m <- solve(data, ...)
                x$setInverseM(m)
                m     
        }      
}
