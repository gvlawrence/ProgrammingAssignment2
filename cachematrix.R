################################################################################
## These functions (makeCacheMatrix and cacheSolve) calculate the inverse of a
## matrix 'x' and save it to the cache. Future requests to calculate the same
## inverse are then satisfied using the cache rather than recalculated.
################################################################################

##------------------------------------------------------------------------------
## makeCacheMatrix:
##    create and return a vector/list of the following sub-functions:
##     - set (sets the value of the input matrix 'x')
##     - get (retrieves the previously set value of the input matrix 'x')
##     - setinv (sets the value of the inverse of matrix 'x')
##     - getinv (retrieve the previously set value of the inverse of matrix 'x')
##------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    ## check for a previously cached matrix inverse
    ## if found, return this cached value as the required matrix inverse    
    
    m <- NULL  ##initialise the cache
    set <- function(y) {
        x <<- y     #assign input matrix y to x in the parent environment
        m <<- NULL  #set m to NULL in the parent environment
    }
    
    ## define sub-functions 
    get <- function() x   #return matrix 'x'
    setinv <- function(inv) m <<- inv
    getinv <- function() m   #return inverse of 'x' retrieved from cache
    
    ## return the sub-function list 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}

##------------------------------------------------------------------------------
## cacheSolve: 
##    calculate and return the inverse of 'x', where cx is a sub-function list 
##       created previously using cx <- makeCacheMatrix(x)
##------------------------------------------------------------------------------
cacheSolve <- function(cx, ...) {
    
    ## check for a previously cached matrix inverse
    ## if found, return this cached value as the required matrix inverse
    invx <- cx$getinv()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    
    ## no cached inverse found, so calculate it now using solve()
    data <- cx$get()
    invx <- solve(data, ...)
    cx$setinv(invx)
    invx
}
