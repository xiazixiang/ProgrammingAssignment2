## makeCacheMatrix is used to generate new matrix object,
## with cached inverse matrix and several functions to
## access or operate with the object.
## cacheSolve is used to access object's cached inverse
## matrix. Based on the result, it can just return cached
## inverse matrix if it is valid, or needs to calculate
## inverse matrix and store it back.


## makeCacheMatrix function contains initialization of
## internal cached inverse matrix "inv" and initialization
## of new matrix object. It also contains functions to
## get matrix content, set cached inverse matrix and get
## inverse matrix. A functions mapping list enables calling
## functions by names.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL ## Initialize cached inverse matrix
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }       ## Initialize new matrix object
        
        get <- function() {return(x)}           ## Return matrix object
        
        setinv <- function(i) {inv <<- i}       ## Set cached inverse matrix
        
        getinv <- function() {return(inv)}      ## Return cached inverse matrix
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)   ## Functions list to be called by names

}


## cacheSolve function first checks whether cached inverse
## matrix is valid or not by calling getinv() function of
## matrix object. If valid then it just returns the cached
## inverse matrix. If invalid then it calculates inverse
## matrix by calling solve() function to matrix content
## from get() function, and stores result by calling setinv()
## function. Certainly, it returns the result.

cacheSolve <- function(x, ...) {

        inv <- x$getinv()       ## Get cached inverse matrix of matrix object
        if(!is.null(inv)) {
                message("Cached data found.")
                return(inv)
        }       ## If cached inverse matrix is not NULL, return it
        
        message("Cached data not found. Calculation needed.")
        xdata <- x$get()        ## Otherwise, first get matrix object
        i <- solve(xdata)       ## Calculate inverse matrix
        x$setinv(i)             ## Set cached inverse matrix with calculated result
        
        return(i)       ## Return inverse matrix

}
