## ===============================================================================
## R-Programming: Assignment 2 --- Cache a matrix and its inverse
##
## Function makeCacheMatrix creates a 'special' matrix object which function
## cacheSolve uses to calculate and cache its inverse. 
## ===============================================================================

## --------------------------------------------------------------- makeCacheMatrix
## Creates a matrix object, returning functions that cache and retrive the
## data matrix and its inverse. 
##
## Usage:  xx <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))

makeCacheMatrix <- function(inmat=matrix()) {
       matinv <- NULL
       set    <- function(newmat)                         # change the data matrix
                 {  inmat  <<- newmat                     # .. store new data                 
                    matinv <<- NULL                       # .. wipe cached inverse               
                 }
       get    <- function()        { inmat }              # return the data matrix
       setinv <- function(calcinv) { matinv <<- calcinv } # cache the inverse
       getinv <- function()        { matinv }             # return the inverse
       list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## -------------------------------------------------------------------- cacheSolve
## Acts on a matrix object created by makeCacheMatrix, returning the inverse.  
##
## Usage:  myinv <- cacheSolve(xx)
##
## If a valid inverse already exists the cached value is returned: if the cached 
## inverse is NULL then the inverse is calculated and cached. The cached inverse 
## will be NULL if the inverse has not yet been calculated OR if the data matrix 
## has been changed (using the 'set' function). 
 
cacheSolve <- function(cmat, ...) {
       calcinv  <- cmat$getinv()             # fetch cached inverse matrix        
       if  ( !is.null(calcinv) )  {          # valid inverse exists
             message("Getting cached inverse")
       } else {                              # need to calculate inverse
             data    <- cmat$get()           # .. fetch the data matrix
             calcinv <- solve(data, ...)     # .. derive inverse 
             cmat$setinv(calcinv)            # .. and cache
       }
       return(calcinv)
}
