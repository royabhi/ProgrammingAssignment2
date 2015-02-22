## Creating a special matrix that caches its inverse
## Then computing and stoing the inverse in it's cache 

## This function creates a special matrix which caches it's
## own inverse to avoid it's computation multiple times
## hence saving time

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialising inverse as NULL
        inv <- NULL
        
        ## Set function should assign matrix 
        ## and set value of inverse to NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        } 
        ## Get function simply returns the 
        ## matrix
        get<-function() x
        
        ## setInversefunction should set
        ## the inverse matrix
        setInverse<-function(inverse) inv<<-inverse
        
        ## getInverse function should fetch
        ## the inverse of the matrix
        getInverse<-function() inv
        
        ## Creating the special matrix
        list(set=set, get=get, 
             setInverse=setInverse, 
             getInverse=getInverse)
}


## This function takes the special matrix and looks for
## the inverse value in the special matrix's cache
## if not found the function will compute it and store in
## the special matrix's cache

cacheSolve <- function(x, ...) {
        ## get cachd value
        inv<-x$getInverse()
        
        ## If cached value is not null return it
        if(!is.null(inv)){
                message("Getting Cached Data")
                return(inv)  
        }
        
        ## Otherwise generate inverse
        mat<-x$get()
        inv<-solve(mat)
        x$setInverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}
