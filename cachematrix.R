# assignment 2

#creating make cache matrix function
# makeCacheMatrix: This function creates a special 
#"matrix" object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() )
{
    # inverse property
    inverse <- NULL
    #setter and getters
    set <- function(y) {
        m <<- y
        inverse <<- NULL
    }
    
    get <- function() m
    
    #set inverse function
    setInverse <- function(i) inverse <<- i
    #get inverse function
    getInverse <- function() inverse
    
    # return a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}

#creating matrix solve function

#acheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

cacheSolve <- function(x, ...) 
{
    # Return the inverse matrix i
    m <- x$getInverse()   
    
    # if calcualted before in chache, then voila it's done
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if not calculated then we start calculating it
    
    #first we parse the data
    data <- x$get()
    
    #then we inverse the matrix
    #note that : solve(A)    Inverse of A where A is a square matrix.
    m <- solve(data, ...) 
    
    #then we set the data inroder not to calcualte it again
    x$setInverse(m)
    
    #and finally return our matrix
    m
}





