## This function takes a matrix as an input and then 
##calculates and caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## Set the values of a matrix
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }       
        ## Get the values of a matrix
        get <- function() x 
        ##Set the value of the inverse of the matrix
        setinverse <- function(solve) inv <<- solve   
        
        ##Get the value of the inverse of a matrix
        getinverse <- function() inv    
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function retrieves the cached data from "makeCacheMatrix", if no cahce is detected, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' from the cache
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If the inverse matrix is not cached then calulate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
