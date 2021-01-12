## This function will instatiate a CacheMatrix with getters and setters,
## furthermore the result (i.e. the inverse) will be cached so that it does
## not have to be re-computed once it has already been computed.

## here we are just initializing, setting setters and getters


makeCacheMatrix <- function(matrx = matrix()) {
                # here the inverse i is instantiated
                i <- NULL
                        
                # setter for matrx
                set <- function(new_matrx) {
                        matrx <<- new_matrx
                        i <<- NULL
                }
                # getter for mat
                get <- function() matrx
                
                # setter for i
                setinverse <- function(new_i) i <<- new_i
                
                # getter for i
                getinverse <- function() i
                
                # list that stores the values so that they can be accessed
                list(set = set, get = get,
                        setinverse = setinverse,
                        getinverse = getinverse)   
}

## if computation of inverse stored return it else compute it

cacheSolve <- function(c_mtrx, ...) {
        # returns cached computation of inverse       
                  i <- c_mtrx$getinverse()
                  if (!is.null(i)) {
                          message("getting cached data")
                          return(i)
                  }
                  data <- c_mtrx$get()
                  i <- solve(data, ...)
                  c_mtrx$setinverse(i)
                  i
}

