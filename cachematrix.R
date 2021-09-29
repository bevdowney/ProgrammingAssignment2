#This function creates a matrix object that can cache 
#its inverse.
makeCacheMatrix <- function(x = matrix()) { #initialize matrix object
        inverse <- NULL #initialize inverse object
                set <- function(y) {
                        x <<- y #assigns y to the x in the parent environment
                        inverse <- NULL #clears any old cached inverse
                }
                get <- function() x #retrieves x from parent environment
                setinverse <- function(solve) inverse <<- solve #assigns inverse to parent environment
                getinverse <- function() inverse #retrieves inverse from parent environment
                #sets appropriate labels fro getters and setters
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


#This function computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse() #gets inverse of matrix from makeCacheMatrix
                if(!is.null(inverse)) { #checks to see if there is already a cached inverse
                        message("getting cached data")
                        return(inverse) #returns cached inverse
        } #if there is no cached inverse, it is computed, cached and returned
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
