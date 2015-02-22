
## These two functions can be used to calculate and cache
## the inverse of one matrix. 

## In order to get the inversion of the matrix, the fitst 
## function makeCacheMatrix will create a list that has
## four functions in it and the second function cacheSolve
## will take the list created by the first function and 
## either take the result from the cache or calculate the 
## inverse.

## Example:
## m <- matrix(1:4,2,2)
## newmatrix <- makeCacheMatrix(m)
## cacheSolve(newmatrix)


## The function makeCacheMatrix can take the matrix and create
## a list including four functions. If you subset the content 
## of the list by using $set(), the set function will be called.
## Among the four functions, the get function can be used to get the
## matrix while the set function can be called to set the value.


makeCacheMatrix <- function(x = matrix()) {
        inverse.output <- NULL
        set <- function(y) {
                x <<- y
                inverse.output <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse.output <<- i
        getinverse <- function() inverse.output
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)

}

## This function takes the list created above and print the output.
## It computes the inverse of the special "matrix" which is
## returned by makeCacheMatrix above. If the matrix has not changed
## and the inverse has already been calculated, then the function 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse.output <- x$getinverse()
        if(!is.null(inverse.output)) {
                message("getting cached data")
                return(inverse.output)
        }
        data <- x$get()
        inverse.output <- solve(data)
        x$setinverse(inverse.output)
        inverse.output
        ## Return a matrix that is the inverse of 'x'
}
