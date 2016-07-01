## Catch the inverse of a matrix:
## There are two functions used to create a matrix and store its inverse in a cache

## This function creates a "special matrix" that then can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
      x <<- y
      inv <<- NULL
}

get <- function() x
set_inverse <- function(inverse) inv <<- inverse
get_inverse <- function(inverse) inv
list(set = set, get = get,
     set_inverse = set_inverse,
     get_inverse = get_inverse)
}


##This function finds the inverse of the matrix created by the function above
##If the matrix has not changed, then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of
        inv <- x$get_inverse()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
      
}
