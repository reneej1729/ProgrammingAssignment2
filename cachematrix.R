## This file contains two functions that cache the inverse of matrix, if it has already
## been calculated. Otherwise, the inverse is calculated and stored for later reference.

# makeCacheMatrix takes in an invertible matrix of numbers, and
# returns a matrix that contains 4 functions:
# element [1,1]: the set function
# element [1,2]: the get function
# element [2,1]: the setsolve function
# element [2,2]: the getsolve function

makeCacheMatrix <- function(x = matrix()) {
     m <- x[[2,2]]()                       # uses getsolve to pull the current cached value
     if(!is.null(m)){                      # if a cached value exists, returns the value
          message("getting cached data")
          return(m)
     }
     
     data <- x[[2,1]]()                    # uses get to pull the data
     m <- solve(data)                      # finds the inverse, if not already done
     x[[1,2]](m)                           # calls setsolve to store the inverse value
     m 
}


# cacheSolve takes in a matrix made by makeMatrix and returns the inverse.
# It first checks to see if the inverse has previously been calculated and will
# return the cached answer, if it exists.

cacheSolve <- function(x, ...) {
     m <- x[[2,2]]()                       # uses getsolve to pull the current cached value
     if(!is.null(m)){                      # if a cached value exists, returns the value
          message("getting cached data")
          return(m)
     }
     
     data <- x[[2,1]]()                    # uses get to pull the data
     m <- solve(data)                      # finds the inverse
     x[[1,2]](m)                           # calls setsolve to store the inverse value
     m 
}
