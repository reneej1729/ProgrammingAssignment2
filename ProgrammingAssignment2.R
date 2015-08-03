makeMatrix <- function(x = matrix()) {
     # takes in an invertible matrix of numbers, returns a matrix that contains 4 functions:
     # element [1,1]: the set function
     # element [1,2]: the get function
     # element [2,1]: the setsolve function
     # element [2,2]: the getsolve function
     
     m <- NULL             # creates the variable m in the makeMatrix environment
     set <- function(y) {  # stores the number matrix and sets cached inverse value to NULL
          x <<- y
          m <<- NULL
     }
     get <- function () x                    # returns the number matrix
     setsolve <- function(solve) m <<- solve # caches the inverse matrix
     getsolve <- function() m                # returns the cached inverse
     
     matrix(data = c(set = set, get = get, setsolve = setsolve, getsolve = getsolve), nrow = 2, ncol = 2)
}

cacheSolve <- function(x) { 
     # cacheSolve takes in a matrix made by makeMatrix and returns the inverse.
     # It first checks to see if the inverse has previously been calculated and will
     # return the cached answer, if it exists.
     
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
