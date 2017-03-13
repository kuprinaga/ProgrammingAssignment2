#makeCacheMatrix creates a list containing a function to set and get the value 
#of the matrix, set and get get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#Function to get inverse matrix. Check if was computed before -> get result
#If not, compute, set  value in  cache with setinverse created ab0ve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse()
          if(!is.null(inv)) {
            message("working it")
            return(inv)
          }
          data <- x$get()
          inv <- solve(data)
          x$setinverse(inv)
          
          inv
}
