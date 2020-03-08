## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#changed "mean" to "solve", which is supposed to create the inverse of a matrix.
#replaced m with inv
# the function should creates a "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

#As above, changed "mean" to "solve" (to compute inverse rather than mean), which is supposed to create the inverse of a matrix and replaced m with inv
## This function computes the inverse of the special "matrix", using the solve function.
# first the function takes the value in "getinv" from made in the matrix object from above. if that object is NOT empty (or a NULL value so to say), the function finds the already saved value in getinv and returns that. if There is no value in getinv, it uses the solve function which gets the inverse of a matrix, setting the value of x to the inverse and returning that/those values) it saves the new inversed values as inv and returns those values.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


