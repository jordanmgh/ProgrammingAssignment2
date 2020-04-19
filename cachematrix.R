## These two functions can be used to set a matrix and its inverse
## and cache them for more efficient use later

## makeCacheMatrics returns a list containing four functions:
##  set - sets the matrix from the argument y
##  get - returns the previously set matrix
##  setinv - sets the inverse of the matrix from argument m
##  getinv - returns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(x_inv) inv <<- x_inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve returns the inverse of the matrix contained by argument (makeCacheMatrix) x
## gets the existing inverse if it has already been cached
## calculates and caches the inverse if the cached value is empty

cacheSolve <- function(x) {
  inv <- x$getinv()
  print("about to test for existing cache")
  if(!is.null(inv)) {
    print("getting previously cached data")
    return(inv)
  }
  print("about to set inverse")
  data <- x$get()
  inv <- solve(data)
  x$setinv(x)
  inv
}
