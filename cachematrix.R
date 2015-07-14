## This R function sets up a matrix representation that caches the
## inverse of that matrix.

## Create a matrix representation with a cached inverse matrix.
## Function set, resets the data that is represented.
## Function get, retrieves the matrix data.
## Function setsolve, computes the inverse of the stored matrix.
## Function getsolve, returns the inverse of the stored matrix.
makematrix <- function(x = matrix()) {
    xi <- NULL
    set <- function(y) {
      x <<- y
      xi <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) xi <<- solve
    getsolve <- function() xi
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Compute the inverse of x, where x is the return value from makematrix.
## The cached inverse stored in x is returned if it is present.  Otherwise,
## the matrix stored in x is inverted and stored back into x; then returned.
cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xi <- x$getsolve()
    if (!is.null(xi)) {
        message("getting cached data")
        return(xi)
    }
    data <- x$get()
    xi <- solve(data, ...)
    x$setsolve(xi)
    xi
}

testme <- function() {
  a <- matrix(c(2, -1, -1, 3), 2, 2)
  b <- matrix(c(3, -1, -1, 2), 2, 2)
  ac <- makematrix(a)
  ai <- cachesolve(ac)
  ai2 <- cachesolve(ac)
  print(ai %*% a)
  print(ai2 %*% a)
  bc <- makematrix(b)
  bi <- cachesolve(bc)
  bi2 <- cachesolve(bc)
  print(bi %*% b)
  print(bi2 %*% b)
}


