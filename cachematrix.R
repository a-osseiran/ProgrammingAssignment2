## The purpose of the following functions is to invert a square matrix, store
## the results in cached matrix, and to retrieve the inverted matrix from the
## cache, if the content of the matrix has not changed. Doing so eliminates the
## need for repetitive and costly computations.


## The  makeCacheMatrix function creates a "special" matrix (x) that can cache
## the inverse of the input matrix (y) in a data object called m. 
## It does so by building a set of four functions (set(), get(), setmatrix(),
## and getmatrix()), then returns the functions (along with the data objects x and m)
## in a list to the global environment. 

## The set() function assigns the input arguments (y) to the x object, and the value
## NULL to the m object, in the global environment. Note that the cached value m 
## is cleared whenever x is reset. Therefore, any subsequent calls to the function
## cahceSolve() will recompute the inverse matrix rather than retrieve the wrong
## value from cache.

## The get () function returns the value of matrix 'x' when 'm' is set to NULL. 

## The setmatrix () function inverts the matrix 'x' and stores the inverted 
## matrix in 'm'

## Finally, the function creates a list of the four functions, and stores these 
## functions in the global environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## The cacheSolve function returns a matrix that is the inverse of 'x'.
## It does so by looking at the data object 'm' in the global environment.
## If 'm' is a NuLL matrix then the function will retrieve the matrix stored in 'x'
## then invert it using the solve function, and assigns it to data object 'm'
## Afterwhich, the inverted matrix is cached in the global environment then printed.
## Calling on the cacheSolve function again will return the cached matrix 'm'
## if the contents of the matrix has not changed.

cacheSolve <- function(x, ...) {

  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix (m)
  m
}