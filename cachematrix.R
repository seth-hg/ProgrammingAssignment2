## makeCacheMatrix() creates an CacheMatrix object from the 
## parameter x. The object supports 4 methods:
##
## set() - set the value of the matrix
## get() - get the value of the matrix
## settrans() - set the transpose of the matrix
## gettrans() - get the transpose of the matrix
## 
## The CacheMatrix object is represented with a list of functions.
## The matrix and its transpose are stored in local variables x 
## and tm.
## 
makeCacheMatrix <- function(x = matrix()) {
  tm <- NULL
  set <- function(y) {
    x <<- y
    tm <<- NULL
  }
  get <- function() x
  settrans <- function(trans) tm <<- trans
  gettrans <- function() tm
  list(set = set, get = get,
       settrans = settrans,
       gettrans = gettrans)
}


## cacheSolve() derives the transpose of a CacheMatrix object x.
## On the first time cacheSolve() is called for x, its transpose
## is saved using settrans() method. Subsequent calls can retrieve
## it using gettrans() method.
## 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  tm <- x$gettrans()
  if(!is.null(tm)) {
    message("getting cached data")
    return(tm)
  }
  data <- x$get()
  tm <- t(data)
  x$settrans(tm)
  tm
}
