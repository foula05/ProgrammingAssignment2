## create a special mattrix that caches its inverse
## and use it to either compute the inverse of a given
## matrix of return its cached value

## creates a list containing a function to
## 1. set the value of the mattrix
## 2. get the value of the mattrix
## 3. set and cache the value of the reverse of the mattrix
## 3. get the cached value of the inverse of the mattrix
makeCacheMatrix <- function(x = matrix()) {
  reverse_x <- NULL
  
  set <- function(y) {
    x <<- y
    reverse_x <<- NULL
  }
  
  get <- function() x
  
  set_reverse <- function(rev) reverse_x <<- rev
  
  get_reverse <- function() reverse_x
  
  list(set = set, get = get, set_reverse = set_reverse, get_reverse = get_reverse)
}

## either computes the inverse of a matrix 
## or returns its cached value
cacheSolve <- function(x, ...) {
  rvrs <- x$get_reverse()
  
  if(!is.null(rvrs)) {
    message("getting the cached version of the mattrix")
    return(rvrs)
  }
  
  data <- x$get()
  
  rvrs <- solve(data)
  x$set_reverse(rvrs)
  
  rvrs
}
