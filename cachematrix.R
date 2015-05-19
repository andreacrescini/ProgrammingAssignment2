## Put comments here that give an overall description of what your
## functions do:

##(Source From the Assignment definition) "Matrix inversion is usually a costly computation and 
##their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
##So, this program is a pair of functions that cache the inverse of a matrix."


## Write a short comment describing this function:

##(Source From the Assignment definition) "This function creates a special "matrix" object that can
##cache its invers makeCacheMatrix"

makeCacheMatrix <- function(x = matrix()) {
  M_inv <- NULL
  set <- function(y) {
    x <<- y
    M_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) M_inv <<- inverse
  getinverse <- function() M_inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function

##(Source From the Assignment definition) "This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not
##changed), then the cachesolve retrieves the inverse from the cache""

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  M_inv <- x$getinverse()
  if(!is.null(M_inv)) {
    message("getting cached data.")
    return(M_inv)
  }
  data <- x$get()
  M_inv <- solve(data)
  x$setinverse(M_inv)
  M_inv
}
