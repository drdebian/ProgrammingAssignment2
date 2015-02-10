## makeCacheMatrix: create matrix object that can cache its inverse
## cacheSolve:      compute the inverse matrix if cache is empty

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                       ## initialize i
  set <- function(y) {                            ## define the set() function
    x <<- y                               
    i <<- NULL
  }
  get <- function() x                             ## define the get() function
  setinverse <- function(inverse) i <<- inverse   ## define the setinverse() function
  getinverse <- function() i                      ## define the getinverse() function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)                   ## return the list of 4 functions
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {    ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()               ## try to get the cached inverse matrix
  if(!is.null(i)) {                 ## success if i is not null
    message("getting cached data")  ## confirm cached version being returned
    return(i)                       ## leave if scope
  }
  data <- x$get()                   ## if there is no cache,
  i <- solve(data, ...)             ## we have to do the heavy lifting right here
  x$setinverse(i)                   ## and store the result in the cache
  i                                 ## as well as return it to the caller
}
