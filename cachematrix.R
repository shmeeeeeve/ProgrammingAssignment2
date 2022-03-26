## Hello! Welcome to my functions. These functions create a matrix that can
## cache its inverse and then caches the inverse. In my example, I cache the 
## inverse of "fancy_matrix." 

## makeCacheMatrix is a function that creates a matrix that can cache its
## inverse!

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(e) {
        x <<- e
        inv <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve caches the inverse of the matrix created above after checking 
## to see if the inverse has already been cached. If it hasn't, it'll 
## go ahead and cache the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
        message("getting cached data!")
        return(inv)
             }
  data_cache <- x$get()
  inv <- solve(data_cache, ...)
  x$setinverse(inv)
  inv
}

 ## Return a matrix that is the inverse of 'x'
fancy_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
fancy_matrix$get()
cacheSolve(fancy_matrix)