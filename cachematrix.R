## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse<- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x = matrix(), ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        mat.data <- x$get()
        m <- solve(mat.data, ...)
        x$setinverse(m)
        return(m)
}
