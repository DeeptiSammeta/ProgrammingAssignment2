## This Program achives the caching of the inverse of the matrix

##makecachematrix shall create a list object which has the following function
##1. get the value of the matrix
##2. set the value of the matrix
##3. get the value of the inverse matrix
##4. set the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve(x)
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##cachesolve function shall return the inverse if the data is not found
## in the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}



