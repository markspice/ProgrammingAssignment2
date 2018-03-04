## These two functions store previous outcomes of an operation (in this case, 
## inverting a matrix) so that they can be recalled without having to re-run the 
## operation, provided that the arguments are unchanged.

## The makeCacheMatrix() function creates a square matrix (x) from a numeric
## vector (a), passed as an argument into the function, and the object i, which
## is set to NULL. [Note that, as it is only square matrices that are invertible,
## it is not necessary to set the dimensions of the matrix. If a vector with a 
## length that is not a square number, e.g. 1:6, is passed into the function, 
## the largest possible square matrix, in this case 2x2, will be created and the 
## remaining observations in the vector will be ignored.] The function outputs
## a list of four functions: set, get, setinverse and getinverse which can be 
## called by the cachesolve() function.

makeCacheMatrix <- function(a = numeric()) {
        x <- matrix(a,sqrt(length(a)),sqrt(length(a)))
        i <- NULL
        set <- function(b) {
                x <<- matrix(b,sqrt(length(b)),sqrt(length(b)))
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cashesolve() function passes the objects created by the makeCacheMatrix()
## function as an argument. If makeCacheMatrix() has been run prior to running 
## this function, i will be NULL. The function then calculates the inverse of the
## matrix x and stores this to i before printing i. (If the matrix is not 
## invertible it will cause an  error.) Where is.null(i)=FALSE, i.e. if 
## cashesolve() is re-run without running makeCacheMatrix(), the existing value
## for i is returned without recalculation.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
