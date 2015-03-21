## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL              ## inv variable will hold the cached value which the 
    set <- function(y) {     ## cacheSolve function will retrieve and test. 
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function will test first if the inverse of the matrix has
## already been computed. If so, it will skip computation  and
## will return the cached value. If not, it will solve and return
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()     ## This line will look for the value of inv.
    if(!is.null(inv)) {       ## This line tests whether there is value for the inv.
      message("getting cached data.")
      return(inv)
    }
    data <- x$get()           ## If there is no value for inv, scrypt will solve
    inv <- solve(data)        ## for the inverse of the matrix.
    x$setinverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}

x <- matrix(2:5, nrow = 2, ncol = 2)
m <- makeCacheMatrix(x)
m$get()
[,1] [,2]
[1,]    2    4
[2,]    3    5

## First run, there is no cached value for the matrix.
cacheSolve(m)
[,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1

## Second run, there is a cached value for the inverse of the matrix.
cacheSolve(m)
getting cached data.
[,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1
