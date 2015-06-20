
## This function makes a cache enabled matrix where the cache stores the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y){
        x<<-y
        ix<<-NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) ix<<- inverse
    getinverse <- function() ix
    
    list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function takes makeCacheMatrix as "x" and returns the inverse of the matrix
## It returns the inverse from the cache if previously calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ix <- x$getinverse()
    if(!is.null(ix)){
        print("getting from cache")
        return (ix)
    }
    data <- x$get()
    ix<-solve(data)
    x$setinverse(ix)
    ix
}

