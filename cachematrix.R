## A pair of functions can cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #Get the value of matrix
        i <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        #get the value of matrix
        get <- function()x
        #set the inverse of matrix
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        #get the inverse of matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

## cacheSolve: This function computes the inverse of the special "matrix"  
## returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached inverse matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
