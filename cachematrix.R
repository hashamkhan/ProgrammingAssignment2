## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m<-NULL
        # sets the matix
        set<-function(y){
            x<<-y
            m<<-NULL
        }
        #gets the matrix x
        get<-function() x
        #sets the inverse of x
        setmatrix<-function(solve) m<<- solve
        #gets the inverted matrix 
        getmatrix<-function() m
        
        #return matrix with newly defined functions
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # If the inverse has already been calculated, 
    # retrieves the inverse from the cache
    m<-x$getmatrix()
    # If the inverse has already been calculated 
    # (and the matrix has not changed) return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Calculating the inverse matrix
    data <- x$get()
    inv <- solve(data, ...)
    # Caching the inverse matrix
    x$setinv(inv)
    # Return the inverse matrix
    inv
}

    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
