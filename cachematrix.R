## the makeCahceMatrix creates a matrix inverse of m.  This stores the value
##the cache Solve checks if the matrix has an inverse stored, if not it calculates
## and returns the matrix

##This function makeCacheMatrix stores a matrix so that it does not
## have to be calculated each time it is needed.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<- function() x
        setinv<- function(inverse) m<<- inverse
        getinv<-function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## calculates the inverse of the matrics created in the above function.
## It checks to see if the inverse has already been calculated.  If so it gets the 
## inverse from the cache and skips computation.  Otherwise it calculates the inverse
## of the matrix and sets the value of the invers via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinv()
        # Checks if m is already stored
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        data<-x$get()
        m<-solve(data)
        
        x$setinv(m)
        
        return()
}
