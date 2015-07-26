makeCacheMatrix <- function(matrix = numeric()) {
        
        inv <- NULL
        
        set <- function(y) {
                matrix <<- y
                inv <<- NULL
        }
        
        #checking if the input is a matrix
        if(!is.matrix(matrix)){
                print("Error: the input argument 'matrix' must be a matrix.")
                return()
        }
        
        get <- function(){
                matrix
        }
        
        setinv <- function(inverse) {
                inv <<- inverse
        }
        
        getinv <- function() {
                inv
        }
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(matrix) {
        
        inv <- matrix$getinv()
        
        if(!is.null(inv)) {
                message("Getting cached information...")
                return(inv)
        }
        
        x <- matrix$get()
        inv <- solve(x)
        matrix$setinv(inv)
        inv
}