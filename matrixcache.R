makeCacheMatrix <- function(matrix = numeric()) {
        
        inv <- NULL  #start clearing the inverse matrix
        
        #set: function to input a new matrix
        set <- function(y) {
                matrix <<- y
                inv <<- NULL
        }
        
        #checking if the input is a matrix, otherwise shows an error message
        if(!is.matrix(matrix)){
                print("Error: the input argument 'matrix' must be a matrix.")
                return()
        }
        
        #get: returns the matrix stored
        get <- function(){
                matrix
        }
        
        #setinv: stores the inverse of the matrix
        setinv <- function(inverse) {
                inv <<- inverse
        }
        
        #getinv: returns the cached inverse
        getinv <- function() {
                inv
        }
        
        #listing functions created
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(matrix) {
        
        #inv <- the cached value
        inv <- matrix$getinv()
        
        #if a cached value exists return a message saying that and the value cached
        if(!is.null(inv)) {
                message("Getting cached information...")
                return(inv)
        }
        
        #in case there is a NULL value cached, compute the inverse of the matrix
        x <- matrix$get()
        inv <- solve(x)
        matrix$setinv(inv) #caching value
        inv #show the result
}