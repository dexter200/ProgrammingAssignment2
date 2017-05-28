#makeCacheMatrix creates four functions that set and get the matrix and inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	
    #Creates the cached value as "inv_x" and initialize to NULL
    inv_x <- NULL

    #Create the matrix in the working environment, "x"
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }

    #Get the value of the matrix
    get <- function() x

    #Invert the matrix and store it as a cached value (inv_x)
    setinv<- function(inverse) inv_x <<-inverse

    #Or, get the values of the inverted matrix from cached value
    getinv <- function() inv_x

    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



#cacheSolve calculates the inverse of the matrix created in the function makeCacheMatrix
#If the cached value already existed in makeCahceMatix, cacheSolve will just return the cached value

cacheSolve <- function(x, ...) {

    #Get the values of the inverted matrix from cached value
    inv_x <- x$getinv()

    #If the cached value already exists, use the return the cached value
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)

    #Else, calculated the inverted matrix from here, and return the inverted matrix
    } else {
        inv_x <- solve(x$get())
        x$setinv(inv_x)
        return(inv_x)
    }
}