makeVector creates a special "vector", which is a list containing a function to

## makeCacheMatrix creates a matrix using a list containing a function to:
## 1. Set the value of the input matrix
## 2. Get the value of the matrix stored in the function
## 3. Set the value of the inverse matrix using the solve function
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, 
             setmatrix = setmatrix, 
             getmatrix = getmatrix)
}


## CacheSolve calculates a matrix that is the inverse of 'x' from the makeCacheMatrix function
## If the inverse of the matrix has already been calculated then the cached inverse matrix 
## will be returned from the setmatrix function
## If not, the inverse matrix is calculated and returned and the new result cached in the setmatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}