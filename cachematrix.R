## Description: (some parts are directly copied from the instructions)

## There are two functions in the file as it is advised

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache


## makeCacheMatrix: It is used to cache the inverse matrix that has been calculated
## 
##There are four functions (methods) within the makeCacheMatrix function:
## The set and get functions are used to create the special matrix (object)
## and to make it available (through get) when necessary.

## The setinv and getinv functions are used to cache the inverse matrix (setinv) 
## and make it available (getinv) when needed.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve: This function decides whether the inverse matrix has already
## been calculated or not. 

## The getinv function looks for an already cached matrix 
## If the inverse of a matrix has already been calculated 
## it gets the cached matrix and assigns it in variable m. 

## The if statement is executed and the value of m is returned.

## Otherwise the data variable is getting the value of the initial matrix and
## it is reversed and stored to m variable. Then, the inverse matrix is cached, 
## for future use (using setinv),and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
