

# Function creates the matrix object
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# Function returns the inverse of the matrix. If the inverse matrix has
# already been calculated, it returns the cached value
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	
	if(!is.null(inv)) {
		message("getting cached data.")
		return(inv)
	}
	
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	
	inv
}