#makeCacheMatrix creates a special matrix 
#object which finds and caches the inverse 
#of itself. It goes through 4 steps: 
#setting the matrix, getting the matrix,
#Setting the inverse and getting the inverse



makeCacheMatrix <- function(x = matrix()) {
        InverseM <- NULL
        set <- function(y) {
                x <<- y
                InverseM <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setInverseM = setInverseM,
             getInverseM = getInverseM)
}

#Now my friends this next function will also
#find the inverse of the special matrix
#Which came from makeCacheMatrix. However, 
#this will retrieve the inverse from the
#cache if makeCacheMatrix has already
#calculated the inverse. 




cacheSolve <- function(x, ...) {
        
        InverseM <- x$getInverseM()
        if(!is.null(InverseM)) {
                message("getting cached data")
                return(InversM)
        }
        data <- x$get()
        InverseM <- solve(data)
        x$setinv(InverseM)
        InverseM
}


