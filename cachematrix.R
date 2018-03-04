#The file has two functions. The first creates
#a cache for a matrix and the other either
#computes and stores the inverse of the matrix
#or it retrieves that value from the cache

#makeCacheMatrix creates a special matrix 
#object which finds and caches the inverse 
#of itself. It goes through 4 steps: 
#setting the matrix, getting the matrix,
#Setting the inverse and getting the inverse



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

}

#Now my friends this next function will also
#find the inverse of the special matrix
#Which came from makeCacheMatrix. However, 
#this will retrieve the inverse from the
#cache if makeCacheMatrix has already
#calculated the inverse. 




cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

}


