## The below functions allow to work with matrices and get an inverse matrix
## from cache


## This function creates a list of functions to work with a matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse_m <- NULL
        set <- function(y) {
                x <<- y
                inverse_m <<- NULL
        }
        get <- function() x
        setinv <- function(inv_m) inverse_m <<- inv_m
        getinv <- function() inverse_m #returns inverse matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function inverses a matrix if this was not done earlier, otherwise it
## returns inversed matrix from chache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse_m <- x$getinv()
        if(!is.null(inverse_m)) {
                message("getting cached data")
                return(inverse_m)
        }
        data <- x$get()
        inverse_m <- solve(data, ...)
        x$setinv(inverse_m)
        inverse_m
        
}

##Below are two examples of matrices that can be inversed.  You can uncomment
##the lines and test the functions performance

#m <- matrix(c(2, 6, 5, 5, 3, -2, 7, 4, -3), nrow = 3, ncol = 3)
#m

#nm <- matrix(c(3, 6, 5, 5, 3, -2, 7, 4, -3), nrow = 3, ncol = 3) #same as m, but first element is changed


#z <- makeCacheMatrix(m)
#z$get()

#z$getinv() #returns NULL as inverse matrix has not been set yet

#z$setinv(solve(m)) #here the matrix is set 

#z$getinv() #retuns inverse matix

#cacheSolve(z) #returns inverse matrix from cache

#z$set(nm)
#z$get()
#z$getinv()
#cacheSolve(z) #returns inverse matrix but not from cache
