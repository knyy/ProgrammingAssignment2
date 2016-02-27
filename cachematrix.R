## https://github.com/knyy/ProgrammingAssignment2


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
    i <- NULL
    set <- function(y) { # sets x, initializes i to NULL
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setinverse <- function(mean) i <<- mean # sets i
    getinverse <- function() i # returns i

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) # returns the cacheMatrix
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

# Assume that the matrix supplied is always invertible.
# 1. it has to be square (same number of columns and rows)
# 2. needs to produced identity matrix as its reduced echelon form
# see sources below for more info
# https://www.youtube.com/watch?v=UdhXPXazRzU
# http://www.math.nyu.edu/~neylon/linalgfall04/project1/jja/group7.htm

cacheSolve <- function(x, ...) {

    i <- x$getinverse()
    if(!is.null(i)) {
            message("getting cached data") # i has been initialized and set
            return(i)
    }
    
    data <- x$get() # get the matrix
    # we are assuming its an invertible matrix, if not, refactor this portion with a try catch block
    i <- solve(data) 
    x$setinverse(i)

    ## Return a matrix that is the inverse of 'x'
    i
}


## Output ##
# test <- makeCacheMatrix(matrix(1:4, nrow = 2 , ncol =2)) # or use test$set
# test$get() # input matrix
#     [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# cacheSolve(test) # inverse matrix
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# cacheSolve(test) # retrieving cached matrix
# getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# second test case https://www.youtube.com/watch?v=w2t9VADaw10
# x <- list(2,3,2,1,2,1,1,1,2)
# > test <- makeCacheMatrix(matrix(unlist(x), nrow = 3 , ncol =3)) # or use test$set
# > cacheSolve(test)