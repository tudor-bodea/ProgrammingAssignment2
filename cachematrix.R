######################### ACKNOWLEDGEMENT #####################################
## MUCH OF THE THINKING AROUND WHAT IS IMPLEMENTED BELOW IS DISCUSSED AT: 
## https://class.coursera.org/rprog-016/forum/thread?thread_id=96
###############################################################################

###############################################################################
################################## OVERVIEW ###################################
###############################################################################
## THE TWO FUNCTIONS PROVIDED BELOW ALLOW ONE TO EVALUATE WHETHER OR NOT THE
## INVERSE OF A SUPPOSEDLY NONSINGULAR MATRIX HAS BEEN COMPUTED. IF IT HAS, THE
## INVERSE IS RETURNED FROM CACHE; OTHERWISE, THE INVERSE IS COMPUTED AND MADE
## AVAILABLE TO POSSIBLY OTHER DOWNSTREAM PROCESSES.

###############################################################################
########################## FUNCTION: makeCacheMatrix ##########################
###############################################################################
## INPUT-1: AN INVERTIBLE/NONSINGULAR SQUARE MATRIX
## OUTPUT-1: A LIST OBJECT THAT STORES THE INPUT MATRIX AND ITS INVERSE
## BEHAVIOR-1: THROUGH THE FOUR FUNCTIONS DEFINED IN ITS BODY, makeCacheMatrix
## BEHAVIOR-2: PROVIDES THE MEANS TO READ THE TWO STORED ELEMENTS (I.E., get
## BEHAVIOR-3: AND getinv) OR CHANGE THEM (I.E., set AND setinv). A NEW INPUT
## BEHAVIOR-4: MATRIX OR A CHANGE OPERATION (TRIGGERED BY set) ASSIGNS THE
## BEHAVIOR-5: INVERSE A VALUE OF NULL.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {x <<- y; inv <<- NULL}
    get <- function() {x}
    setinv <- function(inverse) {inv <<- inverse}
    getinv <- function() {inv}
    list(set = set, get = get, setinv = setinv, getinv = getinv)
} 
############################# END makeCacheMatrix #############################

###############################################################################
############################# FUNCTION: cacheSolve ############################
###############################################################################
## INPUT-1: THE OBJECT OF TYPE LIST CREATED BY makeCacheMatrix
## OUTPUT-1: THE INVERSE (COMPUTED OR CACHED) OF THE MATRIX USED AS INPUT FOR 
## OUTPUT-2: makeCacheMatrix OR UPDATED VIA set
## BEHAVIOR-1: IF THE LIST OBJECT HAS A NOT NULL INVERSE, cacheSolve RETURNS IT
## BEHAVIOR-2: FROM CACHE;
## BEHAVIOR-3: IF THE LIST OBJECT HAS A NULL INVERSE, cacheSolve READS THE 
## BEHAVIOR-4: STORED MATRIX FROM THE OBJECT, COMPUTES ITS INVERSE, UPDATES THE
## BEHAVIOR-5: VALUE OF THE APPROAPRIATE ELEMENT OF THE LIST AND RETURNS THE 
## BEHAVIOR-6: INVERSE

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        cat("##############################", "\n")
        cat("SHOWING CACHED DATA", "\n")
        cat("##############################", "\n")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    return(inv)
} 
################################ END cacheSolve ###############################
