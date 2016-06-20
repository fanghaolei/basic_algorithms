# Iterative factorial
factorial <- function(n){
    result <- 1
    if(n != 0) {
        for(i in 1:n){
            result <- i*result
        }
    }
   return(result)
}

# Recursive factorial
factorial2 <- function(n){
    if(n == 0){
        return(1)
    } else {
        return(n*factorial2(n-1))
    }
}

# is palindrome?
isPalindrome <- function(string){
    if(nchar(string) == 0 | nchar(string) == 1){
        cat('length is 0 or 1, It is a palindrome!\n')
    } else {
        n <- nchar(string)
        firstc <- substr(string, 1, 1)
        lastc <- substr(string, n, n)
        if (firstc == lastc) {
            cat('first letter is the same as last letter!\n')
            string <- substr(string, 2, n-1)
            isPalindrome(string)
        } else {
            cat('It is not a palindrme!\n')
        }
    }
}

# power O(n) solution
pow <- function(x, n){
    if(n == 0) {
        return(1)
    } else if (n > 0) {
        return(x*pow(x, n-1))
    } else {
        return(1/x*pow(x, n+1))
    }
}

# power O(log(n)) solution
pow2 <- function(x, n){
    if(n == 0) {
        return(1)
        # n is even
    } else if (n > 0 & n %% 2 == 0) {
        return(pow(x, n/2)*pow(x, n/2))
    } else if (n >0 & n %% 2 != 0) {
        return(x*pow(x, n-1))
    } else {
        return(1/pow(x, -n))
    }
}
















