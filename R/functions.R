#4
x <-3
x^2

square <- function(x){
	second <- x*x
	return(second)
}

square(x)
square(53)

53^2

addtwonumbers <- function(y,z){
	yplusz <- y+z
	return(yplusz)
}

addtwonumbers(4,6)

#5
raise <- function(x, power =2) {
	xtopower <- x^power
	return(xtopower)
}

raise(5, 3)


#6
x <- c(10, 10, 15)
sd(x)

centered <- x - mean(x)
centered
centeredsq <- centered^2
centeredsq
sumcenteredsq <- sum(centered^2)
sumcenteredsq
n <- length(x)
n
variance <- sumcenteredsq/(n-1)
variance
standev <- sqrt(variance)
standev


standarddev <- function(z){
	n <- length(z)
	if (n < 2){
		standev <- NA
	}
	else {
	centered <- z - mean(z)
	centeredsq <- centered^2
	sumcenteredsq <- sum(centered^2)
	variance <- sumcenteredsq/(n-1)
	standev <- sqrt(variance)}
	return(standev)
}

z <- c(2, 3)
sd(z)
sz <- standarddev(z)
sz

#Modify your function to remove the NA values before calculating the standard
#deviation. (Hint: the na.omit() function will be helpful!)
#Add an argument na.rm = that defaults to TRUE (the opposite of the na.rm
#argument in the built-in R function sd()). If na.rm = FALSE, then the function
#should return NA if there are any NA values in the vector

standarddev2 <- function(z, na.rm){
	if (na.rm) {
		z <- na.omit(z)
	}
		n <- length(z)
		centered <- z - mean(z)
		centeredsq <- centered^2
		sumcenteredsq <- sum(centered^2)
		variance <- sumcenteredsq/(n-1)
		standev <- sqrt(variance)
		return(standev)
}

z <- c(5, 6, 8)
sd(z)
sz <- standarddev2(z)
sz

income <- na.omit(nlsy$income)
sd(income)
standarddev2(nlsy$income, na.rm = FALSE)
standarddev2(nlsy$income, na.rm = TRUE)
