library(dplyr)
library(purrr)

##################### tbd ###########################

# how to break lm
#
# object types, classes, etc. (overview)
# 
# functional programming
# 
# quote & co
# 
# OO systems

##################### tbd ###########################


### Object types ###
# internal types defined in C implementation
# typeof() : internal object type
# class() : official view ;-)

# Mode, storage.mode
# both relying on the output of typeof(x)
# Modes have the same set of names as types (see typeof) except that
# types "integer" and "double" are returned as "numeric".
# types "special" and "builtin" are returned as "function".
# type "symbol" is called mode "name".
# type "language" is returned as "(" or "call".

require(stats)

sapply(options(), mode)

cex3 <- c("1", "1:1", "1i", "list(1)", "data.frame(x = 1)",
          "pairlist(pi)", "c", "lm", "formals(lm)[[1]]",  "formals(lm)[[2]]",
          "y ~ x","expression((1))[[1]]", "(y ~ x)[[1]]",
          "expression(x <- pi)[[1]][[1]]")
lex3 <- sapply(cex3, function(x) eval(parse(text = x)))
mex3 <- t(sapply(lex3,
                 function(x) c(class(x), typeof(x), mode(x))))
dimnames(mex3) <- list(cex3, c("class(.)", "typeof(.)","mode(.)"))
mex3


### substitute ###

# substitute returns the parse tree for the (unevaluated) expression expr, substituting any variables bound in env.
# Substitution takes place by examining each component of the parse tree as follows: 
# If it is not a bound symbol in env, it is unchanged. 
# If it is a promise object, i.e., a formal argument to a function or explicitly created using delayedAssign(),
# the expression slot of the promise replaces the symbol.
# If it is an ordinary variable, its value is substituted, unless env is .GlobalEnv in which case the symbol
# is left unchanged.

mychar <- "snow"
f <- function(x) {
  substitute(x)
}
f(c(mychar, "rain"))


### quote ###
#quote simply returns its argument. The argument is not evaluated and can be any R expression.
quote(c(mychar, "rain"))


### expression ###
# an R expression vector is a list of calls, symbols etc, for example as returned by parse.
typeof(expression(c(mychar, "sun")))
expression(c(mychar, "rain"))

### call ###
# call returns an unevaluated function call, that is, an unevaluated expression which consists of the named
# function applied to the given arguments. Note that although the call is unevaluated, the arguments ... are evaluated.
call("sum", c(1,2,3))

# Objects of mode "list" can be coerced to mode "call". 
# wrong
a <- as.call(list(f = "sum", c(1,2,3)))
a
is.call(a) #TRUE
#eval(a)# Error in eval(expr, envir, enclos) : attempt to apply non-function

# right
b <- as.call(list(f = quote(sum), arg = c(1,2,3)))
b
is.call(b)
eval(b)

### eval ###
# arguments may or may not be quoted
y <- 10.5
call("round", y)       
call("round", quote(y)) 
eval(call("round", y))
eval(call("round", quote(y)))


# ----------------------------#
#   lists and extract etc.    #
#-----------------------------#
# how lists print
l <- list("a", "b")
l
str(l)

l2 <- list("a", list("b","c"))
l2
str(l2)

# extract element
l[[2]]
str(l[[2]])
typeof(l[[2]])

l2[[2]]
str(l2[[2]])
typeof(l2[[2]])

# extract sublist
l[2]
str(l[2])
typeof(l[2])

l2[2]
str(l2[2])
typeof(l2[2])


# ----------------------------#
#   sapply            etc.    #
#-----------------------------#

l <-list(
  col1 = "a",
  col2 = "b",
  col3 = c("c", "d")
)
l




