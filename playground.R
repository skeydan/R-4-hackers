library(dplyr)
library(purrr)
require(stats)


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

# comparing typeof and mode
# closure is used for distinguishing from primitives
# A closure has three components, its formals, its body and its environment
tests1 <- c(expression, `<-`, `c`, `if`, call, `[`, length, sum, `>`, which, nrow, eval)
sapply(tests1, mode)
sapply(tests1, class)
sapply(tests1, typeof)
sapply(tests1, print)


# expressions and calls
# an R expression vector is a list of calls, symbols etc, for example as returned by parse
expression(1,2)
eval(expression(1,2,3))
# call returns an unevaluated function call, that is, an unevaluated expression which consists of the named function applied to the given arguments (name must be a quoted string which gives the name of a function to be called). Note that although the call is unevaluated, the arguments ... are evaluated.
call("sum", 1,2)
eval(call("sum", 1,2))
# or use do.call (expects arguments in list)
do.call("sum", list(1,2))
# Objects of mode "list" can be coerced to mode "call"
# the function argument must be a symbol
as.call(list(`sum`, 1,2))
eval(as.call(list(`sum`, 1,2)))

# language corresponds to basically all unevaluated object types other than constants or names
tests2 <- c(y ~ x, expression(1+2), call("sum", 1,2), quote(x+1))
sapply(tests2, mode)
sapply(tests2, typeof)
sapply(tests2, print)

tests3 <- c(formals(lm)[[1]], quote(x))
sapply(tests3, mode)
sapply(tests3, typeof)
sapply(tests3, print)

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

### call + eval ###
y <- 10.5
e1 <- call("round", y)       
e2 <- call("round", quote(y)) 

e1
e2

rm(y)
eval(e1)
eval(e2)


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

#compare
l2[[2]][[1]] # single element
l2[[2]][1] # list of 1
l2[2][1] # list of lists


# ----------------------------#
#   sapply            etc.    #
#-----------------------------#

l1 <-list(
  col1 = "a",
  col2 = "b",
  col3 = c("c", "d")
)
l1
str(l1)

l2 <- l1[1:2]
l2
str(l2)

u1 <- sapply(l1, toupper)
str(u1)

u2 <- sapply(l2, toupper)
str(u2)






