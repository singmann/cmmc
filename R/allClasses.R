
## virtual Classes needed for Slots:

setClassUnion("OptionalList", c("list", "NULL"))
#setClassUnion("OptionalFunction", c("function", "NULL"))


## Restriction

setClass("Restriction", 
         slots = list(
           restricted = "character",
           to = "character",
           type = "character",
           literal = "character"))

### CmmcMod
setClass("CmmcMod", 
         slots = list(
           predict = "function",
           objective = "function",
           gradient = "OptionalFunction",
           hessian = "OptionalFunction",
           bounds = "list",
           model = "list",
           parameters_show = "character",
           restrictions = "OptionalList"))
