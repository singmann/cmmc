
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
           free_parameter = "list",
           restrictions = "OptionalList",
           model = "list"))
