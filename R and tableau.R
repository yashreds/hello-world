# Scripting functions in R

SCRIPT_REAL
#returns a real number but can have any number of arguments
#Usage
SCRIPT_REAL("

            Sales <- .arg1;
            [Shipping Cost] <- .arg2;
            Discount <- .arg3;
            [Unit Price]<- .arg4;
            
            fit <- lm(Sales ~ Discount + Unit Price + Shipping Cost)
            
            fit$fitted
            "
            ,
            SUM([Sales]),
            SUM([Discount]),
            sum([Unit Price]),
            sum([Shipping Cost])
            
)
#Here all the values used for regression are measure values

SCRIPT_STR
#returns a string but can have any number of arguments

SCRIPT_INT
#returns an intergral value but can have any number of arguments

SCRIPT_BOOL
#returns a boolean value but can have any number of arguments