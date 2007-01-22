input.format <- function(x, n.digits, miss="NA") {

################################################################################
# Function: input.format
# Programmer: Tom Kincaid
# Date: January 25, 2002
# Last Revised: March 24, 2006
# Description:
#   This function formats an input numeric value.  The number of digits after
#   the decimal point can be specified.  Missing values are allowed.
#   Input:
#      x = the input numeric value.
#      n.digits = the number of digits after the decimal point, which can be
#         zero.
#      miss = the missing value code expressed as a character string.  The
#         default is "NA".
#   Output:
#      A character variable that is one of the following, as appropriate: (1) a
#      character variable representation of a real number with the specified 
#      number of digits after the decimal point when the input numeric value is 
#      a real number, (2) a character variable representation of an integer when
#      the input numeric value is an integer, or (3) the missing value code when
#      the input numeric value is missing.
#   Other Functions Required: None
################################################################################

   if (is.na(x)) {
      rslt <- miss
   } else if(n.digits == 0) {
      rslt <- format(round(x, 0))
   } else {
      x.int <- ifelse(x >= 0, floor(x), ceiling(x))
      if (x.int == 0) {
         if (x == 0) {
            rslt <- "0"
         } else {
            rslt <- format(round(x, n.digits))
            nd <- ifelse(x >= 0, nchar(rslt) - 2, nchar(rslt) - 3)
            if (nd != n.digits) {
               if (rslt == "0") 
                  rslt <- paste("0.", paste(rep("0", n.digits), collapse=""),
                     sep="")
               else
                  rslt <- paste(rslt, paste(rep("0", n.digits - nd),
                     collapse=""), sep="")
            }
         }
      } else {
         if((x %% x.int) == 0) {
            rslt <- format(round(x, 0))
         } else {
            rslt <- format(round(x, n.digits))
            nd <- ifelse(x >= 0, nchar(rslt) - 2, nchar(rslt) - 3)
            if (nd != n.digits) {
               if (nchar(rslt) == nchar(format(x.int))) 
                  rslt <- paste(rslt, ".", paste(rep("0", n.digits),
                     collapse=""), sep="")
               else
                  rslt <- paste(rslt, paste(rep("0", n.digits - nd),
                     collapse=""), sep="")
            }
         }
      }
   }

   rslt
}
