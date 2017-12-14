# MATH 423 Final Project - NHANES Health &amp; Nutrition Study

This project focusses on the NHANES health and nutrition study
https://www.cdc.gov/nchs/nhanes/
which a large ongoing survey in the US. In R, a version of the study data is contained in the data frame NHANES stored in the library NHANES: this data frame contains 10000 observations made on 76 variables. It is available in R by installing the package.
```
install.packages('NHANES')
library(NHANES)
```
The help ﬁle for the data frame is here
http://127.0.0.1:23156/library/NHANES/html/NHANES.html
The data frame contains observations on years 2009-10 and 2011-12. It represents a representative(random) sample of the U.S. population obtained from raw survey data by weighted sampling. This project focusses on a subset of the data, from the 2011-12 year, for which complete observations are available on 21 of the variables; the data subset is stored in the comma separated ﬁle
http://www.math.mcgill.ca/dstephens/Regression/Data/Project/nhanes-sub.csv.
