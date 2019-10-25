# stylcoR

stylcoR is an implementation of stylistic corpus analysis in R. 
It combines the computation of frequency distributions and distance tables with the methods of descriptive statistics, classification, and network analysis.


You can install the package using the devtools function install_github:

library(devtools)

install_github("jbrottrager/stylcoR")


I've created it as a part of my MA-Thesis, for which I analysed English literary texts from 1688 to 1914. I'd love to provide access to the corpora I've used, but as some texts are still protected by copyright, I unfortunately can't. However, you'll find all relative frequencies for all features for all texts in the zipped directory "frequency_tables". 

If you want to reproduce my results, you can do so by running the R-file "iterative" and by replacing the variable "freq" (line 78) with the respective frequency table.




