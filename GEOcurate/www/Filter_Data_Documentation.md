## Selecting informative columns

Upon loading the clinical data, you may notice some columns that are not useful for your research purposes. Here, you can drop those columns from the data. There are some preset options for columns to exclude, which are:

* *Columns in which every row has the same value*
    + Columns with all the same value are often unhelpful because they don't measure meaningful change. If you select this option, columns with the same value for every row will be dropped from the data. 
* *Columns in which every row has different values*
    + Columns with all different values are often unnecessary because there is already an ID column in the data. If you select this option, columns with different values for every row will be dropped. 
* *Columns in which every row is a date*
    + Columns with dates are sometimes uninformative because they simply specify when the data was published. However, the publishing date can sometimes be helpful in determining whether the data consists of two merged datsets, a situation which can complicate the reformatting process. If you select this option, columns with dates in every row will be dropped. 
* *Columns in which every row is a web address*
    + Columns in which every row contains a web address starting with `ftp:://` are often unhelpful because they simply specify where the data was published. If you select this option, columns with web addresses in every row will be dropped.
    
You may also choose to select columns by name that are useful to you.

**Click on the animations below to see an example of dropping columns from GSE68849:**
