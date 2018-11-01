## Downloading data from GEO

This tool works with **Series** data from the online repository Gene Expression Omnibus. According to the information on [GEO's website](https://www.ncbi.nlm.nih.gov/geo/info/overview.html):

*"A Series record links together a group of related Samples and provides a focal point and description of the whole study. Series records may also contain tables describing extracted data, summary conclusions, or analyses. Each Series record is assigned a unique and stable GEO accession number (GSExxx)."*

There are many tools available to search for series records on GEO. [Here](https://www.ncbi.nlm.nih.gov/gds/) is one of GEO's own tools.

Once you have found a series record you would like to work with, you can load it into this tool by inputting the GSE ID.

After inputting the GSE ID, you may specify some columns to filter out of the data upon downloading. Limiting the number of columns you have to work with can simplify the reformatting process and is less overwhelming. The options for columns to exclude from the data upon download are:

* All the same value
    + Columns with all the same value are often unhelpful because they don't measure meaningful change. If you select this option, columns with the same value for every row will be dropped from the data. 
* All different values
    + Columns with all different values are often unnecessary because there is already an ID column in the data. If you select this option, columns with different values for every row will be dropped. 
* Dates
    + Columns with dates are sometimes uninformative because they simply specify when the data was published. However, publishing date can sometimes be helpful in determining if the data consists of two merged datsets, a situation which can complicate the reformatting process. If you select this option, columns with dates in every row will be dropped. 
* Web addresses
    + Columns in which every row contains a web address starting with `ftp:://` are often unhelpful because they simply specify where the data was published. If you select this option, columns with web addresses in every row will be dropped.
    
Here is an example of the filtering functionality with GSE68849:
