## Downloading data from GEO

This tool works with **Series** data from the online repository Gene Expression Omnibus. According to the information on [GEO's website](https://www.ncbi.nlm.nih.gov/geo/info/overview.html):

*"A Series record links together a group of related Samples and provides a focal point and description of the whole study. Series records may also contain tables describing extracted data, summary conclusions, or analyses. Each Series record is assigned a unique and stable GEO accession number (GSExxx)."*

[Here](https://www.ncbi.nlm.nih.gov/gds/) you can use GEO's search tool to find a dataset that aligns with your research interests.

Once you have found a series record you would like to work with, you can load it into this tool by inputting the GSE ID in the format GSExxx (e.g., "GSE68849").

After inputting the GSE ID, you may specify some columns to filter out of the data upon downloading. Limiting the number of columns you have to work with can simplify the reformatting process and is less overwhelming. The options for columns to exclude from the data upon download are:

* *Columns in which every row has the same value*
    + Columns with all the same value are often unhelpful because they don't measure meaningful change. If you select this option, columns with the same value for every row will be dropped from the data. 
* *Columns in which every row has different values*
    + Columns with all different values are often unnecessary because there is already an ID column in the data. If you select this option, columns with different values for every row will be dropped. 
* *Columns in which every row is a date*
    + Columns with dates are sometimes uninformative because they simply specify when the data was published. However, the publishing date can sometimes be helpful in determining whether the data consists of two merged datsets, a situation which can complicate the reformatting process. If you select this option, columns with dates in every row will be dropped. 
* *Columns in which every row is a web address*
    + Columns in which every row contains a web address starting with `ftp:://` are often unhelpful because they simply specify where the data was published. If you select this option, columns with web addresses in every row will be dropped.
    
**Click on the animations below to see an example of downloading GSE68849 from GEO:**
