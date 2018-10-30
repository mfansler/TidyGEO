## Downloading data from GEO

This tool works with **Series** data from the online repository Gene Expression Omnibus. According to the information on [GEO's website](https://www.ncbi.nlm.nih.gov/geo/info/overview.html):

> A Series record links together a group of related Samples and provides a focal point and description of the whole study. Series records may also contain tables describing extracted data, summary conclusions, or analyses. Each Series record is assigned a unique and stable GEO accession number (GSExxx).

There are many tools available to search for series records on GEO. [Here](https://www.ncbi.nlm.nih.gov/gds/) is one of GEO's own tools.

Once you have found a series record you would like to work with, you can load it into this tool by inputting the GSE ID.

After inputting the GSE ID, you may specify some columns to filter out of the data upon downloading. Limiting the number of columns you have to work with can simplify the reformatting process and is less overwhelming. The options for columns to exclude from the data upon download are:

* All the same value
    + If a column has the same value for every row, it will be dropped. Columns with all the same value are often unhelpful because they don't measure meaningful change.
    + Example: 
* All different values
    + If a column has different values for every row, it will be dropped. Columns with all different values are often unnecessary because there is already an ID column in the data. 
    + Example: 
* Dates
    + If a column has dates in every row, it will be dropped. Columns with dates are sometimes uninformative because they simply specify when the data was published. However, publishing date can sometimes be helpful in determining if the data consists of two merged datsets, a situation which can complicate the reformatting process.
    + Example:
* Reanalyzed by
    + Removes columns in which every row contains the phrase "Reanalyzed by:", which are often uninformative.
    + Example:
* Web addresses
    + Removes columns in which every row contains a web address starting with `ftp:://`, which are often unhelpful because they simply specify where the data was published.
    + Example:
  
Additionally, there is an option to download the expression data associated with this series record. Checking this option will allow you to work with the expression data in the "Expression data" tab. Leaving the box unchecked will make the download go faster. If you would like to download the expression data later, you will have to redownload the clinical data as well.

Here is an example of the filtering functionality with GSE68849:

<figure>
  <figcaption>Downloading data without using filters.</figcaption>
  <img src="download_example_no_filter.gif" alt="drawing" width="650"/>
</figure>

<figure>
  <figcaption>Downloading data using filters.</figcaption>
  <img src="download_example_with_filter.gif" alt="drawing" width="650"/>
</figure>