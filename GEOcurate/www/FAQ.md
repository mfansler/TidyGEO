FAQ
---------------------


#### Downloading data
* __What does "Trouble establishing connection to GEO. Please try again later" mean?__
    + Your GEO ID is likely in the right format, but it doesn't exist on GEO. 
    + Try searching for that dataset on [GEO](https://www.ncbi.nlm.nih.gov/gds/) to see if it actually exists. If you're sure it does, try refreshing the app. 
    + Please make sure to use the identifier that starts with "GSE" (see the Series section on [this page](https://www.ncbi.nlm.nih.gov/books/NBK159736/)).
* __What does "File not found. Please enter a valid ID" mean?__
    + Your GEO ID is probably not in the right format. 
    + Try searching for that dataset on [GEO](https://www.ncbi.nlm.nih.gov/gds/) to see if it actually exists. Then copy and paste the identifier for that dataset into the text field. 
    + Please make sure to copy the identifier that starts with "GSE" (see the Series section on [this page](https://www.ncbi.nlm.nih.gov/books/NBK159736/)). 
    + Please make sure it doesn't have any extra characters (spaces, dashes, etc).
* __Why isn't the reset button doing anything?__
    + "Reset" restores the data to its original format upon being downloaded. If there haven't been any changes to the data, nothing will happen.

#### Reformatting columns
* __It says that the column has cells that don't contain the delimiter, but I can see that they do. How do I fix this?__
    + Take note of the rows and the values in the error message, and in the substitute values tab (tab 5), fix those values to have the same delimiter as the rest of the column. 
    + If the culprit is missing values ("NA", "?", "-" , etc.), you will have to substitute those values for NA. 
    + Then, try pressing "Reformat columns" again.
* __Why are my columns disappearing?__
    + Columns that are identical (have all the same values in the same positions) are automatically filtered out every time you make a change.
  
#### Excluding columns
#### Renaming columns
* __Why do the column names have periods in them after I have renamed them?__
    + There are certain rules in R (the language the app is built upon) that regulate how columns can be named. 
    + See the Details section of [this webpage](https://stat.ethz.ch/R-manual/R-devel/library/base/html/make.names.html) for details on what kinds of column names are allowed.

#### Substituting values
#### Excluding variables

#### Bugs and suggestions
* If you find an error or would like to make a suggestion, please [open an issue on the GitHub page.](https://github.com/bell-avery/geocurate/issues)
* TODO: enable issues on TidyGEO page from srp33

