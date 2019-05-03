FAQ
---------------------


### Importing the data

* __What is a series?__
    + GEO is a database of experiments that scientists have uploaded to share publicly. One of the ways the experiments are organized is in a Series. A Series is a set of experiments that are closely related. They often pertain to a specific study. If you visit the page for any GEO Series, you can find a link to PubMed where the study has been published.
* __What does "Trouble establishing connection to GEO. Please try again later" mean?__
    + Your GEO ID is likely in the right format, but it doesn't exist on GEO. 
    + Try searching for that dataset on <a href="https://www.ncbi.nlm.nih.gov/gds/" target="_blank">GEO</a> to see if it actually exists. If you're sure it does, try refreshing the app. 
    + Please make sure to use the identifier that starts with "GSE" (see the Series section on <a href="https://www.ncbi.nlm.nih.gov/books/NBK159736/" target="_blank">this page</a>).
* __What does "File not found. Please enter a valid ID" mean?__
    + Your GEO ID is probably not in the right format. 
    + Try searching for that dataset on <a href="https://www.ncbi.nlm.nih.gov/gds/" target="_blank">GEO</a> to see if it actually exists. Then copy and paste the identifier for that dataset into the text field. 
    + Please make sure to copy the identifier that starts with "GSE" (see the Series section on <a href="https://www.ncbi.nlm.nih.gov/books/NBK159736/" target="_blank">this page</a>). 
    + Please make sure it doesn't have any extra characters (spaces, dashes, etc).
* __Why isn't the reset button doing anything?__
    + "Reset" restores the data to its original format upon being downloaded. If there haven't been any changes to the data, nothing will happen.
    
### Clinical data

* __What is clinical data?__
    + This data describes the samples that were used in the study. Each sample has an ID starting with “GSM”. The clinical data may include information about treatments (e.g. case vs control), demographic information (e.g. male vs female) or information about the experiment itself (e.g. the research institution). Knowing clinical information about a sample can be useful in interpreting patterns in the assay data. For example, if you know which samples are the cases and which are the controls, you can look at the differences between their assay measurements.
    
#### (1) Selecting informative columns
* __I selected some preset filters to remove columns from the data, but after I clicked "Filter columns", nothing happened. Why?__
    + The filters you selected would have dropped all the columns from the table, so no columns were dropped. Try unselecting one of the filters and clicking "Filter columns" again.
    + The "Is unique" filter is usually the culprit. Try unselecting this filter and clicking "Filter columns" again.

#### (2) Reformatting columns
* __It says that the column has cells that don't contain the delimiter, but I can see that they do. How do I fix this?__
    + Take note of the rows and the values in the error message, and in the substitute values tab (tab 4), fix those values to have the same delimiter as the rest of the column. 
    + If the culprit is missing values ("NA", "?", "-" , etc.), you will have to substitute those values for NA. 
    + Then, try pressing "Reformat columns" again.
* __Why are my columns disappearing?__
    + Columns that are identical (have all the same values in the same positions) are automatically filtered out every time you make a change.

#### (3) Renaming columns
* __Why do the column names have periods in them after I have renamed them?__
    + There are certain rules in R (the language the app is built upon) that regulate how columns can be named. 
    + See the Details section of <a href="https://stat.ethz.ch/R-manual/R-devel/library/base/html/make.names.html" target="_blank">this webpage</a> for details on what kinds of column names are allowed.

#### (4) Substituting values
* __When I try to use the numeric option, it says that my column is not numeric, even though I can see that it is. Why?__
    + If your column of interest only has one or two unique values, it is treated as a non-numeric column. This is because, usually, two or fewer unique numbers in a column indicates that the column is a categorical, rather than a discrete, variable.
    + With only two unique entries, you should be able to filter the data normally by selecting which value you would like to exclude.
* __How do I discretize a numeric column?__
    + Start by selecting the column you would like to discretize, and select the "Select a range" option.
    + Select a range of values that you would like to replace, for example, numbers 3 to 4 could change to "three to four".
    + Click the add button to add the range to the table.
    + In the column on the right, input the word or phrase you would like to replace the range with, for example, "three to four".
    + Right click on the table to add another row.
    + Continue choosing ranges and values to replace them with until you have covered the entirety of the column.
    + Click "Substitute" to evaluate the changes.
    + Please note that the ranges are inclusive and that they are evaluated in the order that they appear in the table. This means that if you have overlapping ranges, the numbers in the first range will take precedence.
    
#### (5) Excluding variables
* __When I try to use the numeric option, it says that my column is not numeric, even though I can see that it is. Why?__
    + If your column of interest only has one or two unique values, it is treated as a non-numeric column. This is because, usually, two or fewer unique numbers in a column indicates that the column is a categorical, rather than a discrete, variable.
    + With only two unique entries, you should be able to filter the data normally by selecting which value you would like to exclude.
    
### Assay data

* __What is assay data?__
    + This is the actual raw data of the experiment. Depending on the type of experiment, it may be protein or RNA expression levels, or any other kind of metric. The column names are the samples (“GSM”) and the values in the ID column are the metrics we are measuring. This data in conjunction with the clinical data is useful in answering research questions, such as, what kinds of patterns to we see in the RNA expression for cancer patients with stage III prostate cancer vs those with stage I prostate cancer?
* __What is feature data?__
    + This data describes the metrics you see in the ID column of the assay data. Depending on the experiment, these could be probe IDs, gene sequences, etc. Feature data is a kind of thesaurus for the assay ID column; it has an ID column in that matches the one in the assay data and all the other columns are like “synonyms” for those IDs. Sometimes, things like probe IDs might not be very useful, and you might want to look at, say, gene names instead. The feature data is where you get that information.
    
#### (1) Formatting the assay data
* __Why are some of the option buttons disabled?__
    + The "Use different column ID" button is disabled when the data is transposed because the ID becomes the column names, which is harder to substitute. If you would still like to replace the ID column, click "Undo" and "Use different column ID" before transposing.
    + The "Transpose" button is disabled when the entries in the ID column are not all unique. This is because transposing the data makes the ID column the column names, and duplicate column names are not allowed. To enable the "Transpose" button again, click on "Use different column ID" and make sure to use a summarize option other than "keep all". Also make sure to select "Drop NA values" underneath the summarize option.
* __What functionality is not available when my assay data is non-numeric?__
    + You cannot summarize non-numeric data with "Use different column ID". The only option is to keep all the duplicate entries.
    
### Bugs and suggestions

* If you find an error or would like to make a suggestion, please <a href="https://github.com/srp33/TidyGEO/issues" target="_blank">open an issue on the GitHub page.</a>

