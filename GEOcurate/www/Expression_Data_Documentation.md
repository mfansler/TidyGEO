## Formatting the expression data

Each Series file on GEO contains both clinical data and expression (assay) data. The expression data can be a helpful in determining differential expression. Sometimes, the expression data is not in a format that is easy to analyze.

This tool allows users to do four things:

1. Replace the ID column
    + The ID column in the expression dataset often represents probe set identifiers, but it is often more intuitive to represent the IDs as a different feature. 
    + This option allows you to take one of the columns from the feature dataset and use that column as identifiers for the expression dataset.
2. Transpose the data
    + Transposing the data switches the axes. In other words, the "x axis", or the column names, become the ID column, and the "y axis", or the ID column, becomes the column names.
    + This can be helpful because it makes the dataset wider or narrower, which can be easier to view and analyze.
3. Filter the data
    + Because these datasets are so large, you might want to filter the data to include only the variables that are interesting to you.
    + To filter the data, input some search terms in each column you would like to filter. The data will change to show you a preview of what columns will be included in the final result.
    + When you are satisfied with the filtering, click the "Evaluate filters" button to finalize the filters.
    + This will drop all the rows in the dataset that do not contain the search terms in the specified columns. It will also drop the rows from the other dataset (Assay data or Feature data) so that the datasets represent the same variables.
4. Save the data
    + Once you are satisfied with the format of the expression data, you can save the expression data file in any of the available formats.
    + Please note that files with more than 16,384 columns or more than 1,048,576 rows will not load in Excel. If the dataset is too large, you may consider filtering the data to fit within these bounds.
    + Additionally, you may download an R script that will allow you to replicate your edits on this dataset.