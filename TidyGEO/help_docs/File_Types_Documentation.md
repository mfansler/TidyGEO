## File types

Please see the following resources for information about the file types you can use to save the data.

* <a href="https://filext.com/file-extension/CSV" target="_blank">**Comma-separated file (CSV)**</a>
* <a href="https://filext.com/file-extension/TSV" target="_blank">**Tab-separated file (TSV)**</a>
    + <a href="http://www.it4nextgen.com/tsv-vs-csv-file/" target="_blank">TSV vs CSV</a>
* <a href="https://filext.com/file-extension/XLSX" target="_blank">**Excel file (XLSX)**</a>
    + <a href="https://blog.crossjoin.co.uk/2018/08/02/comparing-the-performance-of-csv-and-excel-data-sources-in-power-query/" target="_blank">Excel vs CSV</a>
* <a href="https://filext.com/file-extension/JSON" target="_blank">**JSON file (JSON)**</a>
    + <a href="https://www.educba.com/json-vs-csv/" target="_blank">JSON vs CSV</a>

## BRB Array Tools

BRB Array Tools is a software system that enables researchers do perform comprehensive statistical analysis of microarray experiments. The software is available as <a href="https://brb.nci.nih.gov/BRB-ArrayTools/ArrayToolsRPackages.html" target="_blank">a collection of R packages</a> but also as a <a href="https://brb.nci.nih.gov/BRB-ArrayTools/download.html" target="_blank">Microsoft Excel plugin</a> for easy access. While users can import datasets directly from Gene Expression Omnibus to BRB Array Tools, this limits users to the data as it appears on GEO, which may not be in a format amenable to analysis. TidyGEO, on the other hand, allows users to reformat the data so it is readily analyzed, but this also requires manual import from TidayGEO to BRB Array Tools.

### Importing from TidyGEO to BRB Array Tools

#### Prerequisites:

1. Download <a href="https://brb.nci.nih.gov/BRB-ArrayTools/download.html" target="_blank">BRB Array Tools Excel plugin</a>
2. Load the plugin using the instructions linked <a href="https://brb.nci.nih.gov/BRB-ArrayTools/download.html" target="_blank">here</a>

#### In TidyGEO:
1. Format the clinical data so every __column__ is a __variable__ and every __row__ is a __value.__ This is only necessary for the columns you need in your analysis.
2. Feel free to reformat the assay data, but make sure __not__ to transpose it. Every __column__ should be a __sample__ and every __row__ should be a protein/sequence/etc.
3. Format the feature data so every __column__ is a __variable__ and every __row__ is a __value.__ This is only necessary for the columns you need in your analysis.
4. Make sure that none of the column names have any of the following characters:

```
\ / : * ? < > = + # ~ ` ' ; & % $ @ !
```
5. In the `Merge data` > `Matching up datasets` tab, match the __clinical data rownames__ to the __assay data column names.__
6. You may also choose to match the __assay data ID column__ to the __feature data ID column__ but this is not strictly necessary.
7. In the `Merge data` > `Saving the data` tab, choose to save the data as "all (zipped)." Save it to a location on your computer where you can find it again. 

#### In BRB Array Tools:
1. Navigate to `Add-ins` > `ArrayTools` > `Import data` > `Data import wizard`
2. Select the Data Type for your experiment _How do you tell what datatype it is?_
3. Select "The expression data are in separate files stored in one folder."
    

