Errors
-----------------------

1. When you try to click the download button after something has been downloaded already (after you click "Undo"... you can check if this is always the case), you get the following error: 
```
Warning: Error in normalizeChoicesArgs: Please specify a non-empty vector for `choices` (or, alternatively, for both `choiceNames` AND `choiceValues`).
observeEventHandler [C:\Users\Avery\Documents\R_Code\geocurate_repo\GEOcurate/app.R#371]
```
__SOLVED__
Kept the platforms module from showing if the platforms do not exist. "Undo" should work normally now.

2. After downloading GSE68849, if you click on the summary tab, it will throw the following error (perhaps it has something to do with the semicolons in the column names?):
```
Warning: Error in <Anonymous>: No handler registered for type .clientdata_output_agent:ch1_width
  [No stack trace available]
Error in (function (name, val, shinysession)  : 
  No handler registered for type .clientdata_output_agent:ch1_width
```
__SOLVED__
Added make.names to the plotting code (Summary tab) to get rid of the illegal characters in the column names, but just for the back-end of the plots.

3. GSE10 Expression graphical summary: `Error: missing value where TRUE/FALSE needed`

    __SOLVED__
    Added `na.rm = TRUE` in the renderUI where it's figuring out the default width of the bars

4. `cannot remove prior installation of package 'mime'`

    __SOLVED__

     - Screenshot the place where the packages are being installed
     - End RStudio using Task Manager
     - Navigate to the place where the packages are being installed
     - Try to delete `mime` directory
     - If the directory says it's being used by another program,
     - Navigate to the innermost level of the directory
     - Attempt to delete the innermost file
     - The message will tell you which program is using the directory
     - Find that program in Task Manager
     - End the program
     - Open RStudio again
     - Reinstall the package
 
5. `Error in value[[3L]](cond) : there is no package called ‘markdown’`

    __SOLVED__
    Added `library(rmarkdown)` to app.R (https://community.rstudio.com/t/error-in-deploying/33173/14)

Fixes
----------------------
1. The app does not allow the user to choose which new column name to remove.

    __LEFT__ The "remove" button just takes off the last name that was specified, so if the user wants to delete a different one, they must start over.
2. The app allows the user to put multiple of the same column name.

    __SOLVED__ Added unique = TRUE to make.names in the renameCols function.
3. When substituting values, the options of which values to substitute do not update after the substitute operation has been evaluated.

    __SOLVED__ Updated the options after the "Substitute" button is pressed.
4. The feature data table does not have select inputs for any of the filters, even though those columns should be factors.

    __LEFT__ 
     - https://stackoverflow.com/questions/49699991/how-to-change-the-column-filter-control-in-r-shiny-datatables 
     - https://rstudio.github.io/DT/
5. The app is taking too long to load because of the series list file.

    __SOLVED__ Used feather files instead of RDS files to store the data.
```
[1] "RDS time:"         "0.201472043991089"
[1] "Feather time:"      "0.0897789001464844"
 ```
6. The app is still taking too long to load because of the series list file.

    __SOLVED__ Prepared the feather files all beforehand (naming the columns correctly, including the correct columns) in another script, because making a dataframe in the list dropdown is very time-expensive.
```
[1] "Reading files 0.107712984085083"
[1] "Populating dropdown 0.00898313522338867"
```
 