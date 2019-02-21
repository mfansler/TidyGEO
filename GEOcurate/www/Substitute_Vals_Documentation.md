## Substituting values in a column

You may want to substitute some of the values in a column for different values, for example, if you have missing (`NA`) values represented as "?" or "-", you can replace those values with `NA`.

In the To_Replace column of the table, you can specify which value you would like to replace. This can be an entire value (for example, "Group A") or any part of the value ("Group").

In the New_Val column, you can specify what to insert in the place of the value to replace. If you would like to note that a value is missing (for example, "?" or "-"), please type `NA` into the New_Val column. Leaving the New_Val column blank will substitute To_Replace with an empty string (""), not `NA`.

If you would like to add more values to substitute for the selected column, please right-click on one of the cells in the table and select "Add row". You can add as many rows as you like, but values in To_Replace that are not found in the data will not be replaced. Please note that the substitutions are evaluated in the order that they appear in the table.

You can also specify a range of numeric values to substitute. Once you have adjusted the slider to the desired range, please click "Add" to add this substitution to the table. Please note tha the range includes both the lower and upper limits. Similar to the value substitution, you can add as many range substitutions as you'd like. The range substitutions will be evaluated in the order they appear in the table.

When you have finished specifying all the replacements to make, please click "Substitute" to replace all the values with those you have specified.

If there is a mistake with how the substitutions were evaluated, you can click "Undo" to restore the data to its previous state. Please note that "Undo" only goes back one evaluation.

**Here is an example of the substituting variables functionality with GSE68849:**

