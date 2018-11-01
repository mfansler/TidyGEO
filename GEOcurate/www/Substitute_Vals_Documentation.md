## Substituting values in a column

You may want to substitute some of the values in a column for different values, for example, if you have missing (`NA`) values represented as "?" or "-", you can replace those values with `NA`.

In the To_Replace column of the table, you can specify which value you would like to replace. This can be an entire value (for example, "Group A") or any part of the value ("Group").

In the New_Val column, you can specify what to insert in the place of the value to replace. If you would like to note that a value is missing (for example, "?" or "-"), please type `NA` into the New_Val column.

After filling the To_Replace and New_Val columns on the table, please click "Add" to add this pair to the queue below. The queue displays all the replacements for the column selected above. When you move between columns, the replacements will remain in the queue only if they have been added.

When you have finished specifying all the replacements to make, please click "Substitute" to replace all the values with those you have specified.

If there is a mistake with how the substitutions were evaluated, you can click "Undo" to restore the data to its previous state. Please note that "Undo" only goes back one evaluation.

Please note that when clicking "Remove" without a row selected, the first row in the table will be removed. To remove a specific row or group of rows, please select them in the queue before clicking "Remove".

Please note that when substituting a range of values, the specified range includes both the lower and upper limits. The ranges will be evaluated in the order they appear in the queue. Please see the example below.

Here is an example of the substituting variables functionality with GSE68849:

