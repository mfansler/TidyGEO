## Shifting cells

Does your data contain values for a single variable (such as "Sex" or "Frequency") scattered across multiple columns? It may be helpful to merge the scattered values into a single column that corresponds to the variable.

Most often, scattered values are caused by missing values in a column, which shifts an entire row over. This can create a stairstep effect.

<style>
  table {
    width: 30%
  }
  table, th, td {
    border: 1px solid black;
    border-collapse: collapse;
  }
  th, td {
    padding: 5px;
  }
</style>

<table>
  <tr>
    <th>Letters</th>
    <th>Numbers</th> 
    <th>Symbols</th>
    <th>Words</th>
    <th>Questions</th>
  </tr>
  <tr>
    <td>A</td>
    <td>1</td> 
    <td>@</td>
    <td>frog</td>
    <td>?</td>
  </tr>
  <tr style="background-color:#FFFF00">
    <td>2</td> 
    <td>#</td>
    <td>sack</td>
    <td> </td>
    <td>?</td>
  </tr>
  <tr>
    <td>C</td>
    <td>3</td> 
    <td>$</td>
    <td>drop</td>
    <td>?</td>
  </tr>
  <tr style="background-color:#FFFF00">
    <td>%</td>
    <td>king</td>
    <td> </td>
    <td> </td>
    <td>?</td>
  </tr>
  <tr style="background-color:#FFFF00">
    <td>E</td> 
    <td>&</td>
    <td>call</td>
    <td> </td>
    <td>?</td>
  </tr>
  <tr>
    <td>F</td>
    <td>6</td> 
    <td>+</td>
    <td>wind</td>
    <td>?</td>
  </tr>
</table>
*In this example, row 2 is missing an entry for Letters, row 4 is missing entries for Letters and Numbers, and row 5 is missing an entry for Numbers. This shifts the values for rows 2, 4, and 5 to the wrong columns.*

To fix this:

1. Identify the furthest-right column where the shift stops--this is the destination column. In this example, **Words** is the destination column. **Questions** is not affected by the shift, so we will leave it alone.
2. Shift the values from the column to the left of the destination column--in this case, **Symbols**--into the destination column. If there are conflicts, make sure to keep the values from the destination column.
3. Find any other columns to the left of the destination column whose values belong in the destination column. In this example, **Numbers** also contains values that should be in the **Words** column. Shift each of these into the destination column.
4. Continue this process, working from right to left, until all of the values are in the correct places.

Another use case for shifting cells is if you would like to combine two columns into one, for example, if you wanted to combine "First Name" and "Last Name" into "Name". To achieve this, you would select the first column as the one to "Shift values from" and the second column as "into". When prompted to resolve the conflicts, you would select the "delimiter" option, where you could enter a space character or any other character to separate the values.


**Click on the animation below to see an example of shifting columns with GSE1581.**

