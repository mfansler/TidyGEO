#Splitting key-value pairs

If one or more columns contain key-value pairs separated by a delimiter, like

variable_1      | variable_2     | variable_3
--------------- | -------------- | ----------------
Outcome=Success | Group: control | treatment - drug

...then these columns might need to be split so the value is by itself, for example

Outcome |  Group  | treatment
------- | ------- | ---------
Success | control | drug

Here is an example of the split variables functionality in action:
<img src="www/separate_example.gif" alt="drawing" width="575"/>
