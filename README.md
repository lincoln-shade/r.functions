# r.functions
Some functions I commonly use for projects in R

## table.one.R

`TableOne()` is a function that takes a data.table input and creates a "Table 1"-style output data.table that can be an input table for kableExtra tables. 

- a current issue is that it only works for tabled stratified by a factor variable. It cannot currently output just an overall table. This can be circumvented by creating a dummy factor variable in the input data.table and then just deleting the stratified columns of the output, but I hope to fix this soon.
