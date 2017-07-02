Started 29 June 2017
By Lizzie (so far)

Multiresp info
Multiresp (aka, multiple responses) is supposed to identify cases where we have extracted duplicated data in one way or another.

The columns are created in clean_respvar.R and are:

multiresp: signifies studies that have multiple original respvars and they become the same in respvar.simple (for rows that had multiple respvars but only one respvar.simple)

multibothresp: signifies studies that have multiple original respvars and each one is a separate respvar.simple 

multi_respvar: identifies studies that have multiple response variables in the respvar column

multi_respvar.simple: identifies studies that have multiple response variables in the respvar.simple column