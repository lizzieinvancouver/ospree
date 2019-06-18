README for munich clim data

Lizzie downloaded (raw) from KNMI climate explorer: https://climexp.knmi.nl/ecatemp.cgi?id=someone@somewhere&WMO=4432&STATION=MUNCHEN-BOTANISCHER_GARTEN&extraargs=
generating:

munichclim.txt

munichclim_noheader.txt is the same minus the header rows, for some reason I could not get this to read in via read.delim("\t") in R so I opened it in and Excel and made input/munichclim_noheader.csv from there.