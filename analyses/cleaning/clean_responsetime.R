## Started 28 Jan 2017 ##
## By Lizzie ##
## Oh no, I am working on a weekend! But will stop soon ##

## Eventually we should use this code to clean responsetime ##

# here's the issue:
# just to think on ...
thinkonme <- d[which(d$response>1),] # column nums of possible interest: 25:27
unique(thinkonme$respvar.simple)

# fix some of these issues ...
d$responsedays <- d$response.time
d$responsedays[which(d$response>1 & d$response.time=="")] <-
    d$response[which(d$response>1 & d$response.time=="")]


