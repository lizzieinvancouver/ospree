Info on the stan models in this folder!
Started 17 March 2018: Happy St Patrick’s Day!

<><><><><><><><><><>
Main models
<><><><><><><><><><>
nointer_2level: a(sp) + f(sp) + p(sp) + c(sp)


<><><><><><><><><><>
Other models
<><><><><><><><><><>
winternospwinternosp_2level: a(sp) + f(sp) + p(sp) + c(sp) + cf + cp + fp
winternospwinternosp_2level: a(sp) + f(sp) + p(sp) + c(sp) + cf(sp) + cp(sp) + fp(sp)
nointer_2level_studyint_ncp.stan: a(sp) + f(sp) + p(sp) + c(sp) + cf + cp + fp
nointer_2level_interceptonly: a(sp) + f + p + c (I think)
nointer_2level_interceptonly_sigmoid: a(sp) + f + p + sigmoid c
nointer_2level_interceptonly_sigmoid_adjchilla: a(sp) + f + p + sigmoid c (with a_chill forced to negative)