README

Lizzie started this in early-ish January after discussion with JD and Dan B about adding a forecasting component to the ms. We thought maybe forecasting the long-term O'Keefe data would be a good idea, but the models are tricky to fit (actually, with an NCP the PMM fits well, the lambda is super high) ... and it's not clear what to compare the slopes (changed over years) to ... so I am giving up on this idea. 


This folder has all the useful HF data from O'Keefe updated as of early January 2023 (though I did not need all the files). 

Stan/
regularmodel_ncpb.stan -- regular partial pooling on intercept and slope with NCP on slope
regularmodel_ncpba.stan -- regular partial pooling on intercept and slope with NCP on intercept and slope
regularmodel.stan -- regular partial pooling on intercept and slope
uber_oneslope_cholesky_ncp.stan -- PMM with NCP on slope
uber_oneslopeintercept_modified_cholesky_updatedpriors_lamb0.stan -- from ospree repo but updated the priors and made ONE slope and intercept