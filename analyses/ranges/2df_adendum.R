#models_stanforranges was getting junky so I added this. Run the afore mentioned file unitl line 204

bb.stan.2dfs$latbinum <- as.numeric(as.factor(bb.stan.2dfs$latbi))

datalist.bb <- with(bb.stan.2dfs, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = latbinum,
                         N = nrow(bb.stan.2dfs),
                         n_sp = length(unique(bb.stan.2dfs$latbinum))
                    )
)

m2l.ni = stan('stan/nointer_2level_2dfs.stan', data = datalist.bb,
              iter = 6000, warmup=5000, control=list(adapt_delta=0.99)) 
