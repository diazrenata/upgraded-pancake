library(roleR)
library(ggplot2)


## Parameterize a neutral role model...

p_untb <- untbParams(individuals_local = 100, individuals_meta = 1000, 
                     species_meta = 50, 
                     speciation = 0.2, 
                     dispersal_prob = 0.1, init_type = 'oceanic_island',
                     niter = 10000, niterTimestep = 100)   

model <- runRole(roleModel(p_untb))
stats <-
    getSumStats(model, funs = list(rich = richness,
                                   hill_abund= hillAbund,
                                   abund = totalN)) 


ggplot(stats, aes(iteration, hill_abund_1)) +
    geom_line()

ggplot(stats, aes(iteration, rich)) +
    geom_line()

ggplot(stats, aes(iteration, abund)) +
    geom_line()


# This is really all you need to get the feasible set comparisons. 
# Efficiency? It's a drag to be constantly recomputing draws from the feasible set. 


ggplot(stats, aes(rich, abund)) +
    geom_point()
