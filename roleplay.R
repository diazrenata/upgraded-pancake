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
                                   abund = totalN)) #TODO add default where all existing sumstats are added


ggplot(stats, aes(iteration, hill_abund_1)) +
    geom_line()


ggplot(stats, aes(iteration, abund)) +
    geom_line()

# endState <- getFinalState(model)
# 
# rawA <- rawAbundance(model@modelSteps[[1]])
# 


## Now try it with iter functions... (if iter functions work, this is all you need to do perturbation!sims)



p_untb_iter <- untbParams(individuals_local = function(iter){return(ifelse(iter < 500, 500, 200))}, 
                          individuals_meta = 1000, 
                          species_meta = 50, 
                          speciation = 0.2, 
                          dispersal_prob = 0.1, 
                          init_type = 'oceanic_island',
                          niter = 10000, niterTimestep = 100)   

model_iter <- runRole(roleModel(p_untb_iter))
stats_iter <-
    getSumStats(model_iter, funs = list(rich = richness,
                                   hill_abund= hillAbund,
                                   abund = totalN)) #TODO add default where all existing sumstats are added


ggplot(stats_iter, aes(iteration, hill_abund_1)) +
    geom_line()

ggplot(stats_iter, aes(iteration, abund)) +
    geom_line()


extractJVal <- function(modelStep) {
    
    sum(modelStep@localComm@spAbund)
}

for(i in 1:length(model_iter@modelSteps)) {
    
    print(extractJVal(model_iter@modelSteps[[i]]))
}


extractJPar <- function(modelPar) {
    
    sum(modelStep@localComm@spAbund)
}

for(i in 1:length(model_iter@modelSteps)) {
    
    print(extractJVal(model_iter@modelSteps[[i]]))
}


