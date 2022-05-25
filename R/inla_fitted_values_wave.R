function_inla_fitted <- function(Wave_Inf ){
  
  # load("data/data_bern.RData")
  
  load("data/data_bern_wave.RData")

  
  nc.sids  <- sf::st_read("data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
    mutate(GEM_ID = ifelse(Gemeinde=="MATTEN BEI INTERLAKEN", "243", GEM_ID)) %>%
    filter(!is.na(GEM_ID)) 
  
  row.names(nc.sids) <- nc.sids$GEM_ID
  
  nc.nb <- poly2nb(nc.sids, nc.sids$GEM_ID) 
  nb2INLA("Gemeinde_Inla", nc.nb)
  
  gemeinde.names <- poly2nb(nc.sids, nc.sids$GEM_ID) %>%
    attr("region.id") %>%
    as.data.frame() %>%
    dplyr::rename(GEM_ID = ".") %>%
    mutate(Region = 1:475) 
  
  
  dat.bern <- data_bern_wave%>%
    ungroup() %>%
    as.data.frame() %>%
    mutate(
      GEM_ID=as.character(GEM_ID)) %>%
    full_join(gemeinde.names) %>%
    filter(Wave == Wave_Inf) %>%
    dplyr::group_by( GEM_ID, Population, Region, Wave) %>%
    summarise(Num = sum(NumbCases)) %>%
    ungroup() %>%
    filter(!is.na(Region))
  # 
  # 
  # obs <- dat.bern %>%
  #   select(Year, GEM_ID, Num, Population) %>%
  #   dplyr::mutate(GEM_ID = as.character(GEM_ID)) %>%
  #   dplyr::group_by(Year, GEM_ID, Population) %>%
  #   summarise(Obs = sum(Num))
  
  control.family <- inla.set.control.family.default()
  
  formula <- Num ~ 1 + offset(log(Population)) +
    # f(week, model='rw1', constr = TRUE, scale.model = TRUE, cyclic = TRUE) +
    f(Region, model="bym", graph="Gemeinde_Inla", scale.model = TRUE)

  
  set.seed(20220421)
  inla.mod <- inla(formula,
                   data=dat.bern,
                   # family="nbinomial",
                   family = "zeroinflatednbinomial0",
                   # family = "zeroinflatednbinomial1",
                   #verbose = TRUE,
                   control.family = control.family,
                   # control.compute = list(config = TRUE, dic=TRUE, waic=TRUE, cpo=TRUE,return.marginals.predictor=TRUE),
                   control.compute = list(config = TRUE),
                   control.mode = list(restart = TRUE),
                   # num.threads = round(parallel::detectCores() * .2),
                   control.predictor = list(compute = TRUE, link = 1))

  
  tmarg <- function(marginals) {
    post.means <- mclapply(marginals, function (marg) {
      # Transform post. marginals
      aux <- inla.tmarginal(exp, marg)
      # Compute posterior mean
      inla.emarginal(function(x) x, aux)
    })
    
    return(as.vector(unlist(post.means)))
  }
  
  # Add posterior means to the SpatialPolygonsDataFrame
  
  inla.mod$IID <- tmarg(inla.mod$marginals.fitted.values)
  
  # dat.bern$LER <- inla.tmarginal(inla.mod$marginals.fitted.values)
  
  post.samples <- inla.posterior.sample(n = 1000, result = inla.mod,seed=20220421)
  predlist <- do.call(cbind, lapply(post.samples, function(X)
    exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
  
  rate.drawsMed<-array(unlist( predlist), dim=c(dim(dat.bern)[1], 1000)); dim(rate.drawsMed) 
  dM = as.data.frame(rate.drawsMed)
  # Add to the data and save
  Data= cbind(dat.bern,dM)
  
  mean.samples <- Data %>%
    ungroup() %>%
    dplyr::select(starts_with("V"),"Wave", "Region", "Num","Population") %>%
    # group_by(Region) %>%
    # summarise_all(sum) %>%
    rowwise(Region) %>% 
    dplyr::mutate(fit = median(dplyr::c_across(V1:V1000), na.rm=TRUE),
           LL = quantile(dplyr::c_across(V1:V1000), probs= 0.025,na.rm=TRUE),
           UL = quantile(dplyr::c_across(V1:V1000), probs= 0.975,na.rm=TRUE)) %>%
    select(Region, fit, LL, UL, Num, Population, Wave) %>%
    arrange(Region) %>%
    # left_join(dat.bern, by=c("Region")) %>%
    left_join(  gemeinde.names ) %>%
    # left_join(obs) %>%
    filter(!is.na(Region))
    
  write.xlsx(mean.samples,paste0("data/fitted_values_", Wave_Inf,".xlsx"), row.names=FALSE, overwrite = TRUE)
  save(mean.samples,file=paste0("data/fitted_values_", Wave_Inf,".RData"))
  
}


function_inla_fitted(Wave=1)
function_inla_fitted(Wave=2)