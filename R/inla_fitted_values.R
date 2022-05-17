function_inla_fitted <- function(){
  
  load("data/data_bern.RData")
  

  
  nc.sids  <- sf::st_read("data_raw/Gemeindegeometrie/Gemeinden_BE_1918.shp") %>%
    filter(!is.na(GEM_ID))
  
  
  row.names(nc.sids) <- nc.sids$GEM_ID
  
  nc.nb <- poly2nb(nc.sids, nc.sids$GEM_ID) 
  nb2INLA("Gemeinde_Inla", nc.nb)
  
  gemeinde.names <- poly2nb(nc.sids, nc.sids$Gemeinde) %>%
    attr("region.id") %>%
    as.data.frame() %>%
    dplyr::rename(GEM_ID = ".") %>%
    mutate(Region = 1:474) 
  
  
  dat.bern <- data_bern %>%
    mutate(GEM_ID=as.character(GEM_ID)) %>%
    full_join(gemeinde.names) %>%
    filter(Year==1924) %>%
    dplyr::group_by(Year, GEM_ID, Population, Region) %>%
    summarise(Num = sum(NumbCases))
  
  obs <- dat.bern %>%
    select(Year, GEM_ID, Num, Population) %>%
    dplyr::mutate(GEM_ID = as.character(GEM_ID)) %>%
    dplyr::group_by(Year, GEM_ID, Population) %>%
    summarise(Obs = sum(Num))
  
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
                   control.compute = list(config = TRUE, dic=TRUE, waic=TRUE, cpo=TRUE),
                   control.mode = list(restart = TRUE),
                   # num.threads = round(parallel::detectCores() * .2),
                   control.predictor = list(compute = TRUE, link = 1))
  
  # dat.bern$LER <- inla.tmarginal(inla.mod$marginals.fitted.values)
  
  post.samples <- inla.posterior.sample(n = 1000, result = inla.mod, seed=20220421)
  predlist <- do.call(cbind, lapply(post.samples, function(X)
    exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
  
  rate.drawsMed<-array(unlist( predlist), dim=c(dim(dat.bern)[1], 1000)); dim(rate.drawsMed) 
  dM = as.data.frame(rate.drawsMed)
  # Add to the data and save
  Data= cbind(dat.bern,dM)
  
  mean.samples <- Data %>%
    ungroup() %>%
    dplyr::select(starts_with("V"), "Region", "Num") %>%
    # group_by(Region) %>%
    # summarise_all(sum) %>%
    rowwise(Region) %>% 
    dplyr::mutate(fit = median(dplyr::c_across(V1:V1000)),
           LL = quantile(dplyr::c_across(V1:V1000), probs= 0.025),
           UL = quantile(dplyr::c_across(V1:V1000), probs= 0.975)) %>%
    select(Region, fit, LL, UL, Num) %>%
    arrange(Region) %>%
    # left_join(dat.bern, by=c("Region")) %>%
    left_join(  gemeinde.names ) %>%
    left_join(obs) %>%
    mutate(Inc= fit/Population*10000,
      SIR = Obs/fit)
  
  
}