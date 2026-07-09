#Penalized spline regression taken from https://bragqut.github.io/2016/05/24/samclifford-splines/
bspline <- function(x, K, bdeg=3, cyclic=FALSE, xl=min(x), xr=max(x)){
  x <- as.matrix(x,ncol=1)
  
  ndx <- K - bdeg
  
  # as outlined in Eilers and Marx (1996)
  dx <- (xr - xl) / ndx
  t <- xl + dx * (-bdeg:(ndx+bdeg))
  T <- (0 * x + 1) %*% t
  X <- x %*% (0 * t + 1)
  P <- (X - T) / dx
  B <- (T <= X) & (X < (T + dx))
  r <- c(2:length(t), 1)
  
  for (k in 1:bdeg){
    B <- (P * B + (k + 1 - P) * B[ ,r]) / k; 
  }
  
  B <- B[,1:(ndx+bdeg)]
  
  if (cyclic == 1){
    for (i in 1:bdeg){
      B[ ,i] <- B[ ,i] + B[ ,K-bdeg+i]    
    }
    B <- B[ , 1:(K-bdeg)]
  }
  
  return(B)
}



X <- bspline(1:24, 11, xl=1, xr=24)

makeQ = function(degree, K, epsilon=1e-3){
  x <- diag(K)
  E <- diff(x, differences=degree)
  return( t(E) %*% E + x*epsilon)
} 

long <- function(df){
  t(df) %>%
    as.data.frame() %>%
    setNames(unique(H_ayg$area)) %>%
    mutate(year = unique(Hhat_ay$year)) %>%
    pivot_longer(-year, names_to = "area", values_to = "val") %>%
    mutate(val = round(val, 2)) %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))
}

tableH <- function(mean, sd, lb, ub){
  mean %>%
    left_join(sd, c("year", "area")) %>%
    left_join(lb, c("year", "area")) %>% 
    left_join(ub, c("year", "area")) %>%
    rename(mean = val.x,
           sd = val.y,
           q2.5 = val.x.x,
           q97.5 = val.y.y)
}

get_Rhat <- function(post, cutoff = 1.1){
  list(
    data.frame("Rhat" = post$summary[, "Rhat"][post$summary[, "Rhat"] > cutoff & !is.na(post$summary[, "Rhat"])]),
    "R^ quantiles" = quantile(post$summary[, "Rhat"], probs = seq(0.9, 1, by = .01), na.rm = TRUE))
}

#-------------------------------------------------------------------------------
# Function to compare model results

modelcomp <- function(modlist,start_yr,Y=48){
  for (i in 1:length(modlist)){ #i <- 1
    postH <- modlist[[i]]
    
    #start_yr <- ifelse(names(modlist[i])== "Historical",1977,2020)
    
    #  if (names(modlist[i])== "Historical"){
    #    list2env(readinData_alt(spl_knts = 7,
    #                                     start_yr = start_yr,
    #                                     end_yr = end_yr,
    #                                     SE06 = "exclude"), #SE06 = "exclude"
    #             .GlobalEnv)
    #  } 
    
    #  if (names(modlist[i])== "Contemporary_a1"){
    #    list2env(readinData_alt(spl_knts = 7,
    #                            start_yr = start_yr,
    #                            end_yr = end_yr,
    #                            SE06 = "exclude"), #SE06 = "exclude"
    #             .GlobalEnv)
    #  }
    
    #  if (names(modlist[i])== "Contemporary_a2"){
    #    list2env(readinData_alt(spl_knts = 4,
    #                            start_yr = start_yr,
    #                            end_yr = end_yr,
    #                            SE06 = "exclude"), #SE06 = "exclude"
    #             .GlobalEnv)
    #  }
    
    as.data.frame(
      rbind(t(postH$q50$H_ayg),
            t(postH$q50$H_ayu),
            t(postH$q50$H_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(as.data.frame(
        rbind(t(postH$q2.5$H_ayg),
              t(postH$q2.5$H_ayu),
              t(postH$q2.5$H_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      left_join(as.data.frame(
        rbind(t(postH$q97.5$H_ayg),
              t(postH$q97.5$H_ayu),
              t(postH$q97.5$H_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      left_join(as.data.frame(
        rbind(t(postH$sd$H_ayg),
              t(postH$sd$H_ayu),
              t(postH$sd$H_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      mutate(area = toupper(as.character(area))) %>% #filter(!is.na(area)) %>%
      mutate(model = names(modlist[i])) -> temp
    
    unique(temp$area)
    
    if (i == 1){
      rf_plotdat = temp
    } else {
      rf_plotdat = rbind(rf_plotdat,temp)
    }
    
    as.data.frame(
      rbind(t(postH$q50$Hb_ayg),
            t(postH$q50$Hb_ayu),
            t(postH$q50$Hb_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(as.data.frame(
        rbind(t(postH$q2.5$Hb_ayg),
              t(postH$q2.5$Hb_ayu),
              t(postH$q2.5$Hb_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      left_join(as.data.frame(
        rbind(t(postH$q97.5$Hb_ayg),
              t(postH$q97.5$Hb_ayu),
              t(postH$q97.5$Hb_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      left_join(as.data.frame(
        rbind(t(postH$sd$Hb_ayg),
              t(postH$sd$Hb_ayu),
              t(postH$sd$Hb_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      mutate(area = toupper(as.character(area))) %>% #filter(!is.na(area)) %>%
      mutate(model = names(modlist[i])) -> brftemp
    
    unique(brftemp$area)
    
    # brftemp %>% filter(model == "Contemporary_a2",
    #                         year == "2020",
    #                         area == "CI")
    
    if (i == 1){
      brf_plotdat = brftemp
    } else {
      brf_plotdat = rbind(brf_plotdat,brftemp)
    }
    
    as.data.frame(
      rbind(t(postH$q50$Hy_ayg),
            t(postH$q50$Hy_ayu),
            t(postH$q50$Hy_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(as.data.frame(
        rbind(t(postH$q2.5$Hy_ayg),
              t(postH$q2.5$Hy_ayu),
              t(postH$q2.5$Hy_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      left_join(as.data.frame(
        rbind(t(postH$q97.5$Hy_ayg),
              t(postH$q97.5$Hy_ayu),
              t(postH$q97.5$Hy_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      left_join(as.data.frame(
        rbind(t(postH$sd$Hy_ayg),
              t(postH$sd$Hy_ayu),
              t(postH$sd$Hy_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      mutate(area = toupper(as.character(area))) %>% #filter(!is.na(area)) %>%
      mutate(model = names(modlist[i])) -> yetemp
    
    unique(yetemp$area)
    
    if (i == 1){
      ye_plotdat = yetemp
    } else {
      ye_plotdat = rbind(ye_plotdat,yetemp)
    }
    
    as.data.frame(
      rbind(t(postH$q50$Hd_ayg),
            t(postH$q50$Hd_ayu),
            t(postH$q50$Hd_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(as.data.frame(
        rbind(t(postH$q2.5$Hd_ayg),
              t(postH$q2.5$Hd_ayu),
              t(postH$q2.5$Hd_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      left_join(as.data.frame(
        rbind(t(postH$q97.5$Hd_ayg),
              t(postH$q97.5$Hd_ayu),
              t(postH$q97.5$Hd_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      left_join(as.data.frame(
        rbind(t(postH$sd$Hd_ayg),
              t(postH$sd$Hd_ayu),
              t(postH$sd$Hd_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      mutate(area = toupper(as.character(area))) %>% #filter(!is.na(area)) %>%
      mutate(model = names(modlist[i])) -> dsrtemp
    
    if (i == 1){
      dsr_plotdat = dsrtemp
    } else {
      dsr_plotdat = rbind(dsr_plotdat,dsrtemp)
    }
    
    as.data.frame(
      rbind(t(postH$q50$Hs_ayg),
            t(postH$q50$Hs_ayu),
            t(postH$q50$Hs_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(as.data.frame(
        rbind(t(postH$q2.5$Hs_ayg),
              t(postH$q2.5$Hs_ayu),
              t(postH$q2.5$Hs_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      left_join(as.data.frame(
        rbind(t(postH$q97.5$Hs_ayg),
              t(postH$q97.5$Hs_ayu),
              t(postH$q97.5$Hs_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      left_join(as.data.frame(
        rbind(t(postH$sd$Hs_ayg),
              t(postH$sd$Hs_ayu),
              t(postH$sd$Hs_ay))) %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(start_yr:end_yr, times = 3),
                 user = rep(c("guided", "unguided", "All"), each = Y)) %>%
          pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
        by = c("year","user","area")) %>%
      mutate(area = toupper(as.character(area))) %>% #filter(!is.na(area)) %>%
      mutate(model = names(modlist[i])) -> sltemp
    
    if (i == 1){
      sl_plotdat = sltemp
    } else {
      sl_plotdat = rbind(sl_plotdat,sltemp)
    }
    
    pHpel_mod_tmp <- 
      rbind(postH$mean$pH[,,1,1] %>% t(),
            postH$mean$pH[,,2,1] %>% t()) %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
             source = "model") %>%
      pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(rbind(postH$q2.5$pH[,,1,1] %>% t(),
                      postH$q2.5$pH[,,2,1] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2), #year = rep(unique(Hhat_ay$year), times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr)), #each = length(unique(Hhat_ay$year)))
                         source = "model") %>%
                  pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user","source")) %>%
      left_join(rbind(postH$q97.5$pH[,,1,1] %>% t(),
                      postH$q97.5$pH[,,2,1] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
                         source = "model") %>%
                  pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user","source")) %>%
      mutate(model = names(modlist[i]))
    
    unique(pHpel_mod_tmp$area)
    
    if (i == 1){
      pHpel_mod = pHpel_mod_tmp
    } else {
      pHpel_mod = rbind(pHpel_mod,pHpel_mod_tmp)
    }
    
    pHye_mod_temp <- 
      rbind(postH$mean$pH[,,1,2] %>% t(),
            postH$mean$pH[,,2,2] %>% t()) %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
             source = "model") %>%
      pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(rbind(postH$q2.5$pH[,,1,2] %>% t(),
                      postH$q2.5$pH[,,2,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
                         source = "model") %>%
                  pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user","source")) %>%
      left_join(rbind(postH$q97.5$pH[,,1,2] %>% t(),
                      postH$q97.5$pH[,,2,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
                         source = "model") %>%
                  pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user","source")) %>%
      mutate(model = names(modlist[i]))
    
    unique(pHye_mod_temp$area)
    
    if (i == 1){
      pHye_mod = pHye_mod_temp
    } else {
      pHye_mod = rbind(pHye_mod,pHye_mod_temp)
    }
    
    pHnpny_mod_tmp <- 
      rbind(postH$mean$pH[,,1,3] %>% t(),
            postH$mean$pH[,,2,3] %>% t()) %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
             source = "model") %>%
      pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(rbind(postH$q2.5$pH[,,1,3] %>% t(),
                      postH$q2.5$pH[,,2,3] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
                         source = "model") %>%
                  pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user","source")) %>%
      left_join(rbind(postH$q97.5$pH[,,1,3] %>% t(),
                      postH$q97.5$pH[,,2,3] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
                         source = "model") %>%
                  pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user","source")) %>%
      mutate(model = names(modlist[i]))
    
    unique(pHye_mod_temp$area)
    
    if (i == 1){
      pHnpny_mod = pHnpny_mod_tmp
    } else {
      pHnpny_mod = rbind(pHnpny_mod,pHnpny_mod_tmp)
    }
    
    pHdsr_mod_tmp <- 
      rbind(postH$mean$pH[,,1,4] %>% t(),
            postH$mean$pH[,,2,4] %>% t()) %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
             source = "model") %>%
      pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(rbind(postH$q2.5$pH[,,1,4] %>% t(),
                      postH$q2.5$pH[,,2,4] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
                         source = "model") %>%
                  pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user","source")) %>%
      left_join(rbind(postH$q97.5$pH[,,1,4] %>% t(),
                      postH$q97.5$pH[,,2,4] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
                         source = "model") %>%
                  pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user","source")) %>%
      filter(area %in% c("CSEO","EWYKT","NSEI","NSEO","SSEI","SSEO")) %>%
      mutate(model = names(modlist[i]))
    
    unique(pHye_mod_temp$area)
    
    if (i == 1){
      pHdsr_mod = pHdsr_mod_tmp
    } else {
      pHdsr_mod = rbind(pHdsr_mod,pHdsr_mod_tmp)
    }
    
    pHslope_mod_tmp <- 
      rbind(postH$mean$pH[,,1,5] %>% t(),
            postH$mean$pH[,,2,5] %>% t()) %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
             source = "model") %>%
      pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(rbind(postH$q2.5$pH[,,1,5] %>% t(),
                      postH$q2.5$pH[,,2,5] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
                         source = "model") %>%
                  pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user","source")) %>%
      left_join(rbind(postH$q97.5$pH[,,1,5] %>% t(),
                      postH$q97.5$pH[,,2,5] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
                         source = "model") %>%
                  pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user","source")) %>%
      filter(area %in% c("CSEO","EWYKT","NSEI","NSEO","SSEI","SSEO")) %>%
      mutate(model = names(modlist[i]))
    
    unique(pHye_mod_temp$area)
    
    if (i == 1){
      pHslope_mod = pHslope_mod_tmp
    } else {
      pHslope_mod = rbind(pHslope_mod,pHslope_mod_tmp)
    }
    
    p_pelagic_tmp <- 
      rbind(postH$mean$p_pelagic[,,1] %>% t(),
            postH$mean$p_pelagic[,,2] %>% t()) %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
      pivot_longer(-c(year, user), names_to = "area", values_to = "p_pelagic")  %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(rbind(postH$q2.5$p_pelagic[,,1] %>% t(),
                      postH$q2.5$p_pelagic[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      left_join(rbind(postH$q97.5$p_pelagic[,,1] %>% t(),
                      postH$q97.5$p_pelagic[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      mutate(model = names(modlist[i]))
    
    unique(p_pelagic_tmp$area)
    
    if (i == 1){
      p_pelagic = p_pelagic_tmp
    } else {
      p_pelagic = rbind(p_pelagic,p_pelagic_tmp)
    }
    
    p_black_tmp <- 
      rbind(postH$mean$p_black[,,1] %>% t(),
            postH$mean$p_black[,,2] %>% t()) %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
             source = "model") %>%
      pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_black")  %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(rbind(postH$q2.5$p_black[,,1] %>% t(),
                      postH$q2.5$p_black[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      left_join(rbind(postH$q97.5$p_black[,,1] %>% t(),
                      postH$q97.5$p_black[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      mutate(model = names(modlist[i]))
    
    unique(p_pelagic_tmp$area)
    
    if (i == 1){
      p_black = p_black_tmp
    } else {
      p_black = rbind(p_black,p_black_tmp)
    }
    
    p_yellow_tmp <- 
      rbind(postH$mean$p_yellow[,,1] %>% t(),
            postH$mean$p_yellow[,,2] %>% t()) %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
             source = "model") %>%
      pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_yellow")  %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(rbind(postH$q2.5$p_yellow[,,1] %>% t(),
                      postH$q2.5$p_yellow[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      left_join(rbind(postH$q97.5$p_yellow[,,1] %>% t(),
                      postH$q97.5$p_yellow[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      left_join(rbind(postH$q50$p_yellow[,,1] %>% t(),
                      postH$q50$p_yellow[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "med_p")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      mutate(model = names(modlist[i]))
    
    unique(p_pelagic_tmp$area)
    
    if (i == 1){
      p_yellow = p_yellow_tmp
    } else {
      p_yellow = rbind(p_yellow,p_yellow_tmp)
    }
    
    p_dsr_tmp <- 
      rbind(postH$mean$p_dsr[,,1] %>% t(),
            postH$mean$p_dsr[,,2] %>% t()) %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
             source = "model") %>%
      pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_dsr")  %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(rbind(postH$q2.5$p_dsr[,,1] %>% t(),
                      postH$q2.5$p_dsr[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      left_join(rbind(postH$q97.5$p_dsr[,,1] %>% t(),
                      postH$q97.5$p_dsr[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      left_join(rbind(postH$q50$p_dsr[,,1] %>% t(),
                      postH$q50$p_dsr[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "med_p")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      mutate(model = names(modlist[i]))
    
    if (i == 1){
      p_dsr = p_dsr_tmp
    } else {
      p_dsr = rbind(p_dsr,p_dsr_tmp)
    }
    
    p_slope_tmp <- 
      rbind(postH$mean$p_slope[,,1] %>% t(),
            postH$mean$p_slope[,,2] %>% t()) %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             user = rep(c("charter", "private"), each = length(start_yr:end_yr)),
             source = "model") %>%
      pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_slope")  %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
      left_join(rbind(postH$q2.5$p_slope[,,1] %>% t(),
                      postH$q2.5$p_slope[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      left_join(rbind(postH$q97.5$p_slope[,,1] %>% t(),
                      postH$q97.5$p_slope[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      left_join(rbind(postH$q50$p_slope[,,1] %>% t(),
                      postH$q50$p_slope[,,2] %>% t()) %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = rep(start_yr:end_yr, times = 2),
                         user = rep(c("charter", "private"), each = length(start_yr:end_yr))) %>%
                  pivot_longer(-c(year, user), names_to = "area", values_to = "med_p")  %>%
                  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
                by = c("year","area","user")) %>%
      mutate(model = names(modlist[i]))
    
    if (i == 1){
      p_slope = p_slope_tmp
    } else {
      p_slope = rbind(p_slope,p_slope_tmp)
    }
    
    pG_mod <- 
      postH$mean$pG %>%
      t() %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = unique(start_yr:end_yr),
             source = "model") %>%
      pivot_longer(-c(year, source), names_to = "area", values_to = "pG") %>%
      left_join(postH$q2.5$pG %>%
                  t() %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = unique(start_yr:end_yr),
                         source = "model") %>%
                  pivot_longer(-c(year, source), names_to = "area", values_to = "pG_lo95"),
                by = c("year","source","area")) %>%
      left_join(postH$q97.5$pG %>%
                  t() %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = unique(start_yr:end_yr),
                         source = "model") %>%
                  pivot_longer(-c(year, source), names_to = "area", values_to = "pG_hi95"),
                by = c("year","source","area")) %>%
      mutate(model = names(modlist[i]))
    
    if (i == 1){
      pG = pG_mod
    } else {
      pG = rbind(pG,pG_mod)
    }
    
    bc_mod2 <- 
      #apply(exp(postH$sims.list$logbc_H), c(2, 3), mean) %>%
      exp(postH$q50$logbc_H) %>%
      t() %>%
      as.data.frame() %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = unique(start_yr:end_yr),
             source = "model") %>%
      pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
      mutate(data = "H") %>%
      rbind(exp(postH$q50$logbc_R) %>%
              t() %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = unique(start_yr:end_yr),
                     source = "model") %>%
              pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
              mutate(data = "R")) %>%
      left_join(exp(postH$q2.5$logbc_H) %>%
                  t() %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = unique(start_yr:end_yr),
                         source = "model") %>%
                  pivot_longer(-c(year, source), names_to = "area", values_to = "bc_lo95") %>%
                  mutate(data = "H") %>%
                  rbind(exp(postH$q2.5$logbc_R) %>%
                          t() %>%
                          as.data.frame() %>%
                          setNames(nm = unique(H_ayg$area)) %>%
                          mutate(year = unique(start_yr:end_yr),
                                 source = "model") %>%
                          pivot_longer(-c(year, source), names_to = "area", values_to = "bc_lo95") %>%
                          mutate(data = "R")),
                by = c("year","area","data","source")) %>%
      left_join(exp(postH$q97.5$logbc_H) %>%
                  t() %>%
                  as.data.frame() %>%
                  setNames(nm = unique(H_ayg$area)) %>%
                  mutate(year = unique(start_yr:end_yr),
                         source = "model") %>%
                  pivot_longer(-c(year, source), names_to = "area", values_to = "bc_hi95") %>%
                  mutate(data = "H") %>%
                  rbind(exp(postH$q97.5$logbc_R) %>%
                          t() %>%
                          as.data.frame() %>%
                          setNames(nm = unique(H_ayg$area)) %>%
                          mutate(year = unique(start_yr:end_yr),
                                 source = "model") %>%
                          pivot_longer(-c(year, source), names_to = "area", values_to = "bc_hi95") %>%
                          mutate(data = "R")),
                by = c("year","area","data","source")) %>%
      mutate(model = names(modlist[i]))
    
    if (i == 1){
      bias_ests = bc_mod2
    } else {
      bias_ests = rbind(bias_ests,bc_mod2)
    }
  }
  results <- list(rf_plotdat=rf_plotdat,
                  brf_plotdat=brf_plotdat,
                  ye_plotdat = ye_plotdat,
                  dsr_plotdat = dsr_plotdat,
                  sl_plotdat = sl_plotdat,
                  pHpel_mod = pHpel_mod,
                  pHye_mod = pHye_mod,
                  pHnpny_mod = pHnpny_mod,
                  pHdsr_mod = pHdsr_mod,
                  pHslope_mod = pHslope_mod,
                  p_pelagic = p_pelagic,
                  p_black = p_black,
                  p_yellow = p_yellow,
                  p_dsr = p_dsr,
                  p_slope = p_slope,
                  pG = pG,
                  bias_ests = bias_ests)
  return(results)
}


