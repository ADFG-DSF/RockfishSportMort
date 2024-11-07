data {
    int<lower=1> A; // Areas
    int<lower=1> Y; // Years
    int<lower=1> C; // Spline components
    
    // Harvest and Catch Data
    matrix[A, Y] Hhat_ay;          // Estimated harvest by area and year
    matrix[A, Y] cvHhat_ay;        // Coefficients of variation for Hhat_ay
    matrix[A, Y] Chat_ay;          // Estimated catch by area and year
    matrix[A, Y] cvChat_ay;        // Coefficients of variation for Chat_ay
    
    // Logbook Data
    matrix[A, Y] Hlb_ayg;          // Harvest logbook data
    matrix[A, Y] Hlbp_ayg;         // Pelagic harvested by guides
    matrix[A, Y] Hlby_ayg;         // Yelloweye rockfish harvested by guides
    matrix[A, Y] Rlb_ayg;          // Released logbook data
    matrix[A, Y] Rlbp_ayg;         // Pelagic released by guides
    matrix[A, Y] Rlby_ayg;         // Yelloweye released by guides
    
    // SWHS Data
    matrix[A, Y] Hhat_ayg;         // SWHS estimates of rockfish harvests by area, year and user group
    matrix[A, Y] cvHhat_ayg;       // Coefficients of variation for Hhat_ayg
    matrix[A, Y] Chat_ayg;         // SWHS estimates of rockfish catches by area, year and user group
    matrix[A, Y] cvChat_ayg;       // Coefficients of variation for Chat_ayg
    
    // Spline Data
    matrix[Z, C] Z;                // Spline matrix
    matrix[Q, C] Q;                // Spline matrix component
    vector[C] zero;                // Zero vector
    
    // Composition Data
    int comp_area[N];              // Area for composition data
    int comp_year[N];              // Year for composition data
    int comp_user[N];              // User for composition data
    int comp_N[N];                 // Sample size Nayu
    int comp_pelagic[N];           // Indicator for pelagic
    int comp_black[N];             // Indicator for black rockfish
    int comp_yellow[N];            // Indicator for yelloweye rockfish
    
    int<lower=1> N;                // Number of composition data points
    int regions[16];               // Regional classification
}

parameters {
    // SWHS bias parameters
    real logbc;
    real mu_bc;
    real<lower=0> sd_bc;
    
    // User proportions for guided (H for harvest, C for catch)
    vector[A] pG_H;             // Proportion guided for harvest
    real b1_pG_H;               // Regression parameter for pG_H
    real b2_pG_H;               // Regression parameter for pG_H
    vector[A] pG_C;             // Proportion guided for catch
    real b1_pG_C;               // Regression parameter for pG_C
    real b2_pG_C;               // Regression parameter for pG_C
    
    // Proportion harvested
    vector[A] pH;               // Proportion harvested
    real pH_int;                // Intercept for pH
    real pH_slo;                // Slope for pH

    // Proportions for pelagic, yellow, black species
    vector[A] p_pelagic;        // Proportion pelagic
    real beta0_pelagic;         // Intercept pelagic
    real beta1_pelagic;         // Slope 1 pelagic
    real beta2_pelagic;         // Slope 2 pelagic
    real beta3_pelagic;         // Slope 3 pelagic
    real mu_beta0_pelagic;      // Mean of beta0_pelagic
    real<lower=0> tau_beta0_pelagic;  // Precision of beta0_pelagic
    
    vector[A] p_yellow;         // Proportion yelloweye
    real beta0_yellow;          // Intercept yelloweye
    real beta1_yellow;          // Slope 1 yelloweye
    real beta2_yellow;          // Slope 2 yelloweye
    real beta3_yellow;          // Slope 3 yelloweye
    real mu_beta0_yellow;       // Mean of beta0_yellow
    real<lower=0> tau_beta0_yellow;   // Precision of beta0_yellow
    
    vector[A] p_black;          // Proportion black
    real beta0_black;           // Intercept black
    real beta1_black;           // Slope 1 black
    real beta2_black;           // Slope 2 black
    real mu_beta0_black;        // Mean of beta0_black
    real<lower=0> tau_beta0_black;    // Precision of beta0_black

    // Random effects on species
    vector[A] re_pelagic;       // Random effect pelagic
    vector[A] re_black;         // Random effect black
    vector[A] re_yellow;        // Random effect yellow
    real<lower=0> sd_comp;      // Standard deviation for comp
    
    // Harvest estimates and spline parts
    matrix[A, Y] Htrend_ay;     // Harvest trend
    matrix[A, Y] H_ay;          // Harvest estimates
    real<lower=0> sigma_H;      // Standard deviation for H_ay
    real lambda_H;              // Smoothing parameter for H
    matrix[A, Y] H_ayg;         // Guided harvest estimates
    matrix[A, Y] H_ayu;         // Unguided harvest estimates
    matrix[A, Y] Hb_ayg;        // BRF guided harvest
    matrix[A, Y] Hb_ayu;        // BRF unguided harvest
    matrix[A, Y] Hb_ay;         // BRF harvest
    matrix[A, Y] Hy_ayg;        // YE guided harvest
    matrix[A, Y] Hy_ayu;        // YE unguided harvest
    matrix[A, Y] Hy_ay;         // YE harvest
    matrix[A, Y] logHhat_ay;    // Log of harvest estimates
    
    // Catch estimates and spline parts
    matrix[A, Y] Ctrend_ay;     // Catch trend
    matrix[A, Y] C_ay;          // Catch estimates
    real<lower=0> sigma_C;      // Standard deviation for C_ay
    real lambda_C;              // Smoothing parameter for C
    matrix[A, Y] C_ayg;         // Guided catch estimates
    matrix[A, Y] C_ayu;         // Unguided catch estimates
    matrix[A, Y] Cb_ayg;        // BRF guided catch
    matrix[A, Y] Cb_ayu;        // BRF unguided catch
    matrix[A, Y] Cb_ay;         // BRF catch
    matrix[A, Y] Cy_ayg;        // YE guided catch
    matrix[A, Y] Cy_ayu;        // YE unguided catch
    matrix[A, Y] Cy_ay;         // YE catch
    matrix[A, Y] logChat_ay;    // Log of catch estimates 
}

//transformed parameters {
//}

model {
  tau_comp <- 1 / sd_comp / sd_comp
  sd_comp ~ dunif(0, 10)
  lambda_bc ~ dgamma(0.001, 0.001)

  for(n in 1:N){ 
    comp_pelagic[n] ~ dbin(p_pelagic[comp_area[n], comp_year[n], comp_user[n] + 1], comp_N[n])
    comp_black[n] ~ dbin(p_black[comp_area[n], comp_year[n], comp_user[n] + 1], comp_pelagic[n])
    comp_yellow[n] ~ dbin(p_yellow[comp_area[n], comp_year[n], comp_user[n] + 1], comp_N[n] - comp_pelagic[n])
  }

  for(r in 1:3){
    mu_beta0_pelagic[r] ~ dnorm(0, 0.001)
    tau_beta0_pelagic[r] ~ dgamma(0.001, 0.001)
    mu_beta0_yellow[r] ~ dnorm(0, 0.001)
    tau_beta0_yellow[r] ~ dgamma(0.001, 0.001)
    mu_beta0_black[r] ~ dnorm(0, 0.001)
    tau_beta0_black[r] ~ dgamma(0.001, 0.001)
  }

  for (a in 1:A){
    mu_bc[a] ~ dnorm(0, 0.001)
    tau_bc[a] <- 1 / sd_bc[a] / sd_bc[a]
    sd_bc[a] ~ dunif(0, 10)

    b1_pG_H[a] ~ dunif(1, 50)
    b2_pG_H[a] ~ dunif(1, 50)
    pH_int[a] ~ dnorm(0, 0.001)
    pH_slo[a] ~ dnorm(0, 0.001)

    b1_pG_C[a] ~ dunif(1, 50)
    b2_pG_C[a] ~ dunif(1, 50)

    beta0_pelagic[a] ~ dnorm(mu_beta0_pelagic[regions[a]], tau_beta0_pelagic[regions[a]])
    beta1_pelagic[a] ~ dnorm(0, 0.001)
    beta2_pelagic[a] ~ dnorm(0, 0.001)

    beta0_yellow[a] ~ dnorm(mu_beta0_yellow[regions[a]], tau_beta0_yellow[regions[a]])
    beta1_yellow[a] ~ dnorm(0, 0.001)
    beta2_yellow[a] ~ dnorm(0, 0.001)
    beta3_yellow[a] ~ dnorm(0, 0.001) #allowing more curvature to p_yellow

    beta0_black[a] ~ dnorm(mu_beta0_black[regions[a]], tau_beta0_black[regions[a]])
    beta1_black[a] ~ dnorm(0, 0.001)
    beta2_black[a] ~ dnorm(0, 0.001)
    
    for(y in 22:Y){
      Hlb_ayg[a, y] ~ dpois(H_ayg[a, y])   
      Hlbp_ayg[a, y] ~ dpois(H_ayg[a, y] * p_pelagic[a, y, 1]) 
      Rlb_ayg[a, y] ~ dpois(R_ayg[a, y])   
      Rlbp_ayg[a, y] ~ dpois(R_ayg[a, y] * p_pelagic[a, y, 1])
    } 

    for(y in 30:Y){
      Hlby_ayg[a, y] ~ dpois(Hy_ayg[a, y])
    }

    for(y in 1:Y){
      H_ayg[a, y] <- H_ay[a, y] * pG_H[a, y]
      H_ayu[a, y] <- H_ay[a, y] * (1 - pG_H[a, y])
      H_ay[a, y] <- exp(logH_ay[a, y])

      C_ayg[a, y] <- C_ay[a, y] * pG_C[a, y]
      C_ayu[a, y] <- C_ay[a, y] * (1 - pG_C[a, y])
      C_ay[a, y] <- exp(logC_ay[a, y])

      R_ayg[a, y] <- C_ayg[a, y] - H_ayg[a, y]
      R_ayu[a, y] <- C_ayu[a, y] - H_ayu[a, y]
      R_ay[a, y] <- C_ay[a, y] - H_ay[a, y]

      Hb_ayg[a, y] <- H_ayg[a, y] * p_pelagic[a, y, 1] * p_black[a, y, 1]
      Hb_ayu[a, y] <- H_ayu[a, y] * p_pelagic[a, y, 2] * p_black[a, y, 2]
      Hb_ay[a, y] <- Hb_ayg[a, y] + Hb_ayu[a, y]

      Cb_ayg[a, y] <- C_ayg[a, y] * p_pelagic[a, y, 1] * p_black[a, y, 1]
      Cb_ayu[a, y] <- C_ayu[a, y] * p_pelagic[a, y, 2] * p_black[a, y, 2]
      Cb_ay[a, y] <- Cb_ayg[a, y] + Cb_ayu[a, y]

      Rb_ayg[a, y] <- Cb_ayg[a, y] - Hb_ayg[a, y]
      Rb_ayu[a, y] <- Cb_ayu[a, y] - Hb_ayu[a, y]
      Rb_ay[a, y] <- Cb_ay[a, y] - Hb_ay[a, y]

      Hy_ayg[a, y] <- H_ayg[a, y] * (1 - p_pelagic[a, y, 1]) * p_yellow[a, y, 1]
      Hy_ayu[a, y] <- H_ayu[a, y] * (1 - p_pelagic[a, y, 2]) * p_yellow[a, y, 2]
      Hy_ay[a, y] <- Hy_ayg[a, y] + Hy_ayu[a, y]

      Cy_ayg[a, y] <- C_ayg[a, y] * (1 - p_pelagic[a, y, 1]) * p_yellow[a, y, 1]
      Cy_ayu[a, y] <- C_ayu[a, y] * (1 - p_pelagic[a, y, 2]) * p_yellow[a, y, 2]
      Cy_ay[a, y] <- Cy_ayg[a, y] + Cy_ayu[a, y]

      Ry_ayg[a, y] <- Cy_ayg[a, y] - Hy_ayg[a, y]
      Ry_ayu[a, y] <- Cy_ayu[a, y] - Hy_ayu[a, y]
      Ry_ay[a, y] <- Cy_ay[a, y] - Hy_ay[a, y]
      
      logbc[a, y] ~ dnorm(mu_bc[a], tau_bc[a])
      pG_H[a, y] ~ dbeta(b1_pG_H[a], b2_pG_H[a])
      pG_C[a, y] ~ dbeta(b1_pG_C[a], b2_pG_C[a])

      tauHhat_ay[a, y] <- 1 / log(cvHhat_ay[a, y] * cvHhat_ay[a, y] + 1)
      Hhat_ay[a, y] ~ dlnorm(logHhat_ay[a, y], tauHhat_ay[a, y])
      logHhat_ay[a, y] <- logH_ay[a, y] + logbc[a, y]
      logH_ay[a, y] ~ dnorm(Htrend_ay[a, y], prec_H[a])
      Htrend_ay[a, y] <- Z[y, ] %*% beta_H[a, 1:C]

      tauChat_ay[a, y] <- 1 / log(cvChat_ay[a, y] * cvChat_ay[a, y] + 1)
      Chat_ay[a, y] ~ dlnorm(logChat_ay[a, y], tauChat_ay[a, y])
      logChat_ay[a, y] <- logC_ay[a, y] + logbc[a, y]
      logC_ay[a, y] ~ dnorm(Ctrend_ay[a, y], prec_C[a])
      Ctrend_ay[a, y] <- Z[y, ] %*% beta_C[a, 1:C]

      logit(pH[a, y]) <- pH_int[a] + pH_slo[a] * y

      for(u in 1:2){
        logit(p_pelagic[a, y, u]) <- beta0_pelagic[a] + beta1_pelagic[a] * pH[a, y] + beta2_pelagic[a] * pH[a, y] * pH[a, y] + re_pelagic[a, y]
        logit(p_yellow[a, y, u]) <- beta0_yellow[a] + beta1_yellow[a] * pH[a, y] + beta2_yellow[a] * pH[a, y] * pH[a, y] + re_yellow[a, y] + beta3_yellow[a] * pow(y, 2)
        ## Other yellow option: logit(p_yellow[a, y, u]) <- beta0_yellow[a] + (beta1_yellow[a] * y) / (1 + beta2_yellow[a] * y) + re_yellow[a, y, u]
        logit(p_black[a, y, u]) <- beta0_black[a] + beta1_black[a] * pH[a, y] + beta2_black[a] * pH[a, y] * pH[a, y] + re_black[a, y]
      }
    }
  }
}

   
generated quantities {
      matrix[A, Y] R_ayg;        // Base guided releases
      matrix[A, Y] R_ayu;        // Base unguided releases
      matrix[A, Y] R_ay;         // Base releases

      matrix[A, Y] Rb_ayg;        // BRF guided releases
      matrix[A, Y] Rb_ayu;        // BRF unguided releases
      matrix[A, Y] Rb_ay;         // BRF releases

      matrix[A, Y] Ry_ayg;        // YE guided releases
      matrix[A, Y] Ry_ayu;        // YE unguided releases
      matrix[A, Y] Ry_ay;         // YE releases

      for (a in 1:A) {
        for (y in 1:Y){
          R_ayg[a, y] <- C_ayg[a, y] - H_ayg[a, y]
          R_ayu[a, y] <- C_ayu[a, y] - H_ayu[a, y]
          R_ay[a, y] <- C_ay[a, y] - H_ay[a, y]

          Rb_ayg[a, y] <- Cb_ayg[a, y] - Hb_ayg[a, y]
          Rb_ayu[a, y] <- Cb_ayu[a, y] - Hb_ayu[a, y]
          Rb_ay[a, y] <- Cb_ay[a, y] - Hb_ay[a, y]

          Ry_ayg[a, y] <- Cy_ayg[a, y] - Hy_ayg[a, y]
          Ry_ayu[a, y] <- Cy_ayu[a, y] - Hy_ayu[a, y]
          Ry_ay[a, y] <- Cy_ay[a, y] - Hy_ay[a, y]
        }
      }
    
  }
  
  



