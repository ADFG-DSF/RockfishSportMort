jags_params <- function(){
  params <- c(
    #SWHS bias; assumed same for R and H
    "logbc_H", "mu_bc_H", "sd_bc_H",
    "logbc_R", "mu_bc_R", "sd_bc_R",
    #"bc_R_offset","mu_bc_R_offset","sd_bcRoff",
    
    #User proportions (proportion guided); different for H and R
    "pG", "b1_pG", "b2_pG",
    
    #proportion harvested: 
    "pH", "tau_pH",
    "mu_beta4_pH","tau_beta4_pH",
    "mu_beta5_pH","tau_beta5_pH",
    "beta4_pH","beta5_pH",
    
    #random effects on pH
    "re_pH", 
    "sd_pH", "mu_pH",
    "eps_pH","mean_eps_pH",
    "sdR","mu_sdR","tau_sdR",
    "sig_a","re_area",
    "sig_a_sl","re_area_sl",
    "sig_a_dsr","re_area_dsr",
    "mu_sl","p_obs_sl",
    "mu_dsr","p_obs_dsr",
    "yreps","yrepsl","yrepdsr",
    
    #private:guided release ratio prior beta4 yadda yadda
    "mu_prigui","tau_prigui_pre","tau_prigui",
    
    #Species apportionment of harvests
    "p_pelagic", 
    "beta4_pelagic", "beta5_pelagic", 
    "mu_beta4_pelagic", "tau_beta4_pelagic",
    "mu_beta5_pelagic", "tau_beta5_pelagic",
    "mu_beta4_pelagic_kod", "tau_beta4_pelagic_kod",
    "mu_beta5_pelagic_kod", "tau_beta5_pelagic_kod",
    "p_yellow",  
    "beta4_yellow","beta5_yellow",
    "mu_beta4_yellow", "tau_beta4_yellow",
    "mu_beta5_yellow", "tau_beta5_yellow",
    "mu_beta4_yellow_kod", "tau_beta4_yellow_kod",
    "mu_beta5_yellow_kod", "tau_beta5_yellow_kod",
    "p_black", 
    "beta4_black","beta5_black",
    "mu_beta4_black", "tau_beta4_black",
    "mu_beta5_black", "tau_beta5_black",
    "mu_beta4_black_kod", "tau_beta4_black_kod",
    "mu_beta5_black_kod", "tau_beta5_black_kod",
    "p_dsr", "beta4_dsr",
    "beta5_dsr",
    "p_slope", 
    "beta4_slope","beta5_slope",
    
    #random effects on species
    "re_pelagic", "re_black","re_yellow","re_dsr","re_slope","re_rslope",
    "eps_pel", "eps_bl","eps_ye","eps_dsr","eps_sl",
    "mean_eps_pel", "mean_eps_bl","mean_eps_ye","mean_eps_dsr","mean_eps_sl",
    "sd_comp", "tau_comp","re_comp",
    
    #harvest estimates and spline parts
    "Htrend_ay", "H_ay", "sigma_H", "lambda_H", "H_ayg", "H_ayu", 
    "Hp_ayg", "Hp_ayu", "Hp_ay",
    "Hb_ayg", "Hb_ayu", "Hb_ay",
    "Hy_ayg", "Hy_ayu", "Hy_ay",
    "Hd_ayg", "Hd_ayu", "Hd_ay",
    "Hdnye_ayg", "Hdnye_ayu", "Hdnye_ay",
    "Hs_ayg", "Hs_ayu", "Hs_ay",
    "Ho_ayg", "Ho_ayu", "Ho_ay",
    "logHhat_ay",
    #with hierarchichal spline lambda
    "mu_lambda_H","sigma_lambda_H","beta_H","beta0_H",
    "Hhat_ay","logH_ay","beta_H","sigma_H","lambda_H",

    #releases
    "logRhat_ay","logRhat_ayg",
    "R_ay", "R_ayg", "R_ayu", 
    "Rp_ayg", "Rp_ayu", "Rp_ay",
    "Rb_ayg", "Rb_ayu", "Rb_ay",
    "Ry_ayg", "Ry_ayu", "Ry_ay",
    "Ro_ayg", "Ro_ayu", "Ro_ay",
    "Rdnye_ayg", "Rdnye_ayu", "Rdnye_ay",
    "Rd_ayg", "Rd_ayu", "Rd_ay",
    "pDSR_YE_ayg","pDSR_YE_ayu","pDSR_YE_ay",
    "Rs_ayg", "Rs_ayu", "Rs_ay",
    "pR_dsr","pr_slope", "b1_pRslope","b2_pRslope",
    "nonrecR_ayg",
    
    #Kodiak hydroacoustic
    "mu_kap","sd_kap","kap",
    
    #weight
    "mu_wt","sd_wt","mu_ay_wt",
    "mu2_wt","sd2_wt","sd_a_wt","mu_a_wt",
    "mu3_wt","sd3_wt","sd_r_wt","mu_r_wt",
    "mu4_wt","sd4_wt","sd_sp_wt",
    "wt",
    "swt",
    "wt_user","wt_user_reg","sd_wt_user",
    
    #release mortality
    "Rp_ayg_mort","Rp_ayu_mort","Rp_ay_mort",
    "Rb_ayg_mort","Rb_ayu_mort","Rb_ay_mort",
    "Ry_ayg_mort","Ry_ayu_mort","Ry_ay_mort",
    "Rdnye_ayg_mort","Rdnye_ayu_mort","Rdnye_ay_mort",
    "Rd_ayg_mort","Rd_ayu_mort","Rd_ay_mort",
    "Rs_ayg_mort","Rs_ayu_mort","Rs_ay_mort",
    
    #total mortality
    "Tp_ayg","Tp_ayu","Tp_ay",
    "Tb_ayg","Tb_ayu","Tb_ay",
    "Ty_ayg","Ty_ayu","Ty_ay",
    "Tdnye_ayg","Tdnye_ayu","Tdnye_ay",
    "Td_ayg","Td_ayu","Td_ay",
    "Ts_ayg","Ts_ayu","Ts_ay",
    
    #biomass conversions
    "Bp_ayg","Bp_ayu","Bp_ay",
    "Bb_ayg","Bb_ayu","Bb_ay",
    "By_ayg","By_ayu","By_ay",
    "Bdnye_ayg","Bdnye_ayu","Bdnye_ay",
    "Bd_ayg","Bd_ayu","Bd_ay",
    "Bs_ayg","Bs_ayu","Bs_ay",
    
    #params to save for use in contemporary model
    "log_prigui_true","mu_log","tau_between")
  
  return(params)
}