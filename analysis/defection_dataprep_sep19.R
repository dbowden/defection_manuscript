# Measures of repressive shock
# 
# 1. Change of X sd or pct in theta mean
# 2. EPR downgrade
# 3. Polity decrease
# 4. VDEM regime score decrease
# 5. VDEM torture/kill increase
# 6. NAVCO repression
# 7. Other events data?

# 0. Load packages

library(tidyverse)
library(lubridate)
library(readxl)
library(haven)
library(countrycode)
library(zoo)

# 1. Build a frame of country-years ----

library(states)

cyears <- state_panel(start=1946, end=2017, by="year", partial="any", useGW = T)

cyears$date <- year(cyears$date)

cyears$gwcode <- as.numeric(cyears$gwcode)

cyears <- rename(cyears, gwno_loc=gwcode, year=date)


# 2. My origins (v1) data ----

ucdp.dyad <- read_excel("~/OneDrive - PennO365/Civil War Data/milorigins/milorigins_base_coded.xlsx")

# expand to yearly
ucdp.yearly <- ucdp.dyad %>% 
  group_by(dyad_id) %>% 
  do(data.frame(year=seq(.$styear, .$eyear)))

ucdp.dyad <- left_join(ucdp.yearly, ucdp.dyad)
rm(ucdp.yearly)

ucdp.dyad$rebel_age <- ucdp.dyad$year - ucdp.dyad$styear

ucdp.dyad$new_mil <- ifelse(ucdp.dyad$milorigin_v1==1 & ucdp.dyad$rebel_age==0, 1, 0)
ucdp.dyad$new_civ <- ifelse(ucdp.dyad$origin=="civilian gov" & ucdp.dyad$rebel_age==0, 1, 0)

ucdp.dyad$gwno_loc <- as.numeric(ucdp.dyad$gwno_loc)

ucdp.dyad <- ucdp.dyad %>% 
  filter(!is.na(gwno_loc)) %>% 
  group_by(gwno_loc, year) %>% 
  summarize(new_mil=max(new_mil), new_civ=max(new_civ), non_mil=n_distinct(side_b_id[milorigin_v1==0]))

ucdp.dyad$civconflict <- 1

cyears <- left_join(cyears, ucdp.dyad)
rm(ucdp.dyad)

cyears$new_mil[is.na(cyears$new_mil)] <- 0
cyears$new_civ[is.na(cyears$new_civ)] <- 0
cyears$civconflict[is.na(cyears$civconflict)] <- 0
cyears$non_mil[is.na(cyears$non_mil)] <- 0

cyears$any_non_mil <- ifelse(cyears$non_mil>0, 1, 0)

cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  mutate(non_mil_lag=lag(non_mil))


# 3. FORGE data ----

forge <- read_excel("~/Data/Rebel Groups/forge_v1.0_public.xlsx")

forge <- rename(forge, gwno_loc=ccode, year=foundyear)

forge <- forge %>% 
  group_by(gwno_loc, year) %>% 
  summarize(forge_mil=max(preorgmil), forge_fmrmil=max(preorgfmr), forge_civ=max(preorggov))

cyears <- left_join(cyears, forge)
rm(forge)

cyears$forge_mil[is.na(cyears$forge_mil) & cyears$year < 2012] <- 0
cyears$forge_fmrmil[is.na(cyears$forge_fmrmil) & cyears$year < 2012] <- 0
cyears$forge_civ[is.na(cyears$forge_civ) & cyears$year < 2012] <- 0


# 4. Thyne and Powell coup data ----

thyne <- read_delim("~/Data/Coups/powell_thyne_ccode_year.txt", delim="\t")

# count total coups (1=unsuccessful, 2=successful)
thyne$thyne_failed <- rowSums(thyne[,3:6]==1)
thyne$thyne_success <- rowSums(thyne[,3:6]==2)
thyne$thyne_tot_attempts <- thyne$thyne_failed + thyne$thyne_success
thyne$thyne_attempt <- ifelse(thyne$thyne_tot_attempts > 0, 1, 0)

# remove unnecessary columns and merge
thyne <- select(thyne, gwno_loc=ccode, year, thyne_failed:thyne_attempt)

cyears <- left_join(cyears, thyne)
rm(thyne)

# convert NAs to 0s (result of some countries not being included in Thyne data)
cyears$thyne_attempt[is.na(cyears$thyne_attempt)] <- 0
cyears$thyne_success[is.na(cyears$thyne_success)] <- 0
cyears$thyne_tot_attempts[is.na(cyears$thyne_tot_attempts)] <- 0
cyears$thyne_failed[is.na(cyears$thyne_failed)] <- 0

# 5. CSP Coup data ----

csp <- read_excel("~/Data/Coups/CSPCoupsAnnualv2018.xls")

# limit to necessary columns
csp <- select(csp, gwno_loc=ccode, year, csp_success=scoup1, csp_failed=atcoup2, csp_plot=pcoup3, csp_alleged=apcoup4)

# summary counts
csp$csp_tot_attempts <- csp$csp_success + csp$csp_failed
csp$csp_attempt <- ifelse(csp$csp_tot_attempts > 0, 1, 0)

cyears <- left_join(cyears, csp)
rm(csp)

# replace NAs with 0s (result of csp not including countries)
cyears$csp_success[is.na(cyears$csp_success)] <- 0
cyears$csp_failed[is.na(cyears$csp_failed)] <- 0
cyears$csp_plot[is.na(cyears$csp_plot)] <- 0
cyears$csp_alleged[is.na(cyears$csp_alleged)] <- 0
cyears$csp_tot_attempts[is.na(cyears$csp_tot_attempts)] <- 0
cyears$csp_attempt[is.na(cyears$csp_attempt)] <- 0


# 6. Fariss human rights scores ----

hps <- read_csv("~/Data/Human Rights/HumanRightsProtectionScores_v3.01.csv")

hps <- select(hps, gwno_loc=COW, year=YEAR, theta_mean)

cyears <- left_join(cyears, hps)
rm(hps)

cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  mutate(theta_lag=dplyr::lag(theta_mean), theta_lag2=dplyr::lag(theta_mean,2), theta_lag3=dplyr::lag(theta_mean, 3), theta_lag4=dplyr::lag(theta_mean, 4), theta_lag5=dplyr::lag(theta_mean, 5), theta_lag6=dplyr::lag(theta_mean, 6))

thetasd <- sd(cyears$theta_mean, na.rm=T) * -0.5

cyears$theta_shock3 <- ifelse((cyears$theta_lag - cyears$theta_lag4) <= thetasd, 1, 0)
cyears$theta_shock5 <- ifelse((cyears$theta_lag - cyears$theta_lag6) <= thetasd, 1, 0)
cyears$theta_diff_lag <- cyears$theta_lag - cyears$theta_lag2
rm(thetasd)

# 7. NAVCO 2.0 data ----

navco <- read_excel("~/Data/Human Rights/NAVCO v2.0.xlsx")

navco <- select(navco, gwno_loc=lccode, year, prim_method, discrim, repression)

navco[navco < 0] <- NA

# collapse to country-years
navco <- navco %>% 
  group_by(gwno_loc, year) %>% 
  summarize(navco_campaigns=n(), viol_campaigns=sum(prim_method==0), nonviol_campaigns=sum(prim_method==1), repression=max(repression), discrim=max(discrim), nonviol_repression=ifelse(sum(prim_method==0 & repression>2)>0, 1, 0))

cyears <- left_join(cyears, navco)
rm(navco)

cyears$nonviol_repression <- ifelse(is.na(cyears$nonviol_repression) & cyears$year < 2007, 0, cyears$nonviol_repression)
cyears$repression <- ifelse(is.na(cyears$repression) & cyears$year < 2007, 0, cyears$repression)
cyears$navco_campaigns <- ifelse(is.na(cyears$navco_campaigns) & cyears$year < 2007, 0, cyears$navco_campaigns)

cyears$any_repressed <- as.factor(ifelse(cyears$navco_campaigns>0 & cyears$repression==3, 1, 0))
cyears$any_nonrepressed <- as.factor(ifelse(cyears$navco_campaigns>0 & cyears$repression<3, 1, 0))

cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  mutate(navco_lag=ifelse(dplyr::lag(navco_campaigns)>=1, 1, 0), nonviol_repress_lag3=ifelse(dplyr::lag(nonviol_repression)==1 | dplyr::lag(nonviol_repression,2)==1 | dplyr::lag(nonviol_repression,3)==1,1,0), nonviol_repress_lag5=ifelse(dplyr::lag(nonviol_repression)==1 | dplyr::lag(nonviol_repression,2)==1 | dplyr::lag(nonviol_repression,3)==1 | dplyr::lag(nonviol_repression,4)==1 | dplyr::lag(nonviol_repression,5)==1, 1, 0))

cyears$any_navco <- ifelse(cyears$navco_campaigns>0, 1, 0)

# EPR data ----

# GDP growth ----

# Primary data source: Penn World Tables 9.1
# This uses 2011 millions of USD
library(pwt9)

pwt <- pwt9.1

pwt$gwno_loc <- countrycode(pwt$isocode, "iso3c", "cown")
pwt$gwno_loc[pwt$isocode=="SRB"] <- 345

pwt <- select(pwt, gwno_loc, year, rgdpe, pop_pwt=pop)

cyears <- left_join(cyears, pwt)
rm(pwt)

# Secondary source: Gleditsch
# This uses 2005 dollars, seems to also be in millions
gled <- read_delim("~/Data/Economics/gdpv6.txt", delim = "\t")

# use this calculation to convert to 2011 dollars using this calculation: https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100.00&year1=200501&year2=201101
gled$realgdp <- gled$realgdp * 1.11548

# and convert pop to 1,000s
gled$pop <- gled$pop / 1000

gled <- select(gled, gwno_loc=statenum, year, realgdp_gled=realgdp, pop_gled=pop)

cyears <- left_join(cyears, gled)
rm(gled)

# fill in missing pwt
cyears$rgdpe <- ifelse(is.na(cyears$rgdpe), cyears$realgdp_gled, cyears$rgdpe)
cyears$pop_pwt <- ifelse(is.na(cyears$pop_pwt), cyears$pop_gled, cyears$pop_gled)

# create additional measures
cyears$gdppc <- cyears$rgdpe / cyears$pop_pwt

cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  mutate(rgdpe_lag=lag(rgdpe), rgdp_lag2=lag(rgdpe,2), gdppc_lag=lag(gdppc), gdppc_lag2=lag(gdppc,2))

cyears$gdpgr_lag <- cyears$gdppc_lag/cyears$gdppc_lag2*100 - 100


# Welfare spending ----

gsre <- read_delim("~/Data/Economics/GSRE_raw_1_0.csv", ";", escape_double = FALSE, trim_ws = TRUE)

gsre <- select(gsre, gwno_loc=cowcode, year, total_welfare)

cyears <- left_join(cyears, gsre)

  
# WDI data ----

library(WDI)

oil <- WDI(country="all", indicator="NY.GDP.PETR.RT.ZS", start=1960, end=2017)
area <- WDI(country="all", indicator="AG.LND.TOTL.K2", start=1960, end=2017)

oil <- left_join(oil, area)
rm(area)

oil$gwno_loc <- countrycode(oil$iso2c, "iso2c", "cown")
oil$gwno_loc[oil$country=="Serbia"] <- 340
oil$gwno_loc[oil$country=="Kosovo"] <- 347

oil <- select(oil, gwno_loc, year, oilrents=NY.GDP.PETR.RT.ZS, area=AG.LND.TOTL.K2)

cyears <- left_join(cyears, oil)
rm(oil)

# fill area backwards
cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  fill(area, .direction = "down")

# Polity ----

polity <- read_excel("~/Data/Regimes/p4v2018.xls")

polity <- select(polity, year, gwno_loc=ccode, polity2, durable)

polity <- polity %>% 
  group_by(gwno_loc) %>% 
  mutate(polity2_lag=lag(polity2), polity2_lag2=lag(polity2, 2))

polity$liberal <- ifelse(polity$polity2_lag > polity$polity2_lag2, 1, 0)

cyears <- left_join(cyears, polity)
rm(polity)

# Military regime ----

gwf <- read_delim("~/Data/Regimes/GWF_AllPoliticalRegimes.txt", delim="\t")

gwf <- select(gwf, gwno_loc=cowcode, year, gwf_military)

cyears <- left_join(cyears, gwf)
rm(gwf)

cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  mutate(gwf_mil_lag=lag(gwf_military))


# Cheibub ----

cg <- read_excel("~/Data/Regimes/cheibub_regime.xls")

cg$presidential <- ifelse(cg$regime==2, 1, 0)
cg$cg_mil <- ifelse(cg$regime==4, 1, 0)

cg <- select(cg, gwno_loc=cowcode, year, regime, presidential, cg_mil)

cg <- cg %>% 
  group_by(gwno_loc) %>% 
  mutate(presidential_lag=lag(presidential))

cyears <- left_join(cyears, cg)
rm(cg)


# Counterbalancing ----

load("~/Data/Regimes/Coup-Proofing 1970-2019.RData")

table <- select(table, gwno_loc=ccode, year, effectivenumber)

cyears <- left_join(cyears, table)
rm(table)


# CINC ----

cinc <- read_csv("~/Data/COW/NMC_5_0.csv")

cinc <- select(cinc, gwno_loc=ccode, year, milex, milper, tpop)

cinc$milexps <- cinc$milex / (cinc$milper+1)

cyears <- left_join(cyears, cinc)
rm(cinc)


# Interstate conflict ----

mids <- read_csv("~/Data/COW/MID 4.3/MIDB 4.3.csv")

mids_yearly <- mids %>% 
  rowwise() %>% 
  do(data.frame(dispnum3=.$dispnum3, gwno_loc=.$ccode, year=seq(.$styear,.$endyear)))

mids_yearly <- mids_yearly %>% 
  group_by(gwno_loc, year) %>% 
  summarize(mids=n_distinct(dispnum3))

cyears <- left_join(cyears, mids_yearly)
rm(mids,mids_yearly)

cyears$mids[is.na(cyears$mids)] <- 0


# Past events ----

cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  mutate(past_reb=(cumsum(new_mil)-1), past_coup=(cumsum(thyne_attempt)-1))

cyears$past_defection <- cyears$past_reb + cyears$past_coup
  
cyears$reb_year <- ifelse(cyears$new_mil==1, cyears$year, NA)
cyears$coup_year <- ifelse(cyears$thyne_attempt==1, cyears$year, NA)

cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  fill(reb_year, coup_year)

cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  mutate(reb_year_lag=lag(reb_year), coup_year_lag=lag(coup_year))

cyears$recent_reb <- ifelse((cyears$year - cyears$reb_year_lag) <= 6, 1, 0)
cyears$recent_reb[is.na(cyears$recent_reb)] <- 0

cyears$nonrecent_reb <- ifelse((cyears$year - cyears$reb_year_lag) > 6, 1, 0)
cyears$nonrecent_reb[is.na(cyears$nonrecent_reb)] <- 0

cyears$recent_coup <- ifelse((cyears$year - cyears$coup_year_lag) <= 6, 1, 0)
cyears$recent_coup[is.na(cyears$recent_coup)] <- 0

# Regional dummies ----

cyears$region <- NA
cyears$region[cyears$gwno_loc <= 20 | cyears$gwno_loc>=900] <- "N. America/Oceania"
cyears$region[cyears$gwno_loc > 20 & cyears$gwno_loc < 200] <- "Latin America"
cyears$region[cyears$gwno_loc >= 200 & cyears$gwno_loc < 400] <- "Europe"
cyears$region[cyears$gwno_loc >= 400 & cyears$gwno_loc < 630] <- "SS Africa"
cyears$region[cyears$gwno_loc >= 630 & cyears$gwno_loc < 700] <- "Mideast/N. Africa"
cyears$region[cyears$gwno_loc >= 700 & cyears$gwno_loc < 900] <- "Asia"


# Fearon & Laitin ----

fearon <- read_dta("~/Data/Ethnicity/fearonlaitin.dta")

fearon <- select(fearon, gwno_loc=ccode, year, mtnest, ethfrac)

cyears <- left_join(cyears, fearon)

cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  fill(mtnest, ethfrac, .direction = "up")

cyears <- cyears %>% 
  group_by(gwno_loc) %>% 
  fill(mtnest, ethfrac, .direction = "down")
