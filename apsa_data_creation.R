library(readr)
library(readxl)
library(dplyr)
library(countrycode)

# Use state membership to build list of country-years ----
coups <- read_csv("data/states2016.csv")

# don't have any covariates pre-WWII, so reset lowest start year to 1946
coups$styear <- ifelse(coups$styear < 1946, 1946, coups$styear)

# remove episodes that ended before 1946
coups <- filter(coups, endyear >= 1946)

# extend end year to 2017
coups$endyear <- ifelse(coups$endyear == 2016, 2017, coups$endyear)

#expand to country-year data
coups <- coups %>% 
  rowwise() %>% 
  do(data.frame(ccode=.$ccode, stateabb=.$stateabb, year=seq(.$styear, .$endyear))) #10713 cases


####### I. DVs #######

# Thyne and Powell Coup data V2018.07.18 ----
thyne <- read_delim("data/powell_thyne_ccode_year.txt", delim="\t")

# count total coups (1=unsuccessful, 2=successful)
thyne$thyne_failed <- rowSums(thyne[,3:6]==1)
thyne$thyne_success <- rowSums(thyne[,3:6]==2)
thyne$thyne_tot_attempts <- thyne$thyne_failed + thyne$thyne_success
thyne$thyne_attempt <- ifelse(thyne$thyne_tot_attempts > 0, 1, 0)

# remove unnecessary columns and merge
thyne <- select(thyne, ccode, year, thyne_failed:thyne_attempt)
coups <- left_join(coups, thyne)
rm(thyne)

# convert NAs to 0s (result of some countries not being included in Thyne data)
coups$thyne_attempt[is.na(coups$thyne_attempt)] <- 0
coups$thyne_success[is.na(coups$thyne_success)] <- 0
coups$thyne_tot_attempts[is.na(coups$thyne_tot_attempts)] <- 0
coups$thyne_failed[is.na(coups$thyne_failed)] <- 0


# Center for Systemic Peace Coup Data Series 2017 ----
csp <- read_excel("data/CSPCoupsAnnualv2017.xls")

# limit to necessary columns
csp <- select(csp, ccode, year, csp_success=scoup1, csp_failed=atcoup2, csp_plot=pcoup3, csp_alleged=apcoup4)

# summary counts
csp$csp_tot_attempts <- csp$csp_success + csp$csp_failed
csp$csp_attempt <- ifelse(csp$csp_tot_attempts > 0, 1, 0)

coups <- left_join(coups, csp)
rm(csp)

# replace NAs with 0s (result of csp not including countries)
coups$csp_success[is.na(coups$csp_success) & coups$year <= 2016] <- 0
coups$csp_failed[is.na(coups$csp_failed) & coups$year <= 2016] <- 0
coups$csp_plot[is.na(coups$csp_plot) & coups$year <= 2016] <- 0
coups$csp_alleged[is.na(coups$csp_alleged) & coups$year <= 2016] <- 0
coups$csp_tot_attempts[is.na(coups$csp_tot_attempts) & coups$year <= 2016] <- 0
coups$csp_attempt[is.na(coups$csp_attempt) & coups$year <= 2016] <- 0


# Rebel group data ----
load("data/master_rebel_yearly.Rdata")

# subset to first year for each group
ucdp.group <- ucdp.dyad %>% 
  group_by(SideBID) %>% 
  summarize_all(first)

# create indicators for coup/rebellion
ucdp.group$nsa_coup <- ifelse(ucdp.group$conflicttype=="coup d'etat", 1, 0)
ucdp.group$nsa_milrebel <- ifelse(ucdp.group$origin=="military faction", 1, 0)
ucdp.group$nsa_milrebel_nocoup <- ifelse(ucdp.group$nsa_milrebel==1 & ucdp.group$nsa_coup==0, 1, 0)

# standardize column names
ucdp.group <- rename(ucdp.group, ccode=GWNoLoc, year=Year)
ucdp.group$ccode <- as.numeric(as.character(ucdp.group$ccode))

# condense to yearly summaries
ucdp.group <- ucdp.group %>% 
  group_by(ccode, year) %>% 
  summarize(nsa_coup=sum(nsa_coup), nsa_milrebel=sum(nsa_milrebel), nsa_milrebel_nocoup=sum(nsa_milrebel_nocoup))

coups <- left_join(coups, ucdp.group)
rm(ucdp.group)

# merge in intensity level for all conflicts
ucdp.dyad <- select(ungroup(ucdp.dyad), ccode=GWNoLoc, year=Year, ucdp_intensitylevel=IntensityLevel)
ucdp.dyad$ccode <- as.numeric(as.character(ucdp.dyad$ccode))

ucdp.dyad <- ucdp.dyad %>% 
  group_by(ccode, year) %>% 
  summarize(ucdp_intensitylevel=max(ucdp_intensitylevel))

coups <- left_join(coups, ucdp.dyad)
rm(ucdp.dyad)

# convert NAs to 0s (countries not that don't appear in conflict data)
coups$nsa_coup[is.na(coups$nsa_coup) & coups$year <= 2016] <- 0
coups$nsa_milrebel[is.na(coups$nsa_milrebel) & coups$year <= 2016] <- 0
coups$nsa_milrebel_nocoup[is.na(coups$nsa_milrebel_nocoup) & coups$year <= 2016] <- 0
coups$ucdp_intensitylevel[is.na(coups$ucdp_intensitylevel) & coups$year <= 2016] <- 0
coups$civil_conflict <- ifelse(coups$ucdp_intensitylevel > 0, 1, 0)


###### II. IV 1: Repression ######

# Human Rights Protection Scores v. 2.04 ----
hrps <- read_csv("data/HumanRightsProtectionScores_v2.04.csv")

# calculate global average and sd for each year
hrps <- hrps %>% 
  group_by(YEAR) %>% 
  mutate(latentmean_global_mean=mean(latentmean, na.rm=T), latentmeant_global_sd=sd(latentmean, na.rm = T))

# create binary measures
hrps$severe_repress <- ifelse(hrps$latentmean <= (hrps$latentmean_global_mean - (hrps$latentmeant_global_sd)), 1, 0)
hrps$high_DISAP <- ifelse(hrps$DISAP==0, 1, 0)
hrps$high_KILL <- ifelse(hrps$KILL==0, 1, 0)
hrps$high_POLPRIS <- ifelse(hrps$POLPRIS==0, 1, 0)
hrps$high_TORT <- ifelse(hrps$TORT==0, 1, 0)
hrps$high_Amnesty <- ifelse(hrps$Amnesty==5, 1, 0)
hrps$high_State <- ifelse(hrps$State==5, 1, 0)

# limit to relevant columns and merge
hrps <- rename(hrps, ccode=COW, year=YEAR)
coups <- left_join(coups, hrps)
rm(hrps)


# PHOENIX Historical Data ----
# nyt <- read_csv("~/ClineCenterHistoricalPhoenixEventData/PhoenixNYT_1945-2005.csv")
# swb <- read_csv("~/ClineCenterHistoricalPhoenixEventData/PhoenixSWB_1979-2015.csv")
# fbis <- read_csv("~/ClineCenterHistoricalPhoenixEventData/PhoenixFBIS_1995-2004.csv")
# 
# # combine
# phoenix <- rbind(nyt, swb, fbis)
# rm(nyt, swb, fbis)
# 
# phoenix$root_code <- as.numeric(phoenix$root_code)
# 
# # convert to COW codes and a few
# phoenix$ccode <- countrycode(phoenix$countryname, "iso3c", "cown")
# phoenix$ccode[phoenix$countryname=="XKX"] <- 347 #Kosovo
# phoenix$ccode[phoenix$countryname=="SRB"] <- 345 #COW considers Serbia to have inherited the Yugoslavia code
# phoenix$ccode[phoenix$countryname=="PSE"] <- 666
# 
# # create subset of protests
# protest <- filter(phoenix, (source_agent=="CVL" | source_agent=="OPP") & (target_agent=="GOV" | target_agent=="SPY" | target_agent=="MIL" | target_agent=="COP" | target_agent=="JUD") & root_code>=10)
# 
# # create subset of repression
# repress <- filter(phoenix, (source_agent=="GOV" | source_agent=="MIL" | source_agent=="COP") & (target_agent=="CVL" | target_agent=="OPP") & root_code>=17)
# 
# rm(phoenix)
# 
# # create country_year summary
# protest <- protest %>% 
#   group_by(ccode, year) %>% 
#   summarize(phoenix_protests=n(), phoenix_violent_protests=sum(root_code>=17))
# 
# repress <- repress %>% 
#   group_by(ccode, year) %>% 
#   summarize(phoenix_repress=n(), phoenix_violent_repress=sum(code=="175" | root_code>=18))
# 
# # merge
# coups <- left_join(coups, protest)
# coups <- left_join(coups, repress)
# rm(protest, repress)
# 
# # convert missing to 0 (assume no repression if not observed)
# coups$phoenix_protests[is.na(coups$phoenix_protests) & coups$year <= 2015] <- 0
# coups$phoenix_violent_protests[is.na(coups$phoenix_violent_protests) & coups$year <= 2015] <- 0
# coups$phoenix_repress[is.na(coups$phoenix_repress) & coups$year <= 2015] <- 0
# coups$phoenix_violent_repress[is.na(coups$phoenix_violent_repress) & coups$year <= 2015] <- 0


# NAVCO 2.0 ----
navco <- read_excel("data/NAVCO v2.0.xlsx", na = "-99")

navco <- select(navco, ccode=lccode, year, navco_prim_method=prim_method, navco_repression=repression)

navco$navco_nonviolent_repression <- ifelse(navco$navco_prim_method==1 & navco$navco_repression > 1, 1, 0)

navco <- navco %>% 
  group_by(ccode, year) %>% 
  summarize(navco_repression=max(navco_repression), navco_nonviolent_repression=ifelse(sum(navco_nonviolent_repression>=1, na.rm=T), 1, 0), navco_events=n())

coups <- left_join(coups, navco)
rm(navco)

coups$navco_repression[is.na(coups$navco_repression)] <- 0
coups$navco_nonviolent_repression[is.na(coups$navco_nonviolent_repression)] <- 0
coups$navco_events[is.na(coups$navco_events)] <- 0


##### III. Ethnicity IV #####

# Security Force Ethnicity Data v1.0 ----
sfe <- read_csv("data/SFE_MENA_v1.csv", na = c("-99","-88",""))

# expand to yearly are limit to needed columns
sfe <- sfe %>% 
  rowwise() %>% 
  do(data.frame(ccode=.$GWID, gwgroupid=.$EPR_GROUP_ID, year=seq(.$FROM,.$TO), OFFICER_RELATIVE=.$OFFICER_RELATIVE, OFFICER_UPPER=.$OFFICER_UPPER, RANK_FILE_RELATIVE=.$RANK_FILE_RELATIVE, RANK_FILE_UPPER=.$RANK_FILE_UPPER))

# import epr and expand to yearly
epr <- read_csv("data/EPR-2018.csv")

epr <- epr %>% 
  rowwise() %>% 
  do(data.frame(ccode=.$gwid, gwgroupid=.$gwgroupid, year=seq(.$from,.$to), size=.$size, status=.$status))

# merge epr into sfe
sfe <- left_join(sfe, epr)

# create country-year summaries for sfe
sfe <- sfe %>% 
  group_by(ccode, year) %>% 
  summarize(officer_underrepresented=sum(OFFICER_RELATIVE <= 2, na.rm=T), rank_underrepresented=sum(RANK_FILE_RELATIVE <= 2, na.rm=T), officer_pc_discriminated=sum(OFFICER_UPPER[status=="DISCRIMINATED" | status=="POWERLESS"], na.rm=T), rank_pc_discriminated=sum(RANK_FILE_UPPER[status=="DISCRIMINATED" | status=="POWERLESS"], na.rm=T))

coups <- left_join(coups, sfe)
rm(sfe)

# create country-year summaries for epr
epr <- epr %>% 
  group_by(ccode, year) %>% 
  summarize(country_pc_discriminated=sum(size[status=="DISCRIMINATED" | status=="POWERLESS"]))

coups <- left_join(coups, epr)
rm(epr)


##### IV. Controls #####

# Polity ----
polity <- read_excel("data/p4v2017.xls")

polity <- select(polity, ccode, year, polity2)

coups <- left_join(coups, polity)
rm(polity)

coups$democ <- ifelse(coups$polity2 > 5, 1, 0)
coups$autoc <- ifelse(coups$polity2 < -5, 1, 0)


# Geddes et al. authoritarian regime data ----
auth <- read_delim("data/GWFtscs.txt", delim="\t")

auth <- select(auth, ccode=cowcode, year, gwf_regimetype, gwf_military)

coups <- left_join(coups, auth)
rm(auth)

coups$gwf_military[is.na(coups$gwf_military) & coups$polity2 > -5] <- 0


# GDPpc and pop from Gleditsch ----
gdp <- read_delim("data/gdpv6.txt", delim = "\t")

gdp <- select(gdp, ccode=statenum, year, rgdppc, pop)

coups <- left_join(coups, gdp)
rm(gdp)

coups$log_rgdppc <- log(coups$rgdppc)
coups$log_pop <- log(coups$pop)


# Regional dummies ----
coups$region[coups$ccode < 200] <- "Americas"
coups$region[coups$ccode >= 200 & coups$ccode < 400] <- "Europe"
coups$region[coups$ccode >= 400 & coups$ccode < 600] <- "Sub-Saharan Africa"
coups$region[coups$ccode >= 600 & coups$ccode < 700] <- "MENA"
coups$region[coups$ccode >= 700] <- "Asia"

coups$americas <- ifelse(coups$region=="Americas", 1, 0)
coups$africa <- ifelse(coups$region=="Sub-Saharan Africa", 1, 0)
coups$mena <- ifelse(coups$region=="MENA", 1, 0)
coups$asia <- ifelse(coups$region=="Asia", 1, 0)

# Write to Stata ----
library(haven)

write_dta(coups, "data/apsa_data.dta", version=13)
