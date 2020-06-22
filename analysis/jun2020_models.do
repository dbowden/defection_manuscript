use "/Users/david/OneDrive - PennO365/defection_manuscript/may2020data.dta"

xtset gwno_loc year, yearly

gen democ = polity2 > 4 if !missing(polity2)

gen lnmilex = ln(milex)

gen lnarea = ln(area)

gen lnmilper = ln(milper)

gen lngdppc_lag = ln(gdppc_lag)

gen postcw = year > 1988

encode region, gen(regioncat)














/* OLD */

/* All variations return substantively similar results unless otherwise noted */

probit new_mil theta_diff_lag gdpgr_lag

/* fixed effects */
xtlogit new_mil theta_diff_lag gdpgr_lag, fe

probit new_mil theta_diff_lag gdpgr_lag i.gwno_loc

/* alt DVs */
probit forge_mil theta_diff_lag gdpgr_lag

probit thyne_tot_attempts theta_diff_lag gdpgr_lag

/* alt IVs */
probit new_mil theta_lag gdpgr_lag

probit new_mil downgrade_lag gdpgr_lag /* just barely significant */

probit new_mil theta_shock3 gdpgr_lag

probit new_mil theta_shock5 gdpgr_lag

probit new_mil nonviol_repression gdpgr_lag

probit new_mil theta_shock3 gdpgr_lag

/* controls */
probit new_mil theta_diff_lag gdpgr_lag polity2

probit new_mil theta_diff_lag gdpgr_lag democ

probit new_mil theta_diff_lag gdpgr_lag liberal

probit new_mil theta_diff_lag gdpgr_lag democ milex

probit new_mil theta_diff_lag gdpgr_lag democ lnmilex

probit new_mil theta_diff_lag gdpgr_lag democ lnmilex gdppc_lag /* democ and milex become nonsig */

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag polity2_lag

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable gwf_mil_lag

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable presidential

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable liberal

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable pop_pwt

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable lnarea

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable effectivenumber /* effectivenumber is marginally significant but creates lots of missingness */

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable lnmilex

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable lnmilper

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable mids

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable past_reb

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable past_reb past_coup

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable recent_reb recent_coup

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable past_reb past_coup mtnest

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable past_reb past_coup ethfrac

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable past_reb past_coup i.regioncat

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable past_reb past_coup oilrents



/* Final Models */

probit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable past_reb past_coup

xtlogit new_mil theta_diff_lag gdpgr_lag gdppc_lag durable recent_reb, fe

ivprobit new_mil gdpgr_lag durable recent_reb (theta_diff_lag = youth_bulge_lag)