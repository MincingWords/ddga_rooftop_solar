# Processes economic considerations of rooftop solar systems
# June 2021 - Jeff Hubbs

# load libraries, set working directory, source function, set global cosntants, load in JSON NEMS output
library(nlmrt); library("formattable"); library(scales); library("plyr"); library("rjson")
setwd("/Users/jeff/Desktop/drawdown/misc/rooftop")
source("/Users/jeff/Desktop/drawdown/ran_read/nemsrowreturn.r")

# Pull in JSON object for NEMS run output; use nemsrowreturn() to obtain time series dataframe for table and row
nems_json_filename <- "ref2021.1130a.json"
# Don't reload if the JSON object already exists
if (!exists("nems_ref.json")){nems_ref.json <- fromJSON(file=nems_json_filename)}

# Global constants
dollar_year <- 2017 # All money values are expressed in dollars as valued in this year
cpi_lookback <- 30 # How many years back in FRED CPI data to look back for linear extrapolation
far_future <- 100 # Number of years for arbitrarily far projections of e.g. CPI time series

# We establish a list where each list element is a list consisting of a set of parameters describing a rooftop solar
# scenario. This list of lists will be iterated through, with an scenario evaluation performed upon each list in that
# list. A list of result lists will be generated for each scenario. Here are the elements of the scenario describing
# lists, in order:
#
#   [[1]]  scen_name     A text descriptor for the scenario
#   [[2]]  sys_power     Rated system nameplate capacity, in watts
#   [[3]]  loan_period   System installation loan period, in years
#   [[4]]  interest_pct  Loan annual interest rate (for zero, use very small number)
#   [[5]]  origination   Fee paid to originate loan (distinct from down payment - not applied against principal; 
#                        in dollar_year dollars)
#   [[6]]  cap_fac       Statewide weighted average capacity factor (computed from Google's Project Sunroof data)
#   [[7]]  bb_fac        Buyback price factor - ratio of price/kWh paid for surplus electricity sold back to the utility
#                        to the retail rate (i.e. value of 0.5 means utility pays half the retail rate for surplus)
#   [[8]]  bb_gen_pct    The percentage of rooftop generation that is sold back to the utility. This is a function of
#                        the own/op's local insolation curve, system capacity, capacity factor, and local consumption
#                        in terms of both magnitude and temporal distribution. SEIA indicates that 20-40% of generated
#                        residential rooftop solar is sold back to the grid so we choose a value between that range.
#   [[9]]  itc_pct       Investment tax credit - percentage of total system cost refunded to owner/operator; here, 
#                        assumption is that ITC refund is applied to loan principal in Year 1
#   [[10]] start_year    Year of installation and loan origination
#   [[11]] proj_years    Number of years from start_year to extend projection (value of 1 means years are start_year
#                        and start_year+1; needs to be greater than loan_period)
#   [[12]] degrade_pct   Annual panel power degradation per year, percent
#   [[13]] res_elec_rate Boolean; TRUE if you want to use NEMS' time series for South Atlantic Census Division
#                        residential retail electricity rate, FALSE if you want to use commercial rate. Note: in NEMS
#                        2021, SA CD residential rate is Table 153, Row 6 and commercial rate is Table 153, Row 13
#                        (units: $2020/MBTU)
#   [[14]] bldg_consump  Annual electricity consumption of the building (units: MWh)
#   [[15]] sol_fac       A multiplier for the building consumption used only to calculate the solar case.
#   [[16]] td_loss_pct   Electricity loss percentage due to transmission and distribution. See 
#                        https://www.eia.gov/electricity/state/
#   [[17]] inv_reup      Vector: [1] Years after installation for replacement of inverter
#                                [2] Inverter cost in $/W (2019 dollars) based on midpoint between residental w/ DC 
#                                    optimizers and residential w/ microinverters as of 2020 in Figure 6 (p. 26) of 
#                                    https://www.nrel.gov/docs/fy21osti/77324.pdf. (assume inverter max pwr = rated
#                                    system nameplate capacity)
#                                [3] Factor of replacement inverter cost going for labor (e.g. if new inverter is 
#                                    $X/W and factor is 0.5, labor cost of replacement will be $(0.5*X)/W)
#   [[18]] discount_pct  Discount rate (percent) for the discount-rated versions of the cash flow columns
#
# Set up list here:
#scen_list <- list(list("NREL residential base case",
#                       6200, 25, 5, 1000, 0.1431922, 1, 30, 0, 2030, 25, 0.5, FALSE, 10.649),
#                  list("NREL commercial base case",
#                       200000, 25, 3.8, 0, 0.1431922, 1, 30, 0, 2030, 25, 0.5, FALSE, 353.9))

# Generate the list of scenarios [NOTE: average residential consumption of 10.649MWh/yr provided by
# https://www.eia.gov/tools/faqs/faq.php?id=97&t=3]; multiplied by 200/6.2 to get analogous
# value for commercial
scen_list <- vector(mode="list", len=108) # Change len value to number of iterations through the for loops below
ctr <- 1
for (inst_year in c(2025,2030)){
  # The list elements here are [[1]] residential=TRUE
  #                            [[2]] system nameplate capacity (units: W)
  #                            [[3]] system type text for display
  #                            [[4]] annual loan interest rate
  #                            [[5]] loan origination fee
  #                            [[6]] average building annual consumption per NREL (units: MWh)
  for (bldg_type in list(list(TRUE,    6200,  "Resdl (6.2kW), 30% ITC",        5, 1000, 10.649),
                         list(FALSE, 200000,  "Commercial (200kW), 30% ITC", 3.5,    0, 10.649*(200/6.2))
                         )){
    for (cons_fac in list(list(0.9, "-10%"),
                          list(1, "nominal"),
                          list(1.1, "+10%")
                          )){
      for (sol_fac in list(list(0.9, "-10%"),
                           list(1, "unity"),
                           list(1.1, "+10%")
                           )){
        for (buyback_fac in list(list(0.4, "40%"),
                                 list(1, "unity"),
                                 list(1.2, "120%")
                                 )){
          scen_list[[ctr]] <- list(paste0(bldg_type[[3]],
                                          ", start yr ", inst_year,
                                          ", consump ", cons_fac[[2]],
                                          ", ", buyback_fac[[2]], " buyback",
                                          ", ", sol_fac[[2]], " consump w/ solar"),
                                   bldg_type[[2]],
                                   25,
                                   bldg_type[[4]],
                                   bldg_type[[5]],
# This number is old...            
#                                  0.1431922,
# But this one is based on a 51-county summation of buckets
                                   0.1473197,
                                   buyback_fac[[1]],
                                   30,
                                   30,
                                   inst_year,
                                   30, 
                                   0.5,
                                   bldg_type[[1]],
                                   bldg_type[[6]] * cons_fac[[1]],
                                   sol_fac[[1]],
                                   7.616,
                                   c(10, 
                                     0.275 * cpi_ratio_fred(dollar_year, 2019), # Cast this into dollar_year dollars
                                     0.5),
                                   5)
          ctr <- ctr + 1
        }
      }
    }
  }
}


# MISC FUNCTIONS
# Return the first four characters of a string
first_4 <- function(d){return(substr(d, 1, 4))} 

# CPI DATA LOAD
cpi_by_month <- read.csv("CPIAUCSL_2.csv")
yeerz <- unique(first_4(cpi_by_month$DATE))

# CONSUMER PRICE INDEX
# We need to establish a historical CPI table and extend it into the future so that we can recast dollar amounts 
# freely between one year and another. Data source is FRED (https://fred.stlouisfed.org/series/CPIAUCSL). 
# Objects cpi_by_month and yeerz are set up previously
valz <- c()
# Take the mean of each year's months' values to produce that year's value
for (yeer in yeerz){valz <- c(valz,mean(cpi_by_month$CPIAUCSL[which(yeer==first_4(cpi_by_month$DATE))]))}
cpi_by_year <- matrix(valz, ncol=1, byrow=TRUE)
rownames(cpi_by_year) <- yeerz; colnames(cpi_by_year) <- c("CPI")
# Now extend the time series into the future
#cpi.df <- data.frame(x=as.numeric(row.names(cpi_by_year)),
#                     y=as.vector(cpi_by_year))[(1+(length(cpi_by_year)-cpi_lookback)):length(cpi_by_year),]
cpi.df <- data.frame(x=as.numeric(row.names(cpi_by_year)), y=valz)
linmod <- lm(y~x, data=tail(cpi.df, n=cpi_lookback))
slope <- linmod$coefficients[2]; yint <- linmod$coefficients[1]
# Extrapolate out to an arbitrarily far future
latest_cpi_year <- max(as.numeric(row.names(cpi_by_year)))
extrap_years <- seq(1 + latest_cpi_year, far_future + latest_cpi_year)
cpi_extrap <- function(yeer){(slope * yeer) + yint}
cpi_extension <- as.matrix(mapply(cpi_extrap, extrap_years))
row.names(cpi_extension) <- as.character(extrap_years)
cpi_by_year <- rbind(cpi_by_year, cpi_extension) # Tack on the extrapolation to the history
#plot(x=as.numeric(row.names(cpi_by_year)), y=cpi_by_year, type="l", xlab="Year", ylab="CPI")
#abline(v=min(extrap_years), col="red", lty=2)
# Now, a function cpi_ratio_fred(a, b) that will give you the value of one year-a dollar in year b:
cpi_ratio_fred <- function(y1, y2){return(cpi_by_year[as.character(y1),]/cpi_by_year[as.character(y2),])}

# NEMS-DERIVED CARBON INTENSITY OF ELECTRICITY
# South Atlantic Census Division total electric-power-related CO2 emissions is AEO2021 Table 162, Row 33 (units: Mt); same 
# Census Division's *purchased* electricity is AEO2021 Table 144, Row 103 (units: quadrillion BTU). Note that while both time
# series cover the same region, the former is connected to generation whereas the latter is connected to consumption. 
# Generation is connected to consumption through the transmission and distribution loss, which one can calculate from the
# data at https://www.eia.gov/electricity/state/ to be 7.616% for Georgia. We've made that loss percentage a scenario-
# passable parameter.
nems_co2_emissions.df <- nemsrowreturn(nems_ref.json, 162, 33)
nems_purchased_elec.df <- nemsrowreturn(nems_ref.json, 144, 103)
# Set up a new temporary dataframe to hold elec CO2 emissions and purchased electricity and calc CO2 intensity of electricity
co2_intens.df <- nems_co2_emissions.df 
colnames(co2_intens.df) <- c("co2_emissions") # Units: Mt
co2_intens.df$purchased_elec <- nems_purchased_elec.df$value # Units: quad
co2_intens.df$intensity <- (co2_intens.df$co2_emissions/co2_intens.df$purchased_elec)/2.931E+08 # Units: Mt/MWh=t/Wh


# ESTABLISH SYSTEM COST FUNCTION
# We want to produce a system cost per capacity power value as a function of system capacity power and year.
# To do this, we use two 2010-2020 time series from https://www.nrel.gov/solar/solar-installed-system-cost.html: $/W 
# for a "22-panel" (assumed 6.2kW capacity) residential system and a 200-kW commercial system, fit reciprocal
# (hyperbolic) curves to them, and then treat the two curves as levels on a Z-axis. We then find the slope and Y-
# intercept of the line that joins the points in the two curves at the same year. Assume NREL's data is in 2020
# dollars.
sys_cost.df <- data.frame(year=2010:2020,
                          res=c(7.53, 6.62, 4.67, 4.09, 3.60, 3.36,
                                3.16, 2.94, 2.78, 2.77, 2.71),
                          com=c(5.57, 5.18, 3.57, 2.90, 2.89, 2.40, 
                                2.29, 1.94, 1.88, 1.76, 1.72))
# Note: nls() doesn't work here; using 'c1' because 'c' is misinterpreted
resfit <- nlxb(y~a + (b/((d*t)-c1)), data=data.frame(t=sys_cost.df$year, y=sys_cost.df$res),
               start=list(a=1.0, b=6, c1=2, d=0.3))
comfit <- nlxb(y~a + (b/((d*t)-c1)), data=data.frame(t=sys_cost.df$year, y=sys_cost.df$com),
               start=list(a=1.0, b=6, c1=2, d=0.3))
# Extend the dataframe into the far future
sys_cost.df <- rbind(sys_cost.df, data.frame(year=2021:(2020+far_future),
                                             res=rep(NA, far_future),
                                             com=rep(NA, far_future)))
aye <- resfit$coefficients["a"]; bee <- resfit$coefficients["b"]
cee <- resfit$coefficients["c1"]; dee <- resfit$coefficients["d"]
sys_cost.df$rescurve <- (bee/((dee*sys_cost.df$year)-cee))+aye
aye <-comfit$coefficients["a"]; bee <- comfit$coefficients["b"]
cee <- comfit$coefficients["c1"]; dee <- comfit$coefficients["d"]
sys_cost.df$comcurve <- (bee/((dee*sys_cost.df$year)-cee))+aye
sys_cost.df$slope <- (sys_cost.df$comcurve-sys_cost.df$rescurve)/(200-6.2)
sys_cost.df$yint <- sys_cost.df$comcurve - (sys_cost.df$slope * 200)
# Units for pwr is kW; result is in dollar_year dollars
sys_cost <- function(yr, pwr){
  inx <- which(sys_cost.df$year==yr)
  return(((sys_cost.df$slope[inx] * pwr) + sys_cost.df$yint[inx]) * cpi_ratio_fred(dollar_year, 2020))
}

# Establish output dataframe and evaluate scenarios in the input list
output_list <- vector(mode="list", len=length(scen_list))
ctr <- 1
for (scenario in scen_list){
  scen_name <- scenario[[1]];       sys_power <- scenario[[2]];     loan_period <- scenario[[3]]; interest_pct <- scenario[[4]]
  origination <- scenario[[5]];     cap_fac <- scenario[[6]];       bb_fac <- scenario[[7]];      bb_gen_pct <- scenario[[8]]
  itc_pct <- scenario[[9]];         start_year <- scenario[[10]];   proj_years <- scenario[[11]]; degrade_pct <- scenario[[12]]
  res_elec_rate <- scenario[[13]];  bldg_consump <- scenario[[14]]; sol_fac <- scenario[[15]];    td_pct <- scenario[[16]]
  inv_reup <- scenario[[17]];       discount_pct <- scenario[[18]]
  
  # A wrapper function for cpi_ratio_fred() facilitating the use of the apply() family of functions
  quick_cpi <- function(nyr){cpi_ratio_fred(dollar_year, nyr)}
  
  # Calculate loan principal and payments; establish dataframe
  system_cost_contemp <- sys_power * sys_cost(start_year, sys_power/1000) * cpi_ratio_fred(start_year, dollar_year) # start_year $
  i <- (interest_pct/100)/12
  payment_contemp <- 12*((i*(system_cost_contemp * (1 - (itc_pct/100))))/(1-((1+i)^(-loan_period*12))))
  writeLines(paste0("Contemporary-year annual loan payment: ", dollar(payment_contemp)))
  year_range <- start_year:(start_year+(proj_years-1))
  loan_years <- start_year:(start_year+(loan_period-1))
  pmt <- unlist(lapply(loan_years, quick_cpi)) * (rep(payment_contemp, loan_period) + c(origination, rep(0, loan_period-1)))
  roof.df <- data.frame(year=year_range,
                        payment=c(pmt, rep(0, proj_years-loan_period)))
  
  # Calculate annual generation and amounts sold back to and bought from utility
  init_annl_gen_mwh <- sys_power * cap_fac * 24 * 365.25 * 1E-06
  roof.df$annl_gen_mwh <- init_annl_gen_mwh * ((1-(degrade_pct/100))^(0:(proj_years-1)))
  roof.df$annl_sold_mwh <- roof.df$annl_gen_mwh * (bb_gen_pct/100)
  roof.df$annl_bought_mwh <- (rep(bldg_consump * sol_fac, length(year_range)) + roof.df$annl_sold_mwh) - roof.df$annl_gen_mwh
  
  # Calculate avoided CO2 emissions
  # In the NEMS time range, intensity decays to an asymptote (circa 2E-07). In case our time range is longer than NEMS', 
  # we're just going to repeat the last intensity value to the end of our time range. To do this more easily, let's pre-
  # write the entire new column with the last intensity we calculated from NEMS. Recall that this is the amount of CO2
  # emitted in Mt by the *generation* of 1MWh of electricity in the South Atlantic Census Division; it is a function of
  # how much fossil vs. non-fossil generation there is in a given year.
  roof.df$co2_intens <- tail(co2_intens.df$intensity, n=1) # Units: Mt/MWh=t/Wh
  # Now match up rows from the intensity dataframe with the rows in roof.df and repopulate that col with matching values 
  roofyears <- as.character(roof.df$year)
  for (yearlabel in row.names(co2_intens.df)){
    if (yearlabel %in% roofyears){ # Don't bother trying to match if NEMS years aren't in our range
      roof.df$co2_intens[which(yearlabel==roofyears)] <- co2_intens.df[yearlabel,]$intensity
    }
  }
  # Now calculate *avoided CO2 emissions*. This will be the product of the intensity we just calculated and the displaced
  # generation associated with electricity generated by the solar rig. To get the latter, we need to increase the 
  # solar-generated electricity by the transmission and distribution loss percentage.
  roof.df$co2_avoided_t <- (1 + (td_pct/100)) * roof.df$annl_gen_mwh * roof.df$co2_intens * 1E+06 # Units: t
  
  # Obtain retail electricity price time series from NEMS and set surplus generation buyback rate time series
  if (res_elec_rate){
    use_row <- 6
  }else{
    use_row <- 13
  }
  nems_price.df <- nemsrowreturn(fromJSON(file="ref2018.1213a.json"), 153, use_row) # This is in $2020/MBTU
  nems_years <- as.numeric(row.names(nems_price.df))
  # We may need to extend out the value of the last year's price (easier than extrapolating and no big deal)
  if (max(nems_years) < max(roof.df$year)){
    new_years <- seq(max(nems_years + 1), max(roof.df$year))
    last_price <- nems_price.df[as.character(max(nems_years)),]
    appendme <- data.frame(value=rep(last_price, length(new_years)))
    row.names(appendme) <- as.character(new_years)
    nems_price.df <- rbind(nems_price.df, appendme)
  }
  roof.df$price_per_kwh <- nems_price.df[as.character(year_range),] * 1E-06 * 3412 * cpi_ratio_fred(dollar_year, 2020)
  roof.df$buyback_per_kwh <- roof.df$price_per_kwh * bb_fac
  
  # INVERTER REPLACEMENT
  # This is coded to include two inverter replacements, one in start_year+inv_reup[1] and another in start_year+(2*inv_reup[1])
  roof.df$inverter_cost <- rep(0, nrow(roof.df))        # Initialize column with all zeros
  new_inverter_hw <- inv_reup[2] * sys_power            # Cost of new inverter hardware
  new_inverter_labor <- new_inverter_hw * inv_reup[3]   # Cost of labor to install new hardware
  roof.df$inverter_cost[which(roof.df$year==(start_year+inv_reup[1]))] <- new_inverter_hw + new_inverter_labor
  roof.df$inverter_cost[which(roof.df$year==(start_year+(2*inv_reup[1])))] <- new_inverter_hw + new_inverter_labor
    
  # CASH FLOW
  roof.df$elec_cost <- roof.df$annl_bought_mwh * roof.df$price_per_kwh * 1000
  roof.df$elec_income <- roof.df$annl_sold_mwh * roof.df$buyback_per_kwh * 1000
  # For these, negative means net loss to owner/operator
  roof.df$net_with_solar <- roof.df$elec_income - (roof.df$payment + roof.df$elec_cost + roof.df$inverter_cost)
  roof.df$net_wo_solar <- -rep(bldg_consump, length(year_range)) * roof.df$price_per_kwh * 1000
  roof.df$differential <- roof.df$net_with_solar - roof.df$net_wo_solar
  roof.df$cumul_diff <- cumsum(roof.df$differential)
  
  # DISCOUNTED VERSIONS OF CASH FLOW
  discquot <- (1+(discount_pct/100))^(roof.df$year-start_year) # exponent vector begins at zero on purpose
  roof.df$netws_disc <- roof.df$net_with_solar / discquot
  roof.df$netwos_disc <- roof.df$net_wo_solar / discquot
  roof.df$diff_disc <- roof.df$netws_disc - roof.df$netwos_disc
  roof.df$cum_diff_disc <- cumsum(roof.df$diff_disc)
  
  # Find the crossover year (the last year that the differential goes from nonpositive to positive)
  crossover_year <- max(roof.df$year) # Start out with it at the last year; will decrement from there
  if (roof.df$cumul_diff[which(roof.df$year==crossover_year)] <= 0){ # Don't bother further if differential ends negative
    crossover_year <- 9999
  }else{
    continue <- TRUE
    while (continue){
      if ((roof.df$cumul_diff[which(roof.df$year==crossover_year)] <= 0) | (crossover_year == min(roof.df$year))){
        continue <- FALSE
      }else{
        crossover_year <- crossover_year - 1
      }
    }
  crossover_year <- crossover_year + 1 # This corrects for an off-by-one in the logic
  }
  # Old way that is tripped up by having more than one zero crossing
  # crossover_year <- roof.df$year[which(min(abs(roof.df$cumul_diff),
  #                                         na.rm = TRUE) == abs(cumsum(roof.df$differential)))]
  # Interpolate between years to get down to 1/10 year
  p <- roof.df$cumul_diff[which(roof.df$year==crossover_year)]
  q <- roof.df$cumul_diff[which(roof.df$year==crossover_year) - 1]
  breakeven_years <- round_any((crossover_year-min(year_range)) - (p/(p-q)), 0.1 )
  
  # PLOT W/O DISCOUNT RATE
  plot.new() 
  plot(x=roof.df$year,
       y=roof.df$cumul_diff/1000,
       xlab="Year", 
       ylab=paste("Thousand", as.character(dollar_year), "Dollars"),
       ylim=c(1.1*sum(roof.df$net_wo_solar)*0.001, 1.4*abs(max(roof.df$cumul_diff)*0.001)),
       type="l",
       main=scen_name,
       cex.main=0.75)
  lines(x=roof.df$year, y=cumsum(roof.df$net_wo_solar)/1000, type="l", col="orange")
  lines(x=roof.df$year, y=cumsum(roof.df$net_with_solar/1000), type="l", col="blue")
  abline(h=0, col="black", lty=2)
  abline(v=min(roof.df$year) + breakeven_years, col="gray", lty=2)
  legend(start_year,
         0.6*sum(roof.df$net_wo_solar)*0.001,
         legend=c("Difference", "Solar", "No Solar"),
         col=c("black", "blue", "orange"),
         lty=c(1,1,1),
         cex=0.6)
  #Sys.sleep(2)
  nodisc_plot_object <- recordPlot()
  
  # PLOT WITH DISCOUNT-RATE
  plot.new() 
  plot(x=roof.df$year,
       y=roof.df$cum_diff_disc/1000,
       xlab="Year", 
       ylab=paste("Thousand", as.character(dollar_year), "Dollars"),
       ylim=c(1.1*sum(roof.df$netwos_disc)*0.001, 1.4*abs(max(roof.df$cum_diff_disc)*0.001)),
       type="l",
       main=scen_name,
       cex.main=0.75)
  lines(x=roof.df$year, y=cumsum(roof.df$netwos_disc)/1000, type="l", col="orange")
  lines(x=roof.df$year, y=cumsum(roof.df$netws_disc/1000), type="l", col="blue")
  abline(h=0, col="black", lty=2)
  abline(v=min(roof.df$year) + breakeven_years, col="gray", lty=2)
  legend(start_year,
         0.6*sum(roof.df$netwos_disc)*0.001,
         legend=c("Difference", "Solar", "No Solar"),
         col=c("black", "blue", "orange"),
         lty=c(1,1,1),
         cex=0.6)
  #############################################################################################
  
  disc_plot_object <- recordPlot()
  writeLines(scenario[[1]])
  writeLines(paste0("[[", ctr, "]] ",
                   "Differential with/wo solar: ", dollar(sum(roof.df$differential, na.rm = TRUE)),
                   "; Breakeven years: ", breakeven_years))
  writeLines(paste0("      ",
                    "Total CO2 avoided: ", round_any(sum(roof.df$co2_avoided_t), 0.1), "t;",
                    " Net Cost (", dollar_year, "$) to operator per tonne CO2 Avoided: $",
                    -round_any(sum(roof.df$differential, na.rm = TRUE)/sum(roof.df$co2_avoided_t), 0.01)))
  writeLines("")
  output_list[[ctr]] <- list(scen_name, # Text name for the scenario, copied from scenario list
                             nodisc_plot_object, # Plot showing cumulative solar/no-solar nets and diff w/o disc rate
                             disc_plot_object,   # Plot showing cumulative solar/no-solar nets and diff with disc rate
                             roof.df, # The scenario's dataframe
                             breakeven_years, # How many years to break even between solar/no-solar
                             sum(roof.df$differential, na.rm = TRUE) # Differential with/wo solar
                             )
  ctr <- ctr + 1
}
saveRDS(output_list, "output_list.rds")

