# Analyze Google Project Sunroof data
# Jeff Hubbs 2021

# SETUP
library("rjson")
setwd("~/Desktop/drawdown/misc/rooftop")
rm(list=ls())

# CONTROLS
# This boolean controls the behavior of the estipower() and fullpower() functions. 
# If TRUE, use the given power value of each bucket. If FALSE, use the midpoint
# between the given power value of each bucket and that of the bucket above
# except for the highest-value bucket, in which case use its given value
floors <- FALSE

# Set a lower limit, irrespective of the setting of the "floors" variable above,
# for the nameplate power of a rooftop solar installation. This is because 
# regardless of whether 0 or the midpoint between 0 and the next bin up is used
# as the value of the lower bin for weighted averaging, there is a practical 
# minimum for how small an installation will ever go onto a rooftop. 
minpower <- 3.0 # Units: kW

# FUNCTIONS
# Function to return the total power (i.e., integral) represented by any of those JSON strings
fullpower <- function(jsonstring){
  prelim <- do.call(cbind, fromJSON(jsonstring, method="C"))
  # Assert lower bound on pre_powers using constant "minpower"
  pre_powers <- pmax(minpower, prelim[1,])
  n <- length(prelim)/2
  if (floors==TRUE){
    powers <- pre_powers
  }else{
    powers <- c((pre_powers[2:n] + pre_powers[1:(n-1)])/2, pre_powers[n])
  } 
  bucketcounts <- prelim[2,]
  return(sum(powers * bucketcounts))
}

# Function to calculate the weighted average of the density distribution described
# by the JSON string
wavgpower <- function(jsonstring){
  prelim <- do.call(cbind, fromJSON(jsonstring, method="C"))
  # Assert lower bound on pre_powers using constant "minpower"
  pre_powers <- pmax(minpower, prelim[1,])
  n <- length(prelim)/2
  if (floors==TRUE){
    powers <- pre_powers
  }else{
    powers <- c((pre_powers[2:n] + pre_powers[1:(n-1)])/2, pre_powers[n])
    # This is a kluge to deal with the case where the JSON string coming in is just one pair long. What
    # can we do?? Just overwrite the "powers" structure with something sensible.
    if (length(pre_powers)==1){powers <- pre_powers}
  } 
  return(weighted.mean(powers, prelim[2,])) # prelim[2,] are the counts and become the weights
}

# Function to replace the power of the lowest bin (generally will be 0) with the maximum of it and minpower
lowlimit <- function(somejay){
  blink <- fromJSON(somejay, method="C")
  blink[[1]][1] <- max(blink[[1]][1], minpower)
  return(toJSON(blink))
}

# Function to take a bucket collection and rewrite it with a bucket for every integer power value covered
deconstruct <- function(injay){
  jlist <- fromJSON(injay, method="C") # Make a list of 2-element vectors (power, count) from the JSON
  jmat <- do.call(cbind, jlist) # Make a matrix from that list (top row is powers, bottom row = counts)
  newlist <- vector("list", 1 + max(jmat[1,])) # Make a new list of length equal to the largest power plus one
  for (q in 1:length(newlist)){newlist[[q]] <- c(q-1, 0)}
  for (q in 1:(length(jlist)-1)){ # Iterate through the bucket list up to the next to last bucket
    thispower <- jmat[1,q]
    nextpower <- jmat[1,q+1]
    newcount <- jmat[2,q]/(nextpower-thispower)
    for (r in (thispower+1):nextpower){newlist[[r]][2]<-newcount}
  }
  newlist[[length(newlist)]][2] <- jmat[2,ncol(jmat)]
  return(toJSON(newlist))
}

# A function to effectively sum two power distribution JSON strings. 
powersum <- function(ajson, bjson){
  decon_a <- fromJSON(deconstruct(ajson), method="C")
  decon_b <- fromJSON(deconstruct(bjson), method="C")
  writeLines("Creating newlist...")
  longest_list_length <- max(decon_a[[length(decon_a)]][1], decon_b[[length(decon_b)]][1])
  newlist <- vector("list", 1 + longest_list_length)
  writeLines("Prewriting newlist...")
  for (q in 1:length(newlist)){newlist[[q]] <- c(q-1, 0)}
  writeLines("Pasting decon_a into newlist...")
  newlist[1:length(decon_a)] <- decon_a
  writeLines("Adding bucket set b to newlist...")
  for (q in 1:length(decon_b)){
    put_it_here <- which(sapply(newlist, function(e) is.element(c(decon_b[[q]][1],0), e))[1,])[1]
    newlist[[put_it_here]][2] <- newlist[[put_it_here]][2] + decon_b[[q]][2]
  }
  # newlist is complete and correct here but let's consolidate the now-one-wide buckets where the
  # count values match
  writeLines(paste0("Consolidating down from ", length(newlist), "..."))
  i <- 1
  while (i < length(newlist)){
    accum <- newlist[[i]][2]
    while (newlist[[i]][2]==newlist[[i+1]][2]){
      accum <- accum + newlist[[i+1]][2]
      newlist <- newlist[-(i+1)]
    }
    newlist[[i]][2] <- accum
    i <- i + 1
  }
  writeLines(paste0("Consolidated to ",length(newlist)))
  writeLines("")
  return(toJSON(newlist))
}


# When we use the buckets below 10kW for residential, we need to trick the fullpower() function's treatment
# of the highest bin into not just using the floor value (the first of the pair in the last bucket) like it's
# written but instead think there's another bucket up at 10kW with a zero count. This function takes the list 
# coming out of the fromJSON() function and appends an element corresponding to a bucket "[10,0]".
tackonten <- function(resjson){return(toJSON(c(fromJSON(resjson),list(c(10,0)))))}

# CONSTANTS
# Project sunroof gives 1.650m x 0.992m = 1.6368m^2 panels
panel_area_sqm <- 1.6368

# READ PROJECT SUNROOF DATA
allsunroof.df <- read.csv("project-sunroof-county.csv", header=TRUE)
# Select for just the Georgia counties
gasunroof.df <- allsunroof.df[allsunroof.df$state_name=="Georgia",]
# Just obliterate Twiggs County; it's negligible and it causes problems
gasunroof.df <- gasunroof.df[!gasunroof.df$region_name=="Twiggs County",]
rm(allsunroof.df)

# INITIIALIZE DATAFRAME
# Google Sunroof metadata says:
# install_size_kw_buckets_json - [Number] of buildings with potential for 
# various installation size buckets. Format is a JSON array, where each element 
# is a tuple containing (1) lower bound of bucket, in kW, and (2) number of 
# buildings in that bucket.
#
# existing_installs_count	- [Number] of buildings estimated to have a solar 
# installation, at time of data collection
#
# yearly_sunlight_kwh_total	- total solar energy generation potential for all roof space in that region
#
# yearly_sunlight_kwh_s & yearly_sunlight_kwh_f - same, but for south-facing and flat roof space


rm(rooftop.df)
# Convert "Xxxxx County" to "Xxxxx" on the fly
# gen_potl column is the all-surface "total solar energy generation potential" in kilowatts
# sfspace_gen column is the south-facing and flat "total solar energy generation potential" in kilowatts
rooftop.df <- data.frame(name=sub(" County", "", gasunroof.df$region_name), 
                         buckets=gasunroof.df$install_size_kw_buckets_json,
                         allspace_gen=gasunroof.df$yearly_sunlight_kwh_total / (365.25 * 24),
                         sfspace_gen=(gasunroof.df$yearly_sunlight_kwh_s + gasunroof.df$yearly_sunlight_kwh_f) / (365.25 * 24),
                         south_panels=gasunroof.df$number_of_panels_s,
                         flat_panels=gasunroof.df$number_of_panels_f,
                         total_panels=gasunroof.df$number_of_panels_total,
                         existing_count=gasunroof.df$existing_installs_count,
                         kw_total=gasunroof.df$kw_total)

# EXTEND DATAFRAME

# We're going to create a new field in the dataframe that is just the first two buckets of the potential
# rooftop power distribution. We assume that any system under 10kW is residential. First: a function to 
#convert the whole JSON bucket string to a new one with just the first two buckets.
firsttwo <- function(longjson){return(tackonten(toJSON(fromJSON(longjson)[1:2])))}
# And now a function to return *all but* the first two buckets.
allbutfirsttwo <- function(longjson){
  fj <- fromJSON(longjson)
  return(toJSON(fj[3:length(fj)]))
}
# Use those functions to create new JSON strings for each of residential and commercial
rooftop.df$res_buckets <- unlist(lapply(rooftop.df$buckets, firsttwo), use.names=FALSE)
rooftop.df$com_buckets <- unlist(lapply(rooftop.df$buckets, allbutfirsttwo), use.names=FALSE)

# We need a capacity factor to be able to connect system nameplate powers to their effective generated power.
# We have two columns in the dataframe now that can give aggregate nameplate (buckets) and aggregate generation
# (allspace_gen) that are both predicated on the use of all roof surfaces. We won't actually use all-surface 
# powers for anything because we don't believe that people will realistically install rooftop systems that way
# but we need nameplate and effective powers to be on the same terms if we are to obtain a capacity factor from
# them.
the_fullpowers <- unlist(lapply(rooftop.df$buckets,fullpower), use.names=FALSE)
sf_panel_ratios <- (rooftop.df$south_panels + rooftop.df$flat_panels) / rooftop.df$total_panels
# OLD/WRONG WAY rooftop.df$cap_fac <- rooftop.df$allspace_gen / the_fullpowers
rooftop.df$cap_fac <- rooftop.df$sfspace_gen / (the_fullpowers * sf_panel_ratios)


# That gives us each county's rooftop solar capacity factor but we also want a single number to characterize
# the whole state's capacity factor. We can't just average the values giving them equal weight so first let's
# get counts for the number of systems shown in the buckets and we'll use those to weight the average of 
# capacity factors.
howmany <- function(thejson){return(sum(do.call(cbind, fromJSON(thejson, method="C"))[2,]))}
# NOT USING THIS ANYMORE - SEE PART AT BOTTOM THAT AGGREGATES BUCKETS FROM ALL COUNTIES
# statewide_cap_fac <- weighted.mean(rooftop.df$cap_fac,
#                                    unlist(lapply(rooftop.df$buckets,howmany), use.names=FALSE))

# To get total powers for residential and commercial generation separately, we've split the buckets up into two new 
# sets, one of 0-10kW and another >=1-kW. But those are all-surface nameplate powers so we need to multiply
# the integrated powers we get by a south-flat-surface/all-surface ratio. We already have that by county. We
# also need to use the by-county capacity factors because the res and com buckets have nameplate powers,
# The numbers below are the maximum reasonable effective generation powers for all available residential
# and commercial buildings, using only the south-facing and flat surfaces.
rooftop.df$sf_all_ratios <- (rooftop.df$sfspace_gen/rooftop.df$allspace_gen)
rooftop.df$max_res_gen <- unlist(lapply(rooftop.df$res_buckets,fullpower)) * rooftop.df$sf_all_ratios * rooftop.df$cap_fac
rooftop.df$max_com_gen <- unlist(lapply(rooftop.df$com_buckets,fullpower)) * rooftop.df$sf_all_ratios * rooftop.df$cap_fac
rooftop.df$max_all_sf_capacity <- unlist(lapply(rooftop.df$buckets,fullpower)) * rooftop.df$sf_all_ratios
# This next one is not necessary but it will give you numbers very close to rooftop.df$allspace_gen
# rooftop.df$max_gen <- unlist(lapply(rooftop.df$buckets,fullpower)) * rooftop.df$sf_all_ratios * rooftop.df$cap_fac


# PANEL AREA
# Determine the total amount of maximal panel area on south-facing and flat roofs
total_panel_area_sqkm <- sum(rooftop.df$south_panels,rooftop.df$flat_panels) * panel_area_sqm * 1E-06

# WEIGHTED AVERAGES
# New columns for weighted averages of the bucket sets: all, residential, commercial
rooftop.df$wavg_buck_all <- unlist(lapply(rooftop.df$buckets, wavgpower))
rooftop.df$wavg_buck_res <- unlist(lapply(rooftop.df$res_buckets, wavgpower))
rooftop.df$wavg_buck_com <- unlist(lapply(rooftop.df$com_buckets, wavgpower))

# SYSTEM COUNTS & EXISTING RESIDENTIAL POWER
allpotcounts <- unlist(lapply(rooftop.df$buckets, howmany), use.names=FALSE) # All potential systems
residpotcounts <- unlist(lapply(rooftop.df$res_buckets, howmany), use.names=FALSE) # All residential systems
# Use ratio between those two to estimate how many residential systems exist
rooftop.df$exist_res_count <- (residpotcounts/allpotcounts) * rooftop.df$existing_count
# Use that to estimate existing residential system power
rooftop.df$exist_res_cap_power <- rooftop.df$exist_res_count *
                                  unlist(lapply(rooftop.df$res_buckets, wavgpower), use.names=FALSE) *
                                  rooftop.df$sf_all_ratios
rooftop.df$exist_com_cap_power <- (rooftop.df$existing_count - rooftop.df$exist_res_count) *
                                  unlist(lapply(rooftop.df$com_buckets, wavgpower), use.names=FALSE) *
                                  rooftop.df$sf_all_ratios
rooftop.df$exist_all_cap_power <- rooftop.df$existing_count *
                                  unlist(lapply(rooftop.df$buckets, wavgpower), use.names=FALSE) *
                                  rooftop.df$sf_all_ratios

# OUTPUT
{
  #writeLines(sprintf("Statewide capacity factor weighted average: %.4f", statewide_cap_fac))
  writeLines(sprintf("Maximal (tech-pot) nameplate capacity for south-facing & flat surfaces: %.2fGW",
                     c(sum(rooftop.df$max_all_sf_capacity)/1E06)))
  writeLines(sprintf("Maximal south-facing and flat surface generated power, residential: %.3fGW = %.2fTWh/yr",
                     c(sum(rooftop.df$max_res_gen)/1E06), sum(rooftop.df$max_res_gen)* 8766* 1E-09))
  writeLines(sprintf("Maximal south-facing and flat surface generated power, commercial: %.2fGW = %.2fTWh/yr",
                     c(sum(rooftop.df$max_com_gen)/1E06), sum(rooftop.df$max_com_gen) * 8766*1E-09))
  
  writeLines(sprintf("Total number of existing systems: %d", sum(rooftop.df$existing_count)))
  writeLines(sprintf("Total number of existing residential systems: %.0f", sum(rooftop.df$exist_res_count)))
  writeLines(sprintf("Total capacity power of existing residential systems: %.0fkW", sum(rooftop.df$exist_res_cap_power)))
  writeLines(sprintf("Total capacity power of existing commercial systems:  %.0fkW", sum(rooftop.df$exist_com_cap_power)))
  writeLines(sprintf("Total capacity power of all existing systems: %.0fkW", sum(rooftop.df$existing_count *
                                                                           +     unlist(lapply(rooftop.df$buckets,
                                                                                               wavgpower),
                                                                                        use.names=FALSE) *
                                                                           +     rooftop.df$sf_all_ratios)))
}
# Write out a new CSV
write.csv(rooftop.df, file="google_sunroof_based_estimates.csv", row.names=FALSE)

###################################################################################################
# This code will aggregate the capacity power distributions for all counties. Careful; it's slow.
agg <- lowlimit(rooftop.df$buckets[1])
for (k in 2:nrow(rooftop.df)){
  writeLines(paste0("County=", rooftop.df$name[k], " k=", k))
  agg <- powersum(agg, lowlimit(rooftop.df$buckets[k]))
}
# That being done, calculate the statewide capacity factor
all_state_cap_fac <- sum(rooftop.df$sfspace_gen)/
                     (fullpower(agg)*(((sum(rooftop.df$south_panels+rooftop.df$flat_panels)/sum(rooftop.df$total_panels)))))
writeLines(sprintf("Statewide capacity factor weighted average: %.4f", all_state_cap_fac))
