####################################################################################################################
####################################################################################################################
#This code converts UTC Solar data (OMNI 2) to the Local time zone and calculates daily averages for each zip code.#
####################################################################################################################
####################################################################################################################

library(openair)
library(dplyr)
setwd("") # select directory

file = read.csv("omni2_nVi3Ici47B.csv", header = T, sep = ";")  # Solar data
file$orig = paste0(file$YEAR - 1, "-12-31")
file$date = as.Date(file$DOY, origin = file$orig)
file$time = format(
    as.POSIXct(file$Hour * 3600, origin = "1970-01-01", tz = "UTC"),
    "%T"
)
file$date_1 = paste0(file$date, " ", file$time)
file$date_1 <- as.POSIXct(file$date_1, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

gf1 = read.csv("zipcodes_Timezones.csv", header = T, sep = ",")  # file with zip codes and respective time zone
gf2 = unique(gf1$time_zone)  # selecting the time zones
file_2 = NULL
for (i in seq_along(gf2))
    {
    file$LC_time = format(file$date_1, tz = gf2[i], usetz = TRUE)
    file$time_zone = gf2[i]
    file_2 = rbind(file_2, file)
    file$LC_time = NULL
}

####################################################################################################################
################################Indicating NA values for each variable##############################################
####################################################################################################################

file_2$Bartels_rot_.number = ifelse(file_2$Bartels_rot_.number == 9999, NA, file_2$Bartels_rot_.number)
file_2$Scalar_B = ifelse(file_2$Scalar_B == 999.9, NA, file_2$Scalar_B)
file_2$Vector_B_Mag = ifelse(file_2$Vector_B_Mag == 999.9, NA, file_2$Vector_B_Mag)
file_2$BX = ifelse(file_2$BX == 999.9, NA, file_2$BX)
file_2$BY = ifelse(file_2$BY == 999.9, NA, file_2$BY)
file_2$BZ = ifelse(file_2$BZ == 999.9, NA, file_2$BZ)
file_2$SW_Proton_Den = ifelse(file_2$SW_Proton_Den == 999.9, NA, file_2$SW_Proton_Den)
file_2$SW_Plasma_Speed = ifelse(file_2$SW_Plasma_Speed == 9999, NA, file_2$SW_Plasma_Speed)
file_2$Flow_pressure = ifelse(file_2$Flow_pressure == 99.99, NA, file_2$Flow_pressure)
file_2$E_electric_field = ifelse(file_2$E_electric_field == 999.99, NA, file_2$E_electric_field)
file_2$Plasma_Beta = ifelse(file_2$Plasma_Beta == 999.99, NA, file_2$Plasma_Beta)
file_2$Alfen_mach_number = ifelse(file_2$Alfen_mach_number == 999.9, NA, file_2$Alfen_mach_number)
file_2$Kp_index = ifelse(file_2$Kp_index == 99, NA, file_2$Kp_index)
file_2$R_Sunspot = ifelse(file_2$R_Sunspot == 999, NA, file_2$R_Sunspot)
file_2$Dst.index = ifelse(file_2$Dst.index == 99999, NA, file_2$Dst.index)
file_2$ap_index = ifelse(file_2$ap_index == 999, NA, file_2$ap_index)
file_2$f10.7_index = ifelse(file_2$f10.7_index == 999.9, NA, file_2$f10.7_index)
file_2$pc.index = ifelse(file_2$pc.index == 999.9, NA, file_2$pc.index)
file_2$Proton_flux_10 = ifelse(file_2$Proton_flux_10 == 99999.99, NA, file_2$Proton_flux_10)
file_2$Proton_flux_30 = ifelse(file_2$Proton_flux_30 == 99999.99, NA, file_2$Proton_flux_30)
file_2$Proton_flux_60 = ifelse(file_2$Proton_flux_60 == 99999.99, NA, file_2$Proton_flux_60)
# write.csv(file_2,'solardata_timezones_LCtime.csv') #write the file for all
# the time zones

####################################################################################################################
###################This loop is used for calculating daily averages for each time zone##############################
####################################################################################################################

file5 = NULL
for (i in seq_along(gf2))
    {
    file3 = subset(file_2, file_2$time_zone == gf2[i])
    file3$date = as.Date(file3$LC_time, tz = gf2[i], format = "%Y-%m-%d %H:%M:%S")
    file4 = timeAverage(
        file3, avg.time = "day", type = c("time_zone"),
        start.date = "1970-01-01", end.date = "2022-06-30", data.thresh = 75
    )  # calculates daily averages for days with more than 75% of data available
    file5 = rbind(file5, file4)
    file4 = NULL
}
file5 = as.data.frame(file5)
names(file5)[7] <- "IMF"  # changing the name of the variable from Scalar_B to IMF

# choosing variables of interest (date, time_zone, IMF, SW_Proto_Den, Plasma_Beta, Alfen_mach_number, Kp_index,
# R_Sunspot, f10.7_index, and pc.index)
file6 = file5[, c(2, 1, 7, 12, 16, 17, 18, 19, 22, 23)]
file7 = left_join(gf1, file6, by = "time_zone")  # joining files (zip codes and solar data) by time zone
file8 = subset(
    file7, as.Date(file7$date) >=
        as.Date("1980-01-01") &
        as.Date(file7$date) <=
            as.Date("2022-06-30")
)  #subseting file for the period between 1980 and 2022
# write.csv(file8,''Solar_Data_time_zipcode_merge.csv') #saving in .csv
# saveRDS(file8,'Solar_Data_time_zipcode_merge.rds') #saving in .rds