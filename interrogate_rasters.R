# RASTER INTERROGATION FOR LANDUSE, PERCENT IMPERVIOUS AND SOILS
# Ryan Shojinaga, Water Quality Analyst - Oregon DEQ, Watershed Management
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# Interrogate rasters -> output a data frame of values:
# Cell Index, Cell X, Cell Y, Easting, Northing, # Raster 1 value (at cell XY),
# Raster 2 value, ...
# Currently this script is written to interrogate basins, impervious area,
# nlcd land cover/use, and soils permeability.

# LOAD LIBRARIES ----
library(raster)
library(rgdal)
library(sp)
library(svDialogs)
library(ggplot2)
library(zoo)
library(grDevices)
library(grid)
library(gridExtra)
library(lattice)
library(reshape2)

# LOAD DATA ----
# FILE PATHS
pltDir <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middl',
                 'e_Siletz_River_1710020405/005_reporting/figures/modeling_memo')

rasFil <- list()

filNms <- list()

filPth <- choose.files(default = "", caption = "Select files", multi = TRUE)

# Read in each raster selected and name based on the file
for (i in 1 : length(filPth)) {
  
  # Ordered in the list in alp. order  
  rasFil[[i]] <- raster(filPth[[i]]) 
  
  # Set the name in the list to the raster name
  names(rasFil)[i] <- substr(rasFil[[i]]@file@name, 
                             max(gregexpr("\\\\", rasFil[[i]]@file@name)[[1]]) + 1,
                             regexpr(".tif", rasFil[[i]]@file@name) - 1)
  
}

# CLIP RASTERS ----
extDim <- data.frame(cbind(c(rep(0, length(rasFil))),
                           c(rep(0, length(rasFil))),
                           c(rep(0, length(rasFil)))))

names(extDim) = c("x", "y", "prod")

# Find the raster with the smallest extents
for (i in 1 : length(filPth)) {
  
    # Find the width of each raster
    extDim[i, 1] <- rasFil[[i]]@extent@xmax - rasFil[[i]]@extent@xmin
  
    # Find the height of each raster
    extDim[i, 2] <- rasFil[[i]]@extent@ymax - rasFil[[i]]@extent@ymin 
  
    # Product of the dimensions
    extDim[i, 3] <- extDim[i, 1] * extDim[i, 2] 
    
}

extMin <- which.min(extDim[, 3])

for (i in 1 : length(filPth)) {
  
    rasFil[[i]] <- crop(rasFil[[i]], rasFil[[1]])
    
}



# May also need to include aligning the rasters

# Create a data frame of Index, CellX, CellY, East, North, Basin, Imperv, K_sat, NLCD -- COMPLETE ----
# Start with the first raster (basins)
# index <- 1 : length(raster.Files[["basins2"]])
# coord <- as.data.frame(rowColFromCell(raster.Files[["basins2"]], index))
# xy <- as.data.frame(xyFromCell(raster.Files[["basins2"]], index, spatial=TRUE))
# basin <- as.data.frame(raster.Files[["basins2"]][, ])
# imperv <- as.data.frame(raster.Files[["Imperv"]][, ])
# nlcd <- as.data.frame(raster.Files[["nlcd"]][, ])
# ksat <- as.data.frame(raster.Files[["soils_ks5"]][, ])
# scs <- as.data.frame(raster.Files[["soils_scs2"]][, ])
# drg <- as.data.frame(raster.Files[["soils_drg2"]][, ])
# raster.df.wNA <- cbind(index, coord, xy, basin, imperv, nlcd, ksat, scs, drg)
# names(raster.df.wNA) <- c("Index", "Row", "Col", "East", "North", "Basin", "Imperv", "NLCD", "Ksat", "SCS_Grp", "Drng")
# raster.df <- raster.df.wNA[complete.cases(raster.df.wNA[, 7 : 9]), ] # Remove NA
# # Fill "basin" values of 0 with nearest (indexed) value 
# raster.df[which(raster.df[, 6] == 0 | raster.df[, 6] == 128), 6] <- NA
# raster.df$Basin <- as.numeric(na.locf(zoo(raster.df[, 6]), na.rm = FALSE))

# 1) COMPLETE Calculate NLCD areas and total area per basin -- COMPLETE ----
# basins.area <- dcast(raster.df, Basin ~ NLCD, value.var = "NLCD", length)
# basins.area <- rbind(basins.area, basins.area[nrow(basins.area), ])
# basins.area[nrow(basins.area), 1] <- 999
# basins.area$TTL <- 0
# num.row <- nrow(basins.area)
# num.col <- ncol(basins.area)
# for (i in 1 : num.row)
# {
#     basins.area[i, 17] <- sum(basins.area[i, 2 : 16]) # calculates total area per basin (row)
# }
# for (i in 2 : ncol(basins.area))
# {
#     basins.area[num.row, i] <- sum(basins.area[1 : num.row - 1, i]) # calculates total area per LU (col)
# }
# 
# basins.area[ , 2 : num.col] <- round(basins.area[ , 2 : num.col] * 30^2 * 0.000247105, 1) # Total areas - acres
# basins.area[num.row, 17] <- sum(basins.area[1 : num.row, 17])
# basins.area.cells <- basins.area

# 2) COMPLETE Correlate developed areas and percent impervious -- COMPLETE ---- 
# Subset the developed areas and the impervious areas -- Assumption: Only developed lands are impervious;
# That is, all non-developed land (minus water) is 100% pervious; developed codes are 21 - 24 (Open, Low, Med, High).
# imp.dev <- subset(raster.df, NLCD == 21 | NLCD == 22 | NLCD == 23 | NLCD == 24)
# imp.dev$NLCD <- factor(imp.dev$NLCD )
# percent.dev <- round(length(imp.dev$Index) / length(raster.df$Index) * 100, 2)
# imp.dev.summ <- list(NA, NA, NA, NA)
# imp.dev.summ[[1]] <- dcast(imp.dev, Basin ~ NLCD, value.var = "NLCD", length) # Cell count per basin per developed land type
# imp.dev.summ[[2]] <- dcast(imp.dev, Basin ~ NLCD, value.var = "Imperv", sum)
# imp.dev.summ[[2]][, 2 : 5] <- round(imp.dev.summ[[2]][, 2 : 5] / 100, 0) # Percent of first data frame that's impervious
# imp.dev.summ[[3]] <- imp.dev.summ[[2]]
# imp.dev.summ[[3]][, 2 : 5] <- imp.dev.summ[[1]][, 2 : 5] - imp.dev.summ[[2]][, 2 : 5] # Remainder of the first data frame that's pervious
# imp.dev.summ[[4]] <- imp.dev.summ[[2]]
# imp.dev.summ[[4]][, 2 : 5] <- round(imp.dev.summ[[2]][, 2 : 5] / imp.dev.summ[[1]][, 2 : 5] * 100, 1) # % of developed type that's impervious
# names(imp.dev.summ) <- c("TOTAL_DEV", "IMPERV", "PERV", "PCT")

# imp.dev.area <- imp.dev.summ
# for (i in 1 : 3)
# {
#     imp.dev.area[[i]][, 2 : 5] <- round(imp.dev.summ[[i]][, 2 : 5] * 30^2 * 0.000247105, 2) # Converts from grid cell to area in acres
#     names(imp.dev.area[[i]]) <- c("Basin", "Open", "Low", "Med", "High")
# }
# imp.dev.area[[2]]$TTL <- basins.area[1 : 15, 17] # Calculate the percent imperviousness as a function of the total basin area
# imp.dev.area[[2]]$PCT.IMP <- round((imp.dev.area[[2]]$Open + imp.dev.area[[2]]$Low + imp.dev.area[[2]]$Med + imp.dev.area[[2]]$High) /
#                                     imp.dev.area[[2]]$TTL * 100, 1)
# 
# plot.dir <- "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\005_reporting\\figures\\modeling_memo"
# dev.plot <- ggplot(imp.dev, aes(Imperv, fill = NLCD)) + geom_density(adjust = 1.5, alpha = 0.5, size = 0.1) + theme_bw() +
#             theme(legend.position = c(0.8, 0.7), plot.title = element_text(hjust = 0.5)) +
#             scale_fill_manual(name = "DEVELOPED LAND", labels = c("Open Space", "Low Intensity", "Medium Intensity", "High Intensity"),
#                               values = c("gold", "orangered2", "violetred3", "darkviolet")) +
#             labs(title = "PROBABILITY OF IMPERVIOUSNESS ON\nDEVELOPED LAND CATEGORIES", y = "DENSITY (%)", x = "PERCENT IMPERVIOUSNESS")
# ggsave("DensPlt_Dev_vs_Pct_Imperv.png", plot = dev.plot, path = plot.dir, width = 6.5, height = 6.5, units = "in", dpi = 300)


# 3) COMPLETE DENSITY DIAGRAM OF HYDRAULIC CONDUCTIVITIES VS SCS HYDRO GROUP & DRAINAGE -- COMPLETE ----
# To be replaced values
# scs.tab <- as.data.frame(cbind(as.numeric(c(1, 2, 3, 4, 5, 6)),
#                                as.character(c("B", "D", "A", "C", "C/D", "WATER")),
#                                as.numeric(c(2, 5, 1, 3, 4, 6))))
# drg.tab <- as.data.frame(cbind(as.numeric(c(1, 2, 3, 4, 5, 6, 7)),
#                                as.character(c("WLD", "SWX", "POR", "SWP", "MDW", "VPR", "WATER")),
#                                as.numeric(c(3, 2, 6, 5, 4, 6, 7))))
# 
# # Create columns in raster.df for: SCS_Grp, Drng
# raster.df <- merge(raster.df, scs.tab, by.x = "SCS_Grp", by.y = "V1", all.x = TRUE)
# names(raster.df)[names(raster.df) == "V2"] <- "SCS"
# names(raster.df)[names(raster.df) == "V3"] <- "SCSval"
# raster.df <- merge(raster.df, drg.tab, by.x = "Drng", by.y = "V1", all.x = TRUE)
# names(raster.df)[names(raster.df) == "V2"] <- "Drg"
# names(raster.df)[names(raster.df) == "V3"] <- "Drgval"
# raster.df <- raster.df[, -c(1 : 2)]
# SCS.tab <- as.data.frame(table(raster.df$SCS))
# drg.tab <- as.data.frame(table(raster.df$Drg))

#Plot data
# SCS.plot <- ggplot(raster.df, aes(Ksat, fill = SCS)) + geom_density(adjust = 1, alpha = 0.5, size = 0.1) + theme_bw() +
#     theme(legend.position = c(0.8, 0.7), plot.title = element_text(hjust = 0.5)) +
#     labs(title = "DENSITY OF Ksat VALUES BY SCS HYDROLOGIC GROUP", y = "DENSITY (%)", x = "Ksat (in/hr")
# ggsave("DensPlt_SCS_group_vs_Ksat.png", plot = SCS.plot, path = plot.dir, width = 6.5, height = 6.5, units = "in", dpi = 300)
# drg.plot <- ggplot(raster.df, aes(Ksat, fill = Drg)) + geom_density(adjust = 1, alpha = 0.5, size = 0.1) + theme_bw() +
#     theme(legend.position = c(0.8, 0.7), plot.title = element_text(hjust = 0.5)) +
#     labs(title = "DENSITY OF Ksat VALUES BY DRAINAGE PROPERTIES", y = "DENSITY (%)", x = "Ksat (in/hr")
# ggsave("DensPlt_Drain_vs_Ksat.png", plot = drg.plot, path = plot.dir, width = 6.5, height = 6.5, units = "in", dpi = 300)

# scs.B.sub <- raster.df[which(raster.df$SCS == "B"), ]
# scs.dens <- density(scs.B.sub$Ksat)
# scs.Inf <- scs.dens[["x"]][which.max(scs.dens[["y"]])]
# 
# drg.sub <- raster.df[which(raster.df$Drg == "WLD"), ]
# drg.dens <- density(drg.sub$Ksat)
# drg.Inf <- drg.dens[["x"]][which.max(drg.dens[["y"]])]

# 4a) COMPLETE Create Hydrologic Units and calculate areas -- COMPLETE ----
# Use Ksat = 1.32 in/hr as threshold for Hi/Lo permeability groups. This is from the density analysis from above, and corresponds, roughly,
# to a silty loam Ksat. HRUs are based on the following NLCD classifications:
# new.HRU.table <- as.data.frame(cbind(as.numeric(c(11, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95)),
#                                      c("FOR", "DEV", "DEV", "DEV", "DEV", "GRS", "FOR", "FOR", "FOR", "FOR", "GRS", "CRP", "CRP", "FOR", "FOR"),
#                                      as.numeric(c(1, 2, 2, 2, 2, 3, 1, 1, 1, 1, 3, 4, 4, 1, 1))))
# names(new.HRU.table) <- c("NLCD", "HRU", "CDE")
# raster.df <- merge(raster.df, new.HRU.table, by = "NLCD")
# col.ord <- c(2 : 7, 1, 8 : 15)
# raster.df <- raster.df[, col.ord]
# 
# raster.df$perm <- ifelse(raster.df$Ksat < 1.32, 1, 2) # 1 is low permeabililty, 2 is high permeability
# raster.df <- raster.df[order(raster.df$Index), ]
# row.names(raster.df) <- 1 : nrow(raster.df)
# raster.df$HSPF.cd1 <- 2 * as.numeric(raster.df$CDE) - (2 - raster.df$perm)
# 
# HRU.comb <- as.data.frame(cbind(c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#                                 c("FORLO", "FORHI", "DEVLO", "DEVHI", "GRSLO", "GRSHI", "CRPLO", "CRPHI", "IMPRV")))
# 
# raster.df <- merge(raster.df, HRU.comb, by.x = "HSPF.cd1", by.y = "V1", all.x = TRUE)
# colnames(raster.df)[colnames(raster.df) == "V2"] <- "HSPF.cd2"
# col.ord <- c(2 : 17, 1, 18)
# raster.df <- raster.df[, col.ord]
# raster.df <- raster.df[, -c(14 : 16)]


# 4b) RANDOMLY SELECT DEVELOPED CELLS AND CREATE IMPERVIOUS ONES BASED ON PERCENTAGES FROM imp.dev.summ ----

# raster.df, basins.area.cells, imp.dev.summ






# Output the HRU as a raster ----
# raster.out <- raster.df[, c(3 : 4, 15)]
# raster.out <- rbind(raster.out[1, ], raster.out)
# raster.out[1, ] <- c(1, 2, 0)
# raster.out <- dcast(raster.out, Row ~ Col, fun.aggregate = sum, value.var = "HSPF.cd1")
# raster.out[, 1] <- 0
# raster.out <- as(raster.out, "matrix")
# raster.out <- raster(raster.out,
#                      xmn = raster.Files[[1]]@extent@xmin,
#                      xmx = raster.Files[[1]]@extent@xmax,
#                      ymn = raster.Files[[1]]@extent@ymin,
#                      ymx = raster.Files[[1]]@extent@ymax,
#                      crs = raster.Files[["soils_ks5"]]@crs@projargs)
# rast.out.fil <- "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/GIS/001_data/raster/hspf_analysis/siletz_HRU.tif"
# writeRaster(raster.out, rast.out.fil, format = "GTiff", overwrite = TRUE)

# CREATE A TABLE OF BASIN SUMMARY OF HRUS ----
# basins.area.HSPF <- dcast(raster.df, Basin ~ HSPF.cd2, value.var = "HSPF.cd2", length)
# basins.area.HSPF[ , 2 : 9] <- round(basins.area.HSPF[ , 2 : 9] * 30^2 * 0.000247105, 1) # Total areas - acres
# basins.area.HSPF$DEVHI.PCT <- ifelse((basins.area.HSPF$DEVLO + basins.area.HSPF$DEVHI) == 0, 0,
#                                      basins.area.HSPF$DEVHI / (basins.area.HSPF$DEVLO + basins.area.HSPF$DEVHI) * 100)
# basins.area.HSPF$DEVLO.PCT <- ifelse((basins.area.HSPF$DEVLO + basins.area.HSPF$DEVHI) == 0, 0,
#                                      basins.area.HSPF$DEVLO / (basins.area.HSPF$DEVLO + basins.area.HSPF$DEVHI) * 100)
# basins.area.HSPF$IMP.AREA <- 0
# for(i in 1 : 15)
# {
#     basins.area.HSPF[i, 12] <- round(sum(imp.dev.area[["IMPERV"]][i, 2 : 5]), 1)
# }
# 
# basins.area.HSPF$DEVHI <- round(basins.area.HSPF$DEVHI - basins.area.HSPF$IMP.AREA * basins.area.HSPF$DEVHI.PCT / 100, 1)
# basins.area.HSPF$DEVLO <- round(basins.area.HSPF$DEVLO - basins.area.HSPF$IMP.AREA * basins.area.HSPF$DEVLO.PCT / 100, 1)
# basins.area.HSPF <- basins.area.HSPF[, -c(10, 11)]
# 
# basins.area.HSPF <- rbind(basins.area.HSPF, basins.area.HSPF[15, ])
# basins.area.HSPF[16, 1] <- -1
# basins.area.HSPF$TOTAL <- 0
# for (i in 1 : 15) # Sum the rows
# {
#     basins.area.HSPF[i, 11] <- sum(basins.area.HSPF[i, 2 : 10])
# }
# for (i in 1 : 10) # Sum the columns
# {
#     basins.area.HSPF[16, i + 1] <- sum(basins.area.HSPF[1 : 15, i + 1])
# }
# 
# # Output the subbasin -> HRU -> Area (ac) table
# colnames(basins.area.HSPF)[colnames(basins.area.HSPF)=="IMP.AREA"] <- "IMPRV"
# meas.vars <- c("FORLO", "FORHI", "DEVLO", "DEVHI", "GRSLO", "GRSHI", "CRPLO", "CRPHI", "IMPRV")
# HSPF.HRU.summ <- melt(subset(basins.area.HSPF, Basin != -1), id.vars = "Basin", measure.vars = meas.vars)
# names(HSPF.HRU.summ) <- c("Basin", "HRU", "Area_ac")
# HSPF.HRU.summ <- HSPF.HRU.summ[order(HSPF.HRU.summ$Basin), ]
# row.names(HSPF.HRU.summ) <- 1 : nrow(HSPF.HRU.summ)