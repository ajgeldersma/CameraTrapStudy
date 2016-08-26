		#  Hurley et al Hierarchal Mule Deer Survival Plotting Functions
		#  Maps functions
		#  Josh Nowak
		#  07/2015
#################################################################################

		multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
			# Multiple plot function
			#
			# ggplot objects can be passed in ..., or to plotlist (as a list of 
			#  ggplot objects)
			# - cols:   Number of columns in layout
			# - layout: A matrix specifying the layout. If present, 'cols' is 
			#    ignored.
			#
			# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
			# then plot 1 will go in the upper left, 2 will go in the upper right, 
			#  and 3 will go all the way across the bottom.
			#

			# Make a list from the ... arguments and plotlist
			plots <- c(list(...), plotlist)

			numPlots = length(plots)

			# If layout is NULL, then use 'cols' to determine layout
			if (is.null(layout)) {
				# Make the panel
				# ncol: Number of columns of plots
				# nrow: Number of rows needed, calculated from # of cols
				layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
								ncol = cols, nrow = ceiling(numPlots/cols))
			}

			if (numPlots==1) {
				print(plots[[1]])

			} else {
				# Set up the page
				grid.newpage()
				pushViewport(viewport(layout = grid.layout(nrow(layout), 
								ncol(layout))))

				# Make each plot, in the correct location
				for (i in 1:numPlots) {
				  # Get the i,j matrix positions of the regions that contain this 
				  #  subplot
				  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

				  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
											  layout.pos.col = matchidx$col))
				}
			}
		}


#################################################################################
		study_area <- function(gmu_border = "gray25",
								gmu_line = 1.1,
								gmu_txt = "gray25",
								gmu_size = 2.5,
								pmu_border = "gray90",
								pmu_line = 1.7,
								pmu_size = 3.4,
								pmu_txt = "white",
								eco_fill = c("green3", "darkgreen", 
												"navajowhite4"),
								bground = "terrain-background"){
			#  A function to create a map of the study area with arguments to
			#  control text_color at the gmu and pmu levels, shading color of
			#  eco's and bground or background map.  The background map that is 
			#  desired and can be any of the ?get_map options under the stamen 
			#  dataset.
			#  Requires an internet connection to run!!!
			#  The function returns nothing, but creates a plot
			load(file.path(getwd(), "data", "gmu_labs.RData"))
			load(file.path(getwd(), "data", "pmu_labs.RData"))
			load(file.path(getwd(), "data", "id_fort.RData"))
			load(file.path(getwd(), "data", "pmu_fort.RData"))			
			load(file.path(getwd(), "data", "map_zoom.RData"))
			
			#  Get background map
			#  All options listed under ?get_map
			bckgrd <- get_map(location = map_zoom, 
						source = "stamen",
						maptype = bground,
						color= "bw")
			
			#  Create plot
			ggmap(bckgrd) +
				geom_polygon(data = id_fort, 
								aes(x = long, y = lat, group = group),
								fill = NA, colour = gmu_border, size = gmu_line)+
				geom_text(data = gmu_labs, aes(x=x, y=y, label = GMU, 
							hjust = 0.5, vjust = 0.5), size = gmu_size, 
							colour = gmu_txt) +
				geom_polygon(data = id_fort[!is.na(id_fort$PMU),], 
								aes(x = long, y = lat, group = group, 
									fill = Ecotype),
								alpha = 0.7) +
				geom_polygon(data = pmu_fort,
								aes(x = long, y = lat, group = group),
								fill = NA, colour = pmu_border, size = pmu_line)+
				geom_text(data = pmu_labs, aes(x=x, y=y, label = PMU, 
							hjust = 0.5, vjust = 0.5, fontface = "bold"), 
							size = pmu_size, colour = pmu_txt) +
				xlab("Latitude") +
				ylab("Longitude") +
				scale_fill_manual(values = eco_fill)		
		}
#################################################################################
		rand_map <- function(model_nm, 
							lower = "red",
							upper = "green",
							pmu_alpha = 0.8,
							pmu_line = "gray",
							phi_txt = "black",
							phi_size = 3,
							intercept = T){
			#  Function takes:
			#  model_nm - name of the model to plot (character string)
			#  lower - the color to give to the unit with the lowest phi
			#  upper - the color to give to the unit with the highest phi, a 
			#   gradient is created between
			#  pmu_alpha - a number between 0 and 1 that dictates how transparent
			#   the fill colors of the pmu are (0 is "clear")
			#  phi_txt - the color of the text of mean phi estimates
			#  phi_size - the size of the text of mean phi estimates
			#  intercept - either T or F, if false then the random slopes will
			#  be plotted, but when true the random intercepts are plotted
							
			#  Get PMU map
			load(file.path(getwd(), "data", "pmu_fort.RData"))
			load(file.path(getwd(), "data", "pmu_labs.RData"))
			
			#  Get model results
			if(!grepl(".RData", model_nm)){
				model_nm <- paste(model_nm, "RData", sep = ".")
			}
			load(file.path(getwd(), "plot_in", model_nm))
			
			#  Mean estimates come at the pmu scale and are weekly, study
			#  covered 6 months or 24 weeks of each year
			if(intercept){
				sds <- apply(plogis(surv.res$BUGS$sims.list$alpha0)^24, 2, sd)
				mudf <- data.frame(PMU = as.character(unique(pmu_fort$id)),
									mu = plogis(surv.res$BUGS$mean$alpha0) ^ 24)
				pmu_fort <- pmu_fort %>%
							mutate(PHI =  mudf$mu[match(pmu_fort$id, mudf$PMU)])
				pmu_labs$PHI <- round(mudf$mu, 2)
				pmu_labs$SD <- round(sds, 2)
			}else{
				sds <- round(surv.res$BUGS$sd$alpha2, 2)
				mudf <- data.frame(PMU = as.character(unique(pmu_fort$id)),
									mu = surv.res$BUGS$mean$alpha2)
				pmu_fort <- pmu_fort %>%
							mutate(PHI =  mudf$mu[match(pmu_fort$id, mudf$PMU)])
				pmu_labs$PHI <- round(mudf$mu, 2)
				pmu_labs$SD <- sds
			}
			
			#  Plot PMU's and add mu to center of each as text
			ggplot() +
				geom_polygon(data = pmu_fort, 
					aes(x = long, y = lat, group = group, fill = PHI), 
						alpha = pmu_alpha, colour = pmu_line) +
				scale_fill_continuous(low = lower, high = upper) +
				geom_text(data = pmu_labs, aes(x=x, y=y, label = PHI, 
							hjust = 0.5, vjust = -1), size = phi_size, 
							colour = phi_txt, fontface = "bold") +
				geom_text(data = pmu_labs, aes(x=x, y=y, 
							label = paste("(SD = ", SD, ")", sep = ""), 
							hjust = 0.5, vjust = 1), size = phi_size, 
							colour = phi_txt) +
				xlab("Longitude") +
				ylab("Latitude") +
				theme_bw() +
				theme(panel.grid.major = element_blank(),
						panel.grid.minor = element_blank(),
						panel.background = element_blank(),
						panel.border = element_blank(),
						legend.position = "none",
						axis.line = element_line(color = "black"))
		
		}
#################################################################################


		