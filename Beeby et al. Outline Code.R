# BEEBY ET AL.OUTLINE CODE
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Set working directory
setwd("~/Dropbox")
#
# Load packages
library(readr)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(vegan)
library(caTools)
library(corrplot)
library(MASS)
library(zoo)
library(car)
library(MetBrewer)
library(forcats)
library(ggforce)
library(electivity)
#
# FIGURE1: DIETARY FOODS
#
Feeding_Totals_PropSite <- read_csv("Beeby_Feeding Proportions.csv")
PPMGV <- Feeding_Totals_PropSite %>%
  filter(site == "MGV" & duration_percent >= 1) %>%
  ggplot(., aes(x=reorder(taxon, -duration_percent), y=duration_percent)) +
  geom_col(aes(fill = reorder(taxon, -duration_percent)), position = "dodge") +
  labs(x = "Taxon", y = "Proportion of Feeding Time (%)") +
  scale_fill_viridis_d(direction=1) +
  theme_classic() + ylim(0,14) + theme(aspect.ratio=1.1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none", axis.title.x = element_blank())
PPMGV
plot_build <- ggplot_build(PPMGV)
fill_colors <- plot_build$data[[1]]$fill
# custom colors
MGV_colors <- c("Nonoka"="#440154FF", "Rahiaka"="#471164FF", "Lafa"="#481F70FF", "Vahimberana"="#472D7BFF", "Rotramena"="#443A83FF", "Rotra"="#443A83FF", "Rotrafotsy"="#404688FF",
                "Tavolomanitra"="#3B528BFF", "Fandramanana"="#365D8DFF", "Voamalombotaholahy"="#31688EFF", "Tavolopina"="#2C728EFF", "Amontana"="#287C8EFF", "Amotana"="#287C8EFF", 
                "Sary"="#24868EFF", "Natojabo"="#21908CFF", "Solaitra"="#1F9A8AFF", "Fanorafa"="#20A486FF", "Tavolomaintso"="#27AD81FF", "Tavolo"="#35B779FF", 
                "Vandrika"="#47C16EFF", "Fantsikahitra"="#5DC863FF", "Faritraty"="#75D054FF", "Ramiavontoloho"="#8FD744FF", "Vahipisorona"="#AADC32FF", "Voara"="#C7E020FF",
                "Vahitamboro"="#E3E418FF", "Mahanoro"="#FDE725FF", "Tavolomolaliambo"="#FDE725FF", "Valotra"="#FDE725FF", "Ramandriona"="#FDE725FF", "Apana"="#FDE725FF", 
                "Lanarivatsilana"="#FDE725FF", "Voatakaboka"="#FDE725FF", "Tongolahy"="#FDE725FF")
#
PPTLK <- Feeding_Totals_PropSite %>%
  filter(site == "TLK" & duration_percent >= 1) %>%
  ggplot(., aes(x=reorder(taxon, -duration_percent), y=duration_percent)) +
  geom_col(aes(fill = reorder(taxon, -duration_percent)), position = "dodge") +
  labs(x = "Taxon", y = "Proportion of Feeding Time (%)") +
  scale_fill_manual(values = MGV_colors) +
  theme_classic() + ylim(0,14) + theme(aspect.ratio=1.25) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none", axis.title.x = element_blank())
PPTLK
#
Diets <- ggarrange(PPMGV, PPTLK, nrow=1, ncol=2)
Diets
#
#
# FIGURE 2: ELECTIVITY INDICES
#
Feeding_Phenology <- read_csv("Beeby_Feeding_Phenology.csv")
FR_Indices <- vs_electivity(Feeding_Phenology$mean_fruit_prop2, Feeding_Phenology$fr_fai_prop2, na.rm = TRUE) %>% as.data.frame()
LE_Indices <- vs_electivity(Feeding_Phenology$mean_leaf_prop2, Feeding_Phenology$le_fai_prop2, na.rm = TRUE) %>% as.data.frame()
BU_Indices <- vs_electivity(Feeding_Phenology$mean_bud_prop2, Feeding_Phenology$bu_fai_prop2, na.rm = TRUE) %>% as.data.frame()
FL_Indices <- vs_electivity(Feeding_Phenology$mean_flower_prop2, Feeding_Phenology$fl_fai_prop2, na.rm = TRUE) %>% as.data.frame()
Electivity_Values <- read_csv("Beeby_Electivity.csv")
Electivity_Values <- Electivity_Values %>% pivot_longer(cols = -c(month, site), names_to = "plant_part", values_to = "electivity_value")
Electivity_Values$site <- factor(Electivity_Values$site, levels = c("MGV", "TLK"))
# Month 7 removed due to no data collection for phenology
Electivity_Plot <- Electivity_Values %>% filter(!month=="7") %>% filter(!plant_part=="bu_selectivity") %>% ggplot(., aes(x = month, y = electivity_value)) + 
  geom_point(aes(color = site), size = 2) + geom_line(aes(color = site, group = site)) +
  labs(x = "Month", y = "Electivity Value") + ylim(-1,1) +
  scale_color_manual(values = c("MGV" = "purple4", "TLK" = "seagreen4")) + scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels = c("A","S","O","N","D","J","F*","M","A","M","J","J")) +
  theme_classic() + geom_abline(slope = 0, intercept = 0, color = "darkgrey") + facet_wrap(~factor(plant_part, c("fr_selectivity", "fl_selectivity", "le_selectivity")))
Electivity_Plot
#
#
# FIGURE 3: NUTRITIONAL GEOMETRY
#
Nutrient_Intakes_Props <- read_csv("Beeby_Nutrition Data.csv", 
                                   col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                                    fruit_prop = col_number(), leaf_prop = col_number(), flower_prop = col_number(), other_prop = col_number(),
                                                    total_dry_weight = col_number(), amount_food = col_number(), 
                                                    intake_NDF_g = col_number(), intake_ADF_g = col_number(), intake_ADL_g = col_number(), intake_CP_g = col_number(), intake_ADIN_g = col_number(), intake_fat_g = col_number(), intake_TNC_g = col_number(), intake_ash_g = col_number(), intake_sugar_g = col_number(), intake_starch_g = col_number(),
                                                    intake_CP_kcal = col_number(), intake_AP_kcal = col_number(), intake_NPE_kcal = col_number(), intake_energy_kcal = col_number(), NPE_AP_ratio = col_number(),
                                                    RMT_total = col_number(), RMT_ADIN_prop = col_number(), RMT_carb_prop = col_number(), RMT_TNC_prop = col_number(), RMT_fat_prop = col_number(), RMT_sugar_prop = col_number(), RMT_starch_prop = col_number(),
                                                    RMT_ADIN_prop_kcal = col_number(), RMT_carb_prop_kcal = col_number(), RMT_fat_prop_kcal = col_number()))
#
# MGV INTAKES
Nutrient_Intakes_Props_MGV <- Nutrient_Intakes_Props %>%
  filter(!site=="TLK")
RMT0 <- ggplot(data = Nutrient_Intakes_Props_MGV, mapping = aes(x = RMT_ADIN_prop, y = RMT_carb_prop)) +
  geom_point(mapping = aes(x = RMT_ADIN_prop, y = RMT_carb_prop), color = "purple3") +
  coord_fixed(ratio = 1) +
  labs(x = "% Protein (AP)", y = "% Carbohydrate (NDF + TNC)")
RMT0 <- RMT0 + geom_abline(slope = -1, intercept = 100, color = "grey") + geom_abline(slope = -1, intercept = 75, color = "grey") + geom_abline(slope = -1, intercept = 50, color = "grey") + geom_abline(slope = -1, intercept = 25, color = "grey")
RMT0 <- RMT0 + theme_classic() + ggtitle("Primary Forest (MGV) Intakes") + theme(plot.title = element_text(size = 12, face = "bold"))
RMT0 <- RMT0 + scale_x_continuous(expand = c(0, 0), limits = c(0,100)) + scale_y_continuous(expand = c(0, 0), limits = c(0,100))
RMT0 <- RMT0 + annotate(geom="text", x=49, y=49, label="0", color="grey", size = 3)
RMT0 <- RMT0 + annotate(geom="text", x=36, y=36, label="25", color="grey", size = 3)
RMT0 <- RMT0 + annotate(geom="text", x=23.5, y=23.5, label="50", color="grey", size = 3)
RMT0 <- RMT0 + annotate(geom="text", x=11, y=11, label="75", color="grey", size = 3)
RMT0 <- RMT0 + annotate(geom="text", x=55, y=55, label="Fat %", color="darkgrey", size = 4, angle = 315)
RMT0
#
hull_indices <- chull(Nutrient_Intakes_Props_MGV$RMT_ADIN_prop, Nutrient_Intakes_Props_MGV$RMT_carb_prop)
hull_data <- Nutrient_Intakes_Props_MGV[hull_indices, ]
#
# TLK INTAKES
Nutrient_Intakes_Props_TLK <- Nutrient_Intakes_Props %>%
  filter(!site=="MGV")
RMT1 <- ggplot(data = Nutrient_Intakes_Props_TLK, mapping = aes(x = RMT_ADIN_prop, y = RMT_carb_prop)) +
  geom_point(mapping = aes(x = RMT_ADIN_prop, y = RMT_carb_prop), color = "seagreen4", shape=17) +
  coord_fixed(ratio = 1) +
  labs(x = "% Protein (AP)", y = "% Carbohydrate (NDF + TNC)")
RMT1 <- RMT1 + geom_abline(slope = -1, intercept = 100, color = "grey") + geom_abline(slope = -1, intercept = 75, color = "grey") + geom_abline(slope = -1, intercept = 50, color = "grey") + geom_abline(slope = -1, intercept = 25, color = "grey")
RMT1 <- RMT1 + theme_classic() + ggtitle("Secondary Forest (TLK) Intakes") + theme(plot.title = element_text(size = 12, face = "bold"))
RMT1 <- RMT1 + scale_x_continuous(expand = c(0, 0), limits = c(0,100)) + scale_y_continuous(expand = c(0, 0), limits = c(0,100))
RMT1 <- RMT1 + annotate(geom="text", x=49, y=49, label="0", color="grey", size = 3)
RMT1 <- RMT1 + annotate(geom="text", x=36, y=36, label="25", color="grey", size = 3)
RMT1 <- RMT1 + annotate(geom="text", x=23.5, y=23.5, label="50", color="grey", size = 3)
RMT1 <- RMT1 + annotate(geom="text", x=11, y=11, label="75", color="grey", size = 3)
RMT1 <- RMT1 + annotate(geom="text", x=55, y=55, label="Fat %", color="darkgrey", size = 4, angle = 315)
RMT1
#
# TLK FOOD ITEMS
Food_Macronutrients_TLK <- read_csv("Beeby_Food Macronutrients_TLK.csv", 
                                col_types = cols(No = col_number(), Collection = col_date(format = "%d/%m/%Y"), DM = col_number(), NDF = col_number(), ADF = col_number(), ADL = col_number(), CP = col_number(), ADIN = col_number(), fat = col_number(), ash = col_number(), TNC = col_number(), sugar = col_number(), starch = col_number()))
Food_MacronutrientsTLK <- Food_Macronutrients_TLK %>% filter(!NDF=="NA") %>% filter(!TNC=="NA") %>% rowwise() %>% mutate(CARB=sum(TNC+NDF))
RMT2 <- ggplot(data = Food_MacronutrientsTLK, mapping = aes(x = ADIN, y = CARB)) +
  geom_point(mapping = aes(x = ADIN, y = CARB, color = Part)) + coord_fixed(ratio = 1) + labs(x = "% Protein (AP)", y = "% Carbohydrate (NDF + TNC)")
RMT2 <- RMT2 + geom_abline(slope = -1, intercept = 100, color = "grey") + geom_abline(slope = -1, intercept = 75, color = "grey") + geom_abline(slope = -1, intercept = 50, color = "grey") + geom_abline(slope = -1, intercept = 25, color = "grey")
RMT2 <- RMT2 + theme_classic() + ggtitle("Secondary Forest (TLK) Food Items") + theme(legend.position = "none")
RMT2 <- RMT2 + scale_x_continuous(expand = c(0, 0), limits = c(0,100)) + scale_y_continuous(expand = c(0, 0), limits = c(0,100))
RMT2 <- RMT2 + annotate(geom="text", x=49, y=49, label="0", color="grey", size = 3) + annotate(geom="text", x=36, y=36, label="25", color="grey", size = 3) + annotate(geom="text", x=23.5, y=23.5, label="50", color="grey", size = 3) + annotate(geom="text", x=11, y=11, label="75", color="grey", size = 3) + annotate(geom="text", x=55, y=55, label="Fat %", color="darkgrey", size = 4, angle = 315) +
  theme(plot.title = element_text(size = 12, face = "bold")) + scale_colour_manual(values = c("FL"= "royalblue", "FR"= "magenta4", "LE"= "forestgreen", "OT"= "#f5bb50"))
RMT2
#
# MGV FOOD ITEMS
Food_Macronutrients_MGV <- read_csv("Beeby_Food Macronutrients_MGV.csv", 
                                    col_types = cols(No = col_number(), Collection = col_date(format = "%d/%m/%Y"), DM = col_number(), NDF = col_number(), ADF = col_number(), ADL = col_number(), CP = col_number(), ADIN = col_number(), fat = col_number(), ash = col_number(), TNC = col_number(), sugar = col_number(), starch = col_number()))
Food_MacronutrientsMGV <- Food_Macronutrients_MGV %>% filter(!NDF=="NA") %>% filter(!TNC=="NA") %>% filter(!No=="146") %>% rowwise() %>% mutate(CARB=sum(TNC+NDF))
RMT5 <- ggplot(data = Food_MacronutrientsMGV, mapping = aes(x = ADIN, y = CARB)) +
  geom_point(mapping = aes(x = ADIN, y = CARB, color = Part)) + coord_fixed(ratio = 1) + labs(x = "% Protein (AP)", y = "% Carbohydrate (NDF + TNC)")
RMT5 <- RMT5 + geom_abline(slope = -1, intercept = 100, color = "grey") + geom_abline(slope = -1, intercept = 75, color = "grey") + geom_abline(slope = -1, intercept = 50, color = "grey") + geom_abline(slope = -1, intercept = 25, color = "grey")
RMT5 <- RMT5 + theme_classic() + ggtitle("Primary Forest (MGV) Food Items") + theme(legend.position = "none") + scale_x_continuous(expand = c(0, 0), limits = c(0,100)) + scale_y_continuous(expand = c(0, 0), limits = c(0,100))
RMT5 <- RMT5 + annotate(geom="text", x=49, y=49, label="0", color="grey", size = 3) + annotate(geom="text", x=36, y=36, label="25", color="grey", size = 3) + annotate(geom="text", x=23.5, y=23.5, label="50", color="grey", size = 3) + annotate(geom="text", x=11, y=11, label="75", color="grey", size = 3) + annotate(geom="text", x=55, y=55, label="Fat %", color="darkgrey", size = 4, angle = 315) + 
  theme(plot.title = element_text(size = 11, face = "bold")) + scale_colour_manual(values = c("FL"= "royalblue", "FR"= "magenta4", "LE"= "forestgreen", "OT"= "#f5bb50"))
RMT5
#
Combined_Fig <- ggarrange(RMT0, RMT1, RMT5, RMT2, nrow=2, ncol=2)
Combined_Fig
#
Recol_Data <- read_csv("Beeby_Intakes and Foods.csv", col_types = cols(AP_kcal = col_number(), NPE_kcal = col_number(), NPE_AP_ratio = col_number(), AP_per_g = col_number(), NPE_per_g = col_number()))
View(Recol_Data)
Recol_Data <- Recol_Data %>% filter(!type=="intake") %>% filter(AP_per_g>0) %>% filter(AP_per_g<1)
NPEAPrecol <- ggplot(data = Recol_Data, mapping = aes(x = AP_per_g, y = NPE_per_g, color = site, shape = site)) +
  geom_point() +
  coord_fixed(ratio = 0.13) + 
  labs(x = "AP per gram", y = "NPE per gram") + scale_color_manual(values = c("TLK" = "seagreen4", "MGV" = "purple3")) +
  theme_classic() + theme(legend.position = "right") + theme(plot.title = element_text(size = 11, face = "bold")) + ggtitle("NPE:AP Ratios across Sites")
NPEAPrecol <- NPEAPrecol + geom_abline(slope=4.070, intercept=0, linetype="dashed") + annotate("text", label = "4.1:1", x = 0.82, y = 2.9)
NPEAPrecol <- NPEAPrecol + geom_abline(slope=6.7635, intercept=0) + annotate("text", label = "6.8:1", x = 0.65, y = 5.2)
NPEAPrecol <- NPEAPrecol + geom_abline(slope=28.214, intercept=0, linetype="dashed")  + annotate("text", label = "28.2:1", x = 0.36, y = 7)
NPEAPrecol
#
Recol_Data2 <- Recol_Data %>% filter(!site=="MGV")
NPEAPrecol2 <- ggplot(data = Recol_Data2, mapping = aes(x = AP_per_g, y = NPE_per_g, color = part, shape = part)) +
  geom_point() +
  coord_fixed(ratio = 0.13) + 
  labs(x = "AP per gram", y = "NPE per gram") + scale_colour_manual(values = c("FL"= "royalblue", "FR"= "magenta4", "LE"= "forestgreen", "OT"= "#f5bb50")) + 
  theme_classic() + theme(legend.position = "right") + theme(plot.title = element_text(size = 11, face = "bold")) + ggtitle("NPE:AP Ratios of Food Items")
NPEAPrecol2 <- NPEAPrecol2 + geom_abline(slope=4.070, intercept=0, linetype="dashed") + annotate("text", label = "4.1:1", x = 0.82, y = 2.9)
NPEAPrecol2 <- NPEAPrecol2 + geom_abline(slope=6.7635, intercept=0) + annotate("text", label = "6.8:1", x = 0.65, y = 5.2)
NPEAPrecol2 <- NPEAPrecol2 + geom_abline(slope=28.214, intercept=0, linetype="dashed")  + annotate("text", label = "28.2:1", x = 0.36, y = 7)
NPEAPrecol2
#
NPEAP_plots <- ggarrange(NPEAPrecol, NPEAPrecol2, nrow = 1, ncol = 2)
NPEAP_plots
#
# 
# FIGURE 4: TLK RECOLONISATION FOODS
#
# NPE:AP top 50% foods in diet
Food_MacronutrientsTLK_top50 <- Food_Macronutrients_TLK %>%
  filter(Vernacular=="Tavolomanitra"|Vernacular=="Rahiaka"|Vernacular=="Vahimberana"|Vernacular=="Rotra"|Vernacular=="Tavolomilaliambo") %>%
  rowwise() %>% mutate(CARB=sum(TNC+NDF))
RMT3 <- ggplot(data = Food_MacronutrientsTLK_top50, mapping = aes(x = ADIN, y = CARB)) +
  geom_point(mapping = aes(x = ADIN, y = CARB, color = Part)) +
  coord_fixed(ratio = 1) +
  labs(x = "% Protein (AP)", y = "% Carbohydrate (NDF + TNC)")
RMT3 <- RMT3 + geom_abline(slope = -1, intercept = 100, color = "grey") + geom_abline(slope = -1, intercept = 75, color = "grey") + geom_abline(slope = -1, intercept = 50, color = "grey") + geom_abline(slope = -1, intercept = 25, color = "grey")
RMT3 <- RMT3 + theme_classic() + ggtitle("TLK top 50% Food Items")
RMT3 <- RMT3 + scale_x_continuous(expand = c(0, 0), limits = c(0,100)) + scale_y_continuous(expand = c(0, 0), limits = c(0,100))
RMT3 <- RMT3 + annotate(geom="text", x=49, y=49, label="0", color="grey", size = 3)
RMT3 <- RMT3 + annotate(geom="text", x=36, y=36, label="25", color="grey", size = 3)
RMT3 <- RMT3 + annotate(geom="text", x=23.5, y=23.5, label="50", color="grey", size = 3)
RMT3 <- RMT3 + annotate(geom="text", x=11, y=11, label="75", color="grey", size = 3)
RMT3 <- RMT3 + annotate(geom="text", x=55, y=55, label="Fat %", color="darkgrey", size = 4, angle = 315)
RMT3 <- RMT3 + theme(plot.title = element_text(size = 11, face = "bold")) + scale_colour_manual(values = c("FL"= "royalblue", "FR"= "magenta4", "LE"= "forestgreen"))
RMT3
RMT3poly <- RMT3 + geom_polygon(data = hull_data, aes(x = RMT_ADIN_prop, y = RMT_carb_prop), fill = "gray", alpha = 0.7)
RMT3poly
#
# NPE:AP top 75% foods in diet
Food_MacronutrientsTLK_top75 <- Food_Macronutrients_TLK %>%
  filter(Vernacular=="Tavolomanitra"|Vernacular=="Rahiaka"|Vernacular=="Vahimberana"|Vernacular=="Rotra"|Vernacular=="Tavolomilaliambo"|Vernacular=="Nonoka"|Vernacular=="Voara"|Vernacular=="Tavolopina"|Vernacular=="Amotana"|Vernacular=="Valotra"|Vernacular=="Tavolo"|Vernacular=="Ramandriona"|Vernacular=="Tavolomaintso") %>%
  rowwise() %>% mutate(CARB=sum(TNC+NDF))
RMT4 <- ggplot(data = Food_MacronutrientsTLK_top75, mapping = aes(x = ADIN, y = CARB)) +
  geom_point(mapping = aes(x = ADIN, y = CARB, color = Part)) +
  coord_fixed(ratio = 1) +
  labs(x = "% Protein (AP)", y = "% Carbohydrate (NDF + TNC)")
RMT4 <- RMT4 + geom_abline(slope = -1, intercept = 100, color = "grey") + geom_abline(slope = -1, intercept = 75, color = "grey") + geom_abline(slope = -1, intercept = 50, color = "grey") + geom_abline(slope = -1, intercept = 25, color = "grey")
RMT4 <- RMT4 + theme_classic() + ggtitle("TLK top 75% Food Items")
RMT4 <- RMT4 + scale_x_continuous(expand = c(0, 0), limits = c(0,100)) + scale_y_continuous(expand = c(0, 0), limits = c(0,100))
RMT4 <- RMT4 + annotate(geom="text", x=49, y=49, label="0", color="grey", size = 3)
RMT4 <- RMT4 + annotate(geom="text", x=36, y=36, label="25", color="grey", size = 3)
RMT4 <- RMT4 + annotate(geom="text", x=23.5, y=23.5, label="50", color="grey", size = 3)
RMT4 <- RMT4 + annotate(geom="text", x=11, y=11, label="75", color="grey", size = 3)
RMT4 <- RMT4 + annotate(geom="text", x=55, y=55, label="Fat %", color="darkgrey", size = 4, angle = 315)
RMT4 <- RMT4 + theme(plot.title = element_text(size = 11, face = "bold")) + scale_colour_manual(values = c("FL"= "royalblue", "FR"= "magenta4", "LE"= "forestgreen"))
RMT4
RMT4poly <- RMT4 + geom_polygon(data = hull_data, aes(x = RMT_ADIN_prop, y = RMT_carb_prop), fill = "gray", alpha = 0.7)
RMT4poly
#
TLK_foods_balance <- ggarrange(RMT3poly, RMT4poly, nrow=1, ncol=2)
TLK_foods_balance
#
#
# END