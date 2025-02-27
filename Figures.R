#specify the packages of interest
packages = c("ggplot2","directlabels","reshape", "readxl", "tidytext", "dplyr", "data.table")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded, else installed then loaded.

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})



## Figure 1.2: Provincial Decoupling of GDP and GHG (2005-2018) ##

# Download from URL
decoupling <- fread("https://raw.githubusercontent.com/Canadian-Institute-for-Climate-Choices/11-Ways-to-Measure-Clean-Growth/master/Data/Fig_1.2_Decoupling_GHGs.csv?token=ALA24KOKX76EW2QTK4GFCGS7MJX44") #Uncomment to load from URL


labels <- c(`0 British Columbia`="British Columbia",
            `1 Alberta`="Alberta",
            `2 Saskatchewan`="Saskatchewan",
            `3 Manitoba`="Manitoba",
            `4 Ontario`="Ontario",
            `5 Quebec` ="Quebec",
            `6 New Brunswick`="New Brunswick",
            `7 Nova Scotia`="Nova Scotia", 
            `8 PEI`="P.E.I.", 
            `9 Newfoundland & Labrador`="Newfoundland & Labrador")

ggplot(decoupling, aes(x=Year), group = Place) + 
  geom_line(aes(y = GDP), size=1, color="darkred") +
  geom_line(aes(y = GHG), size=1, color="steelblue") + 
  facet_wrap(. ~ Place, ncol = 2, labeller = labeller(Place = labels)) +
  theme_bw() +
  scale_x_continuous(name="Year", limits=c(2005, 2018), breaks = c(2005, 2009, 2014, 2018)) +
  theme(panel.spacing.x = unit(7, "mm")) +
  scale_y_continuous(name="Change since 2005", limits=c(60, 140), breaks = c(60, 100, 140)) +
  labs(x="Year", y="Change since 2005") +
  #ggtitle("Figure 1.1: Decoupling GHGs from GDP by province (Index)") +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=22,face="bold")) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))




#######
# Figure 7.2: Provincial Decoupling of Employment and GHG (2005-2018) ##
#######

emp_ghg <- fread("https://raw.githubusercontent.com/Canadian-Institute-for-Climate-Choices/11-Ways-to-Measure-Clean-Growth/master/Data/Fig.%207.2_decoupling_Emp_GHG.csv?token=ALA24KIQMK6W24VMSSYTBQS7MJXKY")

labels <- c(`0 British Columbia`="British Columbia",
            `1 Alberta`="Alberta",
            `2 Saskatchewan`="Saskatchewan",
            `3 Manitoba`="Manitoba",
            `4 Ontario`="Ontario",
            `5 Quebec` ="Quebec",
            `6 New Brunswick`="New Brunswick",
            `7 Nova Scotia`="Nova Scotia", 
            `8 PEI`="P.E.I.", 
            `9 Newfoundland & Labrador`="Newfoundland & Labrador")

ggplot(emp_ghg, aes(x=Year), group = Place) + 
  geom_line(aes(y = Employment), size=1, color="darkred") +
  geom_line(aes(y = GHG), size=1, color="steelblue") + 
  facet_wrap(. ~ Place, ncol = 2, labeller = labeller(Place = labels)) +
  theme_bw() +
  #ggtitle("Figure X: Decoupling GHGs from Employment by province (Index)") +
  scale_x_continuous(name="Year", limits=c(2005, 2018), breaks = c(2005, 2009, 2014, 2018)) +
  theme(panel.spacing.x = unit(7, "mm")) +
  scale_y_continuous(name="Change since 2005", limits=c(60, 135), breaks = c(70, 100, 130)) +
  labs(x="Year", y="Change since 2005") +
  #ggtitle("Figure 1.1: Decoupling Employment from GDP by province (Index)") +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=22,face="bold")) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))


## Fig. 7.4 Employment Concentrations

emp_conc <- fread("https://raw.githubusercontent.com/Canadian-Institute-for-Climate-Choices/11-Ways-to-Measure-Clean-Growth/master/Data/Fig.%207.4_CBP%20Emp%20counts.csv?token=ALA24KMWTRPIKTGIP2ARSUS7MJW56")

ggplot(emp_conc[1:20,], aes(x = reorder_within(as.factor(ERNAME), -Concentration, as.factor(NAICS)), Concentration,  fill = as.factor(NAICS))) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0, 0.15), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette="Set3") +
  labs(x="Economic Regions",
       y="Concentration") +
  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.key.size = unit(1.1, "cm"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17),
        axis.ticks = element_blank())


## FIGURE 10.1: Air Quality ##

# Download from URL 
aq <- fread("https://raw.githubusercontent.com/Canadian-Institute-for-Climate-Choices/11-Ways-to-Measure-Clean-Growth/master/Data/Fig.%2010.1_air_quality.csv?token=ALA24KIYVVHYBCQP4R5GCES7MJXB2")

aq <- as.data.frame(aq)

# Reformat data from "wide" to "long"
aq2 <- reshape2::melt(aq, id.vars = "City")


# Add two new columns for CAAQS 2020
aq2 <- aq2 %>%
  filter(value > 0) %>%
  mutate(CAAQS_2020 = ifelse(variable == "PM2.5 (�g/m3)", 8.8, 
                             ifelse(variable == "SO2 (ppb)", 5,
                                    ifelse(variable == "NO2 (ppb)", 17,
                                           ifelse(variable == "O3 (ppb)", 62, 8.8)))) 
  ) %>%
  mutate(CAAQS_2025 = ifelse(variable == "SO2 (ppb)", 4,
                             ifelse(variable == "NO2 (ppb)", 12,
                                    ifelse(variable == "O3 (ppb)", 60, NA)))
  )


my_breaks <- function(x) { if (max(x) < 11 ) seq(0, 10, 1) else seq(0, 60, 10) }


# Make faceted bar graph of air quality data by city
ggplot(aq2, aes(x = reorder_within(City, value, variable), y=value)) + 
  geom_bar(stat = "identity", fill = "#296B75") +
  facet_wrap(.~ variable, scales = "free", ncol = 4) +
  #scale_y_continuous(breaks = my_breaks) +
  scale_x_reordered() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_blank()) +
  theme(strip.text = element_text(size = 20, face = "bold")) +
  theme(axis.ticks = element_blank()) +
  
  geom_hline(aes(yintercept = CAAQS_2020), color = "#A11F7F", size = 2, linetype = "dashed") +
  geom_hline(aes(yintercept = CAAQS_2025), color = "steelblue", size = 2, linetype = "dashed")

