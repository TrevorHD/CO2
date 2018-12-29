# ----- Initialise and tidy data ------------------------------------------------------------------

# Load packages to be used
library(tidyverse)
library(grid)
library(gridBase)

# Load data
data <- read.csv("https://raw.githubusercontent.com/TrevorHD/CO2/master/EmissionsByCountry.csv")

# Remove irrelevant columns; rename country name column
data %>% select(-"Country.Code", -"Indicator.Name", -"Indicator.Code") %>% 
         rename("Country" = "ï..Country.Name") -> data

# Remove the following non-countries
# Must do it manually since there really isn't a better way
data <- data[-c(6,      # Arab world
                35,     # Central Europe and the Baltics
                48,     # Caribbean small states
                60,     # East Asia & Pacific (excluding high income)
                61,     # Early-demographic dividend
                62,     # East Asia & Pacific
                63,     # Europe & Central Asia (excluding high income)
                64,     # Europe & Central Asia
                67,     # Euro area
                72,     # European Union
                73,     # Fragile and conflict affected situations
                94,     # High income
                97,     # Heavily indebted poor countries (HIPC)
                101,    # IBRD only
                102,    # IDA & IBRD total
                103,    # IDA total
                104,    # IDA blend
                106,    # IDA only
                109,    # Not classified
                127,    # Latin America & Caribbean (excluding high income)
                133,    # Latin America & Caribbean
                134,    # Least developed countries: UN classification
                135,    # Low income
                138,    # Lower middle income
                139,    # Low & middle income
                141,    # Late-demographic dividend
                152,    # Middle East & North Africa
                155,    # Middle income
                160,    # Middle East & North Africa (excluding high income)
                169,    # North America
                180,    # OECD members
                182,    # Other small states
                190,    # Pre-demographic dividend
                196,    # Pacific island small states
                197,    # Post-demographic dividend
                203,    # South Asia
                214,    # Sub-Saharan Africa (excluding high income)
                216,    # Sub-Saharan Africa
                217,    # Small states
                229,    # East Asia & Pacific (IDA & IBRD countries)
                230,    # Europe & Central Asia (IDA & IBRD countries)
                235,    # Latin America & the Caribbean (IDA & IBRD countries)
                237,    # Middle East & North Africa (IDA & IBRD countries)
                239,    # South Asia (IDA & IBRD)
                240,    # Sub-Saharan Africa (IDA & IBRD countries)
                248,    # Upper middle income
                258), ] # World

# Remove "X" preceding number in year column titles
names(data) <- str_replace(names(data), "X(.*)(.*)(.*)(.*)", "\\1\\2\\3\\4")





# ----- Top 15 CO2-producing countries (total emissions from 1994-2014) ----------------------------

         # Change factors to characters to make ggplot play nice
data %>% mutate_if(is.factor, as.character) %>% 
  
         # Calculate total emissions over the time period; convert from kilotonnes to gigatonnes
         # Also, add dummy variable so that only one bar can be plotted
         mutate("CO2" = as.numeric(rowSums(.[36:56])/1e6),
                "Total" = "Total") %>% 
  
         # Keep only necessary columns; sort and remove countries with NA as total
         select("Country", "CO2", "Total") %>%
         arrange_at("CO2", desc) %>% 
         na.omit() %>%
  
         # Create entry for all other countries and add it to list
         rbind(c("Other", as.numeric(sum(.$CO2[16:length(.$CO2)])), "Total")) %>%
         slice(-c(16:(length(.$CO2) - 1))) %>%
  
         # Emission number was converted to character in process; convert back to numeric
         mutate_at(vars(CO2), funs(as.numeric)) %>%
  
         # Plot everything as a single stacked bar; will need to manually change legend
         ggplot(aes(x = Total, y = CO2, fill = Country, group = "type")) + 
                geom_bar(stat = "identity", position = "fill") +
                scale_y_continuous(labels = scales::percent, expand = c(0.015, 0)) +
                scale_fill_manual(values = c("purple3", "lawngreen", "red3", "olivedrab1", "orange",
                                             "mediumturquoise", "steelblue2", "yellow2", "seagreen3",
                                             "turquoise2", "gray47", "orangered", "purple", 
                                             "slateblue2", "limegreen", "red2")) +
                ylab("Percent of total global CO2 emissions, 1994-2014") +
                theme(legend.position = "none", 
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.text.x = element_text(size = 16, hjust = 0.4,
                                                 margin = margin(t = 5, r = 0, b = 0, l = 0)),
                      axis.text.y = element_blank(),
                      axis.title.x = element_text(size = 26, 
                                                  margin = margin(t = 12, r = 0, b = 0, l = 0)),
                      axis.title.y = element_blank(),
                      axis.ticks.x = element_line(size = 1),
                      axis.ticks.y = element_blank(),
                      axis.ticks.length = unit(8, "points")) +
                coord_flip() -> top15.bargraph





# ----- Top 5 CO2-producing countries (annual emissions from 1994-2014) ---------------------------

         # Calculate total emissions; convert kilotonnes to gigatonnes
data %>% mutate("Total" = as.numeric(rowSums(.[36:56])/1e6)) %>%
         mutate_at(vars(paste0(c(1960:2014))), funs(./1e6)) %>%
  
         # Sort and take only the top 5
         arrange_at("Total", desc) %>% 
         slice(c(1:5)) %>%
  
         # Gather annual emissions for each country; keep only necessary columns
         gather(paste0(c(1994:2014)), key = "Year", value = "CO2") %>%
         select("Country", "Year", "CO2") %>% 
  
         # Re-order factor levels so that countries are plotted in order of emissions
         mutate(Country = factor(Country, levels = c("China", "United States", 
                                                     "Russian Federation","India", "Japan"))) %>%
  
         # Plot area graph; all colours sum to total emissions from these countries in given year
         ggplot(aes(x = Year, y = CO2, group = Country)) + 
                geom_area(stat = "identity", aes(fill = Country)) +
                scale_fill_manual(values = c("red3", "red2", "orangered", "orange", "yellow2")) +
                scale_x_discrete(breaks = seq(1994, 2014, by = 2), expand = c(0.01, 0.01)) +
                scale_y_continuous(minor_breaks = seq(0 , 20, 2), breaks = seq(0, 20, 4), 
                                   expand = c(0.01, 0.01)) +
                ylab("Annual CO2 emissions (billions of tonnes)") + 
                theme(legend.position = "none",
                      panel.grid.major = element_line(colour = "gray74"), 
                      panel.grid.minor = element_line(colour = "gray74"),
                      panel.background = element_rect(fill = "white"),
                      axis.text.x = element_text(size = 14, hjust = 0.6),
                      axis.text.y = element_text(size = 17),
                      axis.title.x = element_blank(),
                      axis.title.y = element_text(margin = margin(t = 0, r = 6, b = 0, l = 0),
                                                  size = 24)) -> top5.areagraph





# ----- Top 5 CO2-producing countries within "other" category (annual emissions from 1994-2014) ---

# Calculate total emissions; convert kilotonnes to gigatonnes
data %>% mutate("Total" = as.numeric(rowSums(.[36:56])/1e6)) %>%
         mutate_at(vars(paste0(c(1960:2014))), funs(./1e6)) %>%
  
         # Sort and take only the top 5
         arrange_at("Total", desc) %>% 
         slice(c(6:15)) %>%
  
         # Gather annual emissions for each country; keep only necessary columns
         gather(paste0(c(1994:2014)), key = "Year", value = "CO2") %>%
         select("Country", "Year", "CO2") %>% 
  
         # Re-order factor levels so that countries are plotted in order of emissions
         mutate(Country = factor(Country, levels = c("Germany", "Canada", "United Kingdom",
                                                     "Korea, Rep.", "Iran, Islamic Rep.",
                                                     "Mexico", "Italy", "South Africa",
                                                     "Saudi Arabia", "Brazil"))) %>%
  
         # Plot area graph; all colours sum to total emissions from these countries in given year
         ggplot(aes(x = Year, y = CO2, group = Country)) + 
                geom_area(stat = "identity", aes(fill = Country)) +
                scale_fill_manual(values = c("olivedrab1", "lawngreen", "limegreen", 
                                             "seagreen3", "mediumturquoise", "turquoise2", 
                                             "steelblue2", "slateblue2", "purple", "purple3")) +
                scale_x_discrete(breaks = seq(1994, 2014, by = 2), expand = c(0.01, 0.01)) +
                scale_y_continuous(minor_breaks = seq(0 , 5, 0.5), breaks = seq(0, 5, 1), 
                                   expand = c(0.01, 0.01)) +
                ylab("Annual CO2 emissions (billions of tonnes)") + 
                theme(legend.position = "none",
                      panel.grid.major = element_line(colour = "gray74"), 
                      panel.grid.minor = element_line(colour = "gray74"),
                      panel.background = element_rect(fill = "white"),
                      axis.text.x = element_text(size = 14, hjust = 0.6),
                      axis.text.y = element_text(size = 17),
                      axis.title.x = element_blank(),
                      axis.title.y = element_text(margin = margin(t = 0, r = 6, b = 0, l = 0),
                                                  size = 24)) -> other10.areagraph





# ----- Plot all graphs in one visualisation ------------------------------------------------------

# Prepare graphics device
jpeg(filename = "CO2Plots.jpeg", width = 2000, height = 1500, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1500, 2000)
pushViewport(viewport(layout = gly))

# Place bar graph on bottom of page
print(top15.bargraph, vp = viewport(layout.pos.row = 1250:1450, layout.pos.col = 1:1995))

# Create borders in which the area graphs will be enclosed
pushViewport(viewport(layout.pos.row = 300:1150, layout.pos.col = 30:975))
grid.rect(gp = gpar(lwd = 2))
popViewport()
pushViewport(viewport(layout.pos.row = 300:1150, layout.pos.col = 1025:1970))
grid.rect(gp = gpar(lwd = 2))
popViewport()

# Place area graphs in borders
print(top5.areagraph, vp = viewport(layout.pos.row = 301:1148, layout.pos.col = 33:972))
print(other10.areagraph, vp = viewport(layout.pos.row = 301:1148, layout.pos.col = 1028:1967))

# Create line segments that indicate area graphs as insets
grid.segments(x0 = c(0.021, 0.572, 0.572, 0.742), y0 = rep(0.153, 4),
              x1 = c(0.015, 0.487, 0.512, 0.986), y1 = rep(0.234, 4), gp = gpar(lwd = 2))
grid.segments(x0 = c(0.021, 0.572, 0.572, 0.742), y0 = rep(0.153, 4),
              x1 = c(0.021, 0.572, 0.572, 0.742), y1 = rep(0.089, 4), gp = gpar(lwd = 2))

# Add text labels for each country
grid.text(c("China", "United States", "Russia", "India", "Japan"), x = rep(0.43, 5),
          y = c(0.625, 0.449, 0.362, 0.315, 0.273), gp = gpar(fontsize = 20))
grid.text(c("Germany", "Canada", "United Kingdom", "South Korea", "Iran", "Mexico", 
            "Italy", "South Africa", "Saudi Arabia", "Brazil"), x = rep(0.92, 10),
          y = c(0.738, 0.68, 0.63, 0.579, 0.52, 0.466, 0.424, 0.381, 0.33, 0.281), 
          gp = gpar(fontsize = 20))

# Add title text
grid.text("Top 15 Countries by Total CO2 Emissions, 1994-2014", x = 0.5, y = 0.9,
          gp = gpar(fontsize = 72))

# Add text for data source
grid.text("Data Sources: World Bank, Carbon Dioxide Information Analysis Center",
          x = 0.87, y = 0.025, gp = gpar(fontsize = 14))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

# Note that graphics device may vary between computers
# Thus, adjustments may need to be made to this code section





# ---------------------------------------------

# Initialise data
file2 <- file.choose()
data2 <- read.csv(file2)

# Remove irrelevant columns and rename country name column
data2 %>% select(-"Country.Code", -"Indicator.Name", -"Indicator.Code") %>% 
          rename("Country" = "ï..Country.Name") -> data2

# Remove the following non-countries
# Must do it manually since there really isn't a better way
data2 <- data2[-c(6,      # Arab world
                  35,     # Central Europe and the Baltics
                  48,     # Caribbean small states
                  60,     # East Asia & Pacific (excluding high income)
                  61,     # Early-demographic dividend
                  62,     # East Asia & Pacific
                  63,     # Europe & Central Asia (excluding high income)
                  64,     # Europe & Central Asia
                  67,     # Euro area
                  72,     # European Union
                  73,     # Fragile and conflict affected situations
                  94,     # High income
                  97,     # Heavily indebted poor countries (HIPC)
                  101,    # IBRD only
                  102,    # IDA & IBRD total
                  103,    # IDA total
                  104,    # IDA blend
                  106,    # IDA only
                  109,    # Not classified
                  127,    # Latin America & Caribbean (excluding high income)
                  133,    # Latin America & Caribbean
                  134,    # Least developed countries: UN classification
                  135,    # Low income
                  138,    # Lower middle income
                  139,    # Low & middle income
                  141,    # Late-demographic dividend
                  152,    # Middle East & North Africa
                  155,    # Middle income
                  160,    # Middle East & North Africa (excluding high income)
                  169,    # North America
                  180,    # OECD members
                  182,    # Other small states
                  190,    # Pre-demographic dividend
                  196,    # Pacific island small states
                  197,    # Post-demographic dividend
                  203,    # South Asia
                  214,    # Sub-Saharan Africa (excluding high income)
                  216,    # Sub-Saharan Africa
                  217,    # Small states
                  229,    # East Asia & Pacific (IDA & IBRD countries)
                  230,    # Europe & Central Asia (IDA & IBRD countries)
                  235,    # Latin America & the Caribbean (IDA & IBRD countries)
                  237,    # Middle East & North Africa (IDA & IBRD countries)
                  239,    # South Asia (IDA & IBRD)
                  240,    # Sub-Saharan Africa (IDA & IBRD countries)
                  248,    # Upper middle income
                  258), ] # World

# Number in year column titles is preceded by "X" for some reason
# Remove the X using regular expresions
names(data2) <- str_replace(names(data2), "X(.*)(.*)(.*)(.*)", "\\1\\2\\3\\4")

# ----- Top 5 CO2-producing countries within "other" category (annual emissions from 1994-2014) ---

# Calculate total emissions and rank by that; same procedure as earlier
data %>% mutate("Total" = as.numeric(rowSums(.[36:56])/1e6)) %>%
         mutate_at(vars(paste0(c(1960:2014))), funs(./1e6)) %>%
         arrange_at("Total", desc) %>% 
         slice(c(1:15)) %>%
         gather(paste0(c(1994:2014)), key = "Year", value = "PC-CO2") %>%
         select("Country", "Year", "PC-CO2") %>% 
  
  # Re-order factor levels so that countries are plotted in order of emissions
  mutate(Country = factor(Country, levels = c("Germany", "Canada", "United Kingdom",
                                              "Korea, Rep.", "Iran, Islamic Rep.",
                                              "Mexico", "Italy", "South Africa",
                                              "Saudi Arabia", "Brazil"))) %>%
