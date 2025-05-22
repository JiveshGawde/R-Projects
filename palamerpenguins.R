library(tidyverse)
library(palmerpenguins)
library(ggthemes)

penguins
glimpse(penguins)


ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)
       ) +
  geom_point(mapping = aes(color = species, shape = species)
             ) +
  geom_smooth(method = 'lm') + 
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body Mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()


dim(penguins)

?penguins

##Make a scatterplot of bill_depth_mm vs. bill_length_mm. That is, make a scatterplot 
#with bill_depth_mm on the y-axis and bill_length_mm 
#on the x-axis. Describe the relationship between these two variables.

ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(
    mapping = aes(color = species, shape = species),
    na.rm = TRUE
    ) +
  labs(
    x = "Bill length (mm)",
    y = "Bill Depth (mm)",
    title = "Bill length and bill depth",
    subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins",
    color = "Species",
    shape = "Species"
  ) +
  scale_color_colorblind()


# What happens if you make a scatterplot of species vs. bill_depth_mm? 
 # What might be a better choice of geom?

ggplot(
  data = penguins,
  mapping = aes(x = species, y = bill_depth_mm)
) +
  geom_boxplot(
    mapping = aes(fill = species)
    ) +
  labs(
    title="Species and bill depth",
    subtitle="Dimensions for Adelie, Chinstrap and Gentoo penguins",
    caption = "Data come from the palmerpenguins package.",
    x = "Species",
    y = "Bill depth (mm)",
    color = "Species",
    shape = "Species"
  ) +
  scale_color_colorblind()

#Recreate the following visualization. What aesthetic should bill_depth_mm be 
#mapped to? And should it be mapped at the global level or at the geom level?

ggplot(
  data = penguins,
  mapping = aes(x=flipper_length_mm, y=body_mass_g)
) +
  geom_point(
    mapping = aes(color = bill_depth_mm),
    na.rm = TRUE
    ) +
  geom_smooth(
    method = "gam",
    na.rm = TRUE,
    formula = y ~ s(x)
    ) +
  labs(
    title="Body mass and flipper length",
    subtitle="Dimensions for Adelie, Chinstrap and Gentoo penguins",
    caption = "Data come from the palmerpenguins package.",
    x = "Flipper length (mm)",
    y = "Body Mass (g)",
    color = "Bill depth (mm)",
    shape = "Bill depth (mm)"
  )


ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)


penguins %>% 
  ggplot(
    aes(x = fct_infreq(species), fill = species)
  ) +
  geom_bar()

ggplot(penguins, aes(x = body_mass_g, fill=species)) +
  geom_histogram(binwidth = 200, na.rm = TRUE)


ggplot(penguins, aes(x = body_mass_g, fill = species)) +
  geom_histogram(binwidth = 200, na.rm = TRUE, color = "black", alpha = 0.9) +
  labs(
    title = paste("Distribution of Body Mass in Penguins"),
    x = "Body Mass (g)",
    y = "Count"
  ) +
  theme(
    plot.background = element_rect(fill = "black", color = NA),  
    panel.background = element_rect(fill = "black", color = NA),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill="black", color = NA)
  )

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density(na.rm = TRUE)

ggplot(na.omit(penguins), aes(x = body_mass_g, fill = species, color = species)) +
  geom_density(alpha = 0.7, linewidth = 1) +
  scale_y_continuous(labels = scales::label_number()) +
  theme(
    plot.background = element_rect(fill = "black", color = NA),  
    panel.background = element_rect(fill = "black", color = NA),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill="black", color = NA)
  ) +
  scale_fill_manual(values = c("Adelie" = "#FF6B6B", "Chinstrap" = "#6BFFB3", "Gentoo" = "#6B9BFF")) +  
  scale_color_manual(values = c("Adelie" = "#FF6B6B", "Chinstrap" = "#6BFFB3", "Gentoo" = "#6B9BFF")) +
  labs(
    title = "Density Distribution of Penguin Body Mass",
    x = "Body Mass (g)",
    y = "Density"
  )

ggplot(
  data = penguins,
  mapping = aes(x = island, fill = species)
) +
  geom_bar()

ggplot(
  data = penguins,
  mapping = aes(x = island, fill = species)
) +
  geom_bar(position = "fill")

ggplot(
  data = penguins,
  mapping = aes(x=flipper_length_mm, y = body_mass_g) 
) +
  geom_point(
    aes(color = species, shape = island),
    na.rm = TRUE
  ) +
  labs(
    title="Body mass and flipper length",
    subtitle="Dimensions for Adelie, Chinstrap and Gentoo penguins",
    caption = "Data come from the palmerpenguins package.",
    x = "Flipper length (mm)",
    y = "Body Mass (g)",
    color = "Species",
    shape = "Island"
  )

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point(
    aes(color = species, shape = species),
    na.rm = TRUE
  ) +
  facet_wrap(~island) +
  labs(
    title="Body mass and flipper length",
    subtitle="Dimensions for Adelie, Chinstrap and Gentoo penguins",
    caption = "Data come from the palmerpenguins package.",
    x = "Flipper length (mm)",
    y = "Body Mass (g)",
    color = "Species",
    shape = "Species"
  )

# The mpg data frame that is bundled with the ggplot2 package contains 234 
#observations collected by the US Environmental Protection Agency on 38 car models.
#Which variables in mpg are categorical? Which variables are numerical? 
#(Hint: Type ?mpg to read the documentation for the dataset.) 
#How can you see this information when you run mpg?




# Make a scatterplot of hwy vs. displ using the mpg data frame. Next, map a third,
#numerical variable to color, then size, then both color and size, then shape. 
#How do these aesthetics behave differently for categorical vs. numerical variables?


mpg %>% 
  ggplot(
    mapping = aes(x = hwy, y = displ)
  ) +
  geom_point(
    aes(color = drv, shape = drv)
  ) +
  labs(
    title = "Highway miles per gallon and engine displacement in litre",
    subtitle = "Dimensions for four wheel, rear wheel and all wheel drive.",
    x = "Highway miles per gallon",
    y = "Engine displacement per litre",
    caption = "Fuel economy data by EPA (https://fueleconomy.gov/)",
    color = "Drive Train",
    shape = "Drive Train"
  ) +
  scale_color_colorblind()

mpg %>% 
  ggplot(
    mapping = aes(x = hwy, y = displ)
  ) +
  geom_point(
    aes(color = drv, shape = drv)
  ) +
  labs(
    title = "Highway miles per gallon and engine displacement in litre",
    subtitle = "Dimensions for four wheel, rear wheel and all wheel drive grouped with respect to cyclinder count.",
    x = "Highway miles per gallon",
    y = "Engine displacement per litre",
    caption = "Fuel economy data by EPA (https://fueleconomy.gov/)",
    color = "Drive Train",
    shape = "Drive Train"
  ) +
  scale_color_colorblind() +
  facet_wrap(~cyl)

# shares categorical and numeric variables
extract_vars(mpg)

# shares categories from categorical variables
extract_categories(mpg)

#gives simplified summary for dataset
dataset_summary(mpg)



ggplot(mpg, aes(x = displ, y = hwy, color = cyl, size = cyl)) +
  geom_point()

# Make a scatterplot of bill_depth_mm vs. bill_length_mm and color the points by 
# species. What does adding coloring by species reveal about the relationship 
# between these two variables? What about faceting by species

ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) + 
  geom_point(
    aes(color = species, shape = species),
    na.rm = TRUE
  ) +
  facet_wrap(~species) +
  labs(
    title="Body mass and flipper length",
    subtitle="Dimensions for Adelie, Chinstrap and Gentoo penguins",
    caption = "Data come from the palmerpenguins package.",
    x = "Flipper length (mm)",
    y = "Body Mass (g)",
    color = "Species",
    shape = "Species"
  )
ggsave(filename = "penguin-plot.png", width = 10, height = 5)

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, color = species
  )
) +
  geom_point(
    aes(color = species, shape = species),
    na.rm = TRUE
  ) +
  labs(color = "Species",
       shape = "Species") ## this gives it same legend 


### Create the two following stacked bar plots. Which question can you answer 
# with the first one? Which question can you answer with the second one?

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")


ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
