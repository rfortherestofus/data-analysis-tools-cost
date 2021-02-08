# Load Packages -----------------------------------------------------------

library(tidyverse)
library(tibble)
library(hrbrthemes)
library(gganimate)
library(scales)
library(ggtext)
library(shadowtext)

# Added for fonts to be able to work on Maia's device
# extrafont::loadfonts(device = "win")

# Generate Data -----------------------------------------------------------

data_tools_cost <- tribble(~tool, ~year_1) %>% 
  
  # R is free
  
  add_row(tool = "R",
          year_1 = 0) %>% 
  
  # Stata is $765 per year
  # https://www.Stata.com/order/new/gov/single-user-licenses/dl/
  
  add_row(tool = "Stata",
          year_1 = 765) %>% 
  
  # SPSS is $99/month
  # https://www.ibm.com/products/SPSS-statistics/pricing
  
  add_row(tool = "SPSS",
          year_1 = 99 * 12) %>% 
  
  # SAS Analytics Pro is $10,170
  # https://www.SAS.com/store/products-solutions/cSoftware-p1.html
  # Ongoing fees are around 30% of first year fee
  # Source: https://www.quora.com/How-much-does-SAS-cost
  
  add_row(tool = "SAS",
          year_1 = 10170) %>% 
  
  mutate(year_2 = case_when(
    tool == "SAS" ~ year_1 * 0.3,
    TRUE ~ year_1
  )) %>% 
  mutate(year_3 = case_when(
    tool == "SAS" ~ year_1 * 0.3,
    TRUE ~ year_1
  )) %>% 
  mutate(year_4 = case_when(
    tool == "SAS" ~ year_1 * 0.3,
    TRUE ~ year_1
  )) %>% 
  mutate(year_5 = case_when(
    tool == "SAS" ~ year_1 * 0.3,
    TRUE ~ year_1
  )) %>% 
  pivot_longer(cols = -tool,
               names_to = "year",
               values_to = "cost") %>% 
  group_by(tool) %>% 
  mutate(total_cost = cumsum(cost)) %>% 
  ungroup() %>% 
  mutate(
    year = parse_number(year),
    labels_cost = ifelse(year == 5, dollar(total_cost), ""),
    labels_tool = ifelse(year == 5, tool, ""),
    time_reveal = row_number() # Unique index to reveal data over in animation
    )


# Make + Save Plot --------------------------------------------------------

# Create static plot
data_tools_cost_plot2 <- 
  data_tools_cost %>% 
  ggplot(aes(year, total_cost,
             group = tool,
             color = tool,
             label = labels_cost)) +
  geom_line(size = 1) +
  geom_shadowtext(aes(label = tool), 
                  bg.color = "white",
                  family = "Inter SemiBold",
                  size = 7) +
  geom_shadowtext(aes(x = 5.5),
                  bg.color = "white",
                  family = "Inter SemiBold",
                  size = 7) +
  coord_cartesian(clip = "off") +
  scale_color_manual(guide = guide_legend(reverse = TRUE),
                     values = c(
                       "R" = "#6cabdd",
                       "SAS" = "#ff7400",
                       "Stata" = "#ec3325",
                       "SPSS" = "#ffc659"
                     )) +
  labs(title = "Cost over time of <b><span style='color: #6cabdd'>R</span></b> compared to 
       <b><span style='color: #ff7400'>SAS</span></b>,
       <b><span style='color: #ffc659'>SPSS</span></b>, and
       <b><span style='color: #ec3325'>Stata</span></b>") +
  scale_y_continuous(labels = dollar_format(), limits = c(0, 23000)) +
  scale_x_continuous(labels = c("Year 1", "2", "3", "4", "5"), breaks = 1:5) +
  theme_ipsum(base_family = "Inter",
              base_size = 13) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_markdown(hjust = 0.5, margin = margin(b = 40)),
        legend.position = "none")

# Add transition elements
data_tools_cost_animation2 <-
  data_tools_cost_plot2 +
  # Reveal over the unique data index
  transition_reveal(
    time_reveal, keep_last = TRUE
  )

#animate(data_tools_cost_animation2, nframes = 30)

# Create gif
anim_save(data_tools_cost_animation2,
          filename = "cost-over-time2.gif",
          nframes = 75,
          end_pause = 10,
          width = 1000,
          height = 500, 
          type = "cairo")
