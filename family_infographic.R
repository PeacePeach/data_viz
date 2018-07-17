library(tidyverse)
library(lubridate)
library(waffle)
library(extrafont)
library(gridExtra)
library(grid)
library(gtable)

# Load the data
great_grandchild <- read_csv("../data/great_grandchild.csv")

# View the data
great_grandchild

# ---------------------------------
# Data Pre-Processing
# ---------------------------------

# ----- Prepare data for Waffle Chart -----
# General
ggchild <- great_grandchild %>% 
  count(root, sort = TRUE) %>% 
  rename(family = root) %>% 
  mutate(value = as.integer(n)) %>% 
  select(-n)

# By gender: boys
ggchild.boy <- great_grandchild %>% 
  filter(gender == "male") %>% 
  count(root, sort = TRUE) %>% 
  rename(family = root) %>% 
  mutate(value = as.integer(n)) %>% 
  select(-n)

# By gender: girls
ggchild.girl <- great_grandchild %>% 
  filter(gender == "female") %>% 
  count(root, sort = TRUE) %>% 
  rename(family = root) %>% 
  mutate(value = as.integer(n)) %>% 
  select(-n)

# Change the dataset from tibble to named vectors
myData      <- setNames(as.integer(ggchild$value), as.character(ggchild$family))
myData.boy  <- setNames(as.integer(ggchild.boy$value), as.character(ggchild.boy$family))
myData.girl <- setNames(as.integer(ggchild.girl$value), as.character(ggchild.girl$family))


# ----- Prepare data for Bar Plot -----
# Main data
ggchild.year <- great_grandchild %>% 
  mutate(yob = year(dob),
         root = factor(root, levels = rev(c("Che Long", "Ayah Long", "Che Yoh", "Che Jae",
                                            "Ayah Ngoh", "Che Jon", "Che Lah")))) %>% 
  count(root, yob) %>% 
  arrange(yob) %>% 
  rename(family = root)

# Line data
df  <- tibble(yob = c(2003:2018), counts = rep(1, 16))
df2 <- tibble(yob = c(2003:2018), counts = c(rep (1, 11), rep(3, 5)))
df3 <- tibble(yob = c(2003:2018), counts = c(rep (1, 11), rep(5, 4), 1))
df4 <- tibble(yob = c(2003:2018), counts = c(rep (1, 14), 6, 1))

# ---------------------------------
# Waffle Chart
# ---------------------------------

family.plot <- waffle(myData, 
                      rows = 5, 
                      legend_pos = "bottom", 
                      colors = c("#09abbd", "#f89c27", "#e23b69", "#fed823", 
                                 "#a7cc42", "#84739a", "#b4a897"),
                      size = 0.01,
                      use_glyph = "child", 
                      glyph_size = 10)


boy.plot <- waffle(myData.boy,
                   rows = 5,
                   legend_pos = "",
                   colors = c("#09abbd", "#e23b69", "#f89c27", "#84739a", "#fed823", 
                              "#b4a897", "#a7cc42"),
                   size = 0.01,
                   use_glyph = "male",
                   glyph_size = 8,
                   title = "Boys = 28")


girl.plot <- waffle(myData.girl,
                    rows = 5,
                    legend_pos = "",
                    colors = c("#09abbd", "#f89c27", "#e23b69", "#a7cc42", "#b4a897", "#fed823"),
                    size = 0.01,
                    use_glyph = "female", 
                    glyph_size = 8,
                    title = "Girls = 26")


# ---------------------------------
# Bar Plot
# ---------------------------------

child.yr <- ggchild.year %>% 
  ggplot() +
  geom_bar(color = "black", stat = "identity", aes(yob, n, fill = family)) +
  geom_crossbar(data = df,  
                aes(x = yob, y = counts, ymin = counts - 1, ymax = counts), 
                fatten = 1) +
  geom_crossbar(data = df2, 
                aes(x = yob, y = counts, ymin = counts - 1, ymax = counts), 
                fatten = 1) +
  geom_crossbar(data = df3, 
                aes(x = yob, y = counts, ymin = counts - 1, ymax = counts), 
                fatten = 1) +
  geom_crossbar(data = df4, 
                aes(x = yob, y = counts, ymin = counts - 1, ymax = counts), 
                fatten = 1) +
  ggtitle("") +
  labs(x = "Year", y = "") +
  scale_fill_manual(values = rev(c("#09abbd", "#b4a897", "#84739a", "#e23b69", "#fed823", 
                                   "#f89c27", "#a7cc42"))) +
  theme(panel.background   = element_rect(fill = "white"),
        plot.background    = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line("#DCDCDC"),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(face = "bold", size = 21, hjust = 0.5),
        axis.title         = element_text(size = 17),
        axis.title.x       = element_text(margin = margin(t = 15)),
        axis.text          = element_text(size = 13),
        legend.position    = "none")


# ---------------------------------
# Table
# ---------------------------------

ggchild.table <- great_grandchild %>% 
  count(root, gender) %>% 
  spread(gender, n, fill = 0) %>% 
  rename(Family = root, Boys = male, Girls = female) %>% 
  mutate(Sum = rowSums(select(., -Family))) %>% 
  arrange(desc(Sum)) %>% 
  select(Family, Boys, Girls, Sum)

family.table <- tableGrob(ggchild.table,
                          rows = NULL,
                          theme = ttheme_minimal())

family.table <- gtable_add_grob(family.table,
                                grobs = segmentsGrob(
                                  x0 = unit(0,"npc"),
                                  y0 = unit(0,"npc"),
                                  x1 = unit(1,"npc"),
                                  y1 = unit(0,"npc"),
                                  gp = gpar(lwd = 2.0)),
                                t = 1, b = 1, l = 1, r = 4
)


# ---------------------------------
# Combine All
# ---------------------------------

chart.layout <- rbind(c(1, 1, 2), c(3, 4, 5))

chart.title <- textGrob("Tok Che's Great-Grandchildren\nMini Infographic", 
                   gp = gpar(fontsize = 23, fontface = "bold"), 
                   hjust = 0.5)

grid.arrange(family.plot, child.yr, boy.plot, girl.plot, family.table, 
             layout_matrix = chart.layout, top = chart.title)


