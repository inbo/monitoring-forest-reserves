library(tidyverse)

source('Kleuren.R')

# FIGUUR 1 ARTIKEL ----
# LINERANGE
# managed vs set-aside: stock t1 en stock t2 per PNV
dta_C_BRVBI_mCI %>% # niet dataset zelf veranderen, maar hier aanpassen wat nodig is voor de grafiek
  dplyr::filter(variable != "Cstockchange_ha_yr") %>%
  mutate(variabele = str_sub(variable, 1, str_length(variable)-5),
         periode = readr::parse_number(variable),
         periode2 = case_when(periode == '1998' ~ '1',
                              periode == '2003' ~ '3',
                              periode == '2013' ~ '13',
                              periode == '2014' ~ '16'),
         periode2 = as.numeric(periode2),
         PNV = factor(PNV,
                      levels = c(2, 4, 5, 6, 7),
                      labels = c('WATERLOGGED', 'WET ALLUVIAL', 'SILT', 'SANDY SILT', 'SAND'))) %>%
  ggplot(., aes(x = periode2, y = m,
                ymin = CIL, ymax = CIU,
                colour = PNV, shape = forests, group = forests)) + 
  geom_linerange(linewidth = 1, 
                 position = position_dodge2(width = 0.4)) +
  geom_line(linewidth = 0.5, linetype = 'dotted', 
            position = position_dodge2(width = 0.4)) +
  geom_point(size = 3.5, fill = 'white', 
             position = position_dodge2(width = 0.4)) +
  scale_x_continuous(lim = c(0, 17)) +
  labs(x = 'inventory',
       y = expression(paste('carbon stock (tC  ', ha^{-1},  ')'))) +
  facet_grid(. ~ PNV, 
             scales = 'free_y') +
  scale_colour_manual("", values = KleurenSiteType,
                      guide = 'none') +
  scale_shape_manual("", values = c(21, 16)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = 'bottom',
        legend.box = 'horizontal',
        strip.text.x = element_text(size = 14),
        strip.background = element_blank())
#
# FIGUUR 2 ARTIKEL ----
# SCATTERPLOTS
# wet vs dry sites
dta_C_BR_smooth <- dta_C_BR %>%
  dplyr::filter((site == 'wet' & Cstock_sum_t_ha_1 > 40 & Cstock_sum_t_ha_1 < 140) |
                  (site == 'dry' & Cstock_sum_t_ha_1 > 45 & Cstock_sum_t_ha_1 < 190)) # artisanaal de grenzen aangeven waarvoor de smoother geplot moet worden
ggplot() + 
  geom_hline(yintercept = 0, lty = 'dotted', colour = 'grey') + 
  geom_smooth(data = dta_C_BR_smooth, 
              aes(x = Cstock_sum_t_ha_1, y = Cflux_sum_yr),
              method = 'loess', 
              size = 1, alpha = 0.2, colour = 'black') +
  geom_point(data = dta_C_BR, 
             aes(x = Cstock_sum_t_ha_1, y = Cflux_sum_yr, colour = site),
             size = 3, alpha = 0.5) +
  facet_grid(. ~ site,
             labeller = as_labeller(c(wet = 'WET SITES', dry = 'DRY SITES'))) +
  scale_shape_manual("", values = c(16, 1)) +
  scale_colour_manual("", values = KleurenNatDroog, 
                      guide = 'none') +
  labs(y = expression(paste('carbon stock change (tC  ', ha^{-1}, yr^{-1}, ')')), 
       x = expression(paste('carbon stock (tC ', ha^{-1}, ')'))) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = 'bottom',
        strip.text.x = element_text(size = 14),
        strip.background = element_blank())