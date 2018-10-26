# number of women candidates in congress

library('tidyverse')

## http://cawp.rutgers.edu/potential-candidate-summary-2018
## http://www.cawp.rutgers.edu/sites/default/files/resources/can_histsum.pdf

cand <- read_csv('congress_running.csv')

# by chamber
ggplot(cand, aes(x = year, y = number, group = party)) +
  geom_line(aes(color = party)) +
  geom_point(data = cand %>% filter(year == 2018), 
             aes(x = year, y = number, 
                 group = party, color = party)) +
  facet_wrap(~ chamber) +
  scale_x_continuous(breaks = seq(1970, 2018, 4), limits = c(1970, 2018))

# house and senate together
cand.sum <- cand %>% group_by(year, party) %>% summarise(number = sum(number))

ggplot(cand.sum, aes(x = year, y = number, group = party)) +
  geom_line(aes(color = party), size = 1) +
  geom_point(data = cand.sum %>% filter(year == 2018), 
             aes(x = year, y = number, 
                 group = party, color = party), size = 2) +
 geom_vline(xintercept = 1992, linetype = 2, color = 'black', size = .5) + 
  labs(x = '', y = '') +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(breaks = seq(1970, 2018, 4), limits = c(1970, 2018), 
                     labels = c("1970", "'74", "78", "'82", "'86", "'90", "'94", "'98",
                                "2002", "'06", "'10", "'14", "'18")) +
  scale_color_manual(values = c('#446982', '#d53e4f')) +
  theme_minimal()+ theme(panel.grid.minor.x = element_blank(),
                         axis.ticks.x = element_line(),
                         axis.line.x = element_line(),
                         panel.grid.major.x = element_blank(), 
                         panel.grid.major.y = element_line(colour="grey46", size = rel(0.25)), 
                         panel.grid.minor.y = element_line(colour="grey46", size = rel(0.25)), 
                         axis.text = element_text(color = 'black', size = 12), 
                         legend.position = 'none', legend.title = element_blank()) 

ggsave('congress_candidates.png', width = 5.5, height = 4)


