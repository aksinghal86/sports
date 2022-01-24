library(tidyverse)
library(geomtextpath)
library(patchwork)

read_dat <- function(filename) { 
  read_csv(paste0('data/', filename))
}

players <- c('looney', 'poole', 'curry', 'wiggins', 'draymond', 'iggy', 'lee', 'otto')
players <- setNames(players, 1:8)
players_df <- paste0(players, '.csv') %>% 
  purrr::map_df( ~read_csv(.x, col_types = cols(.default = 'c')), .id = 'Player') %>%
  mutate_at(vars(Rk, G, PTS, `FG%`), as.numeric) %>% 
  mutate(MP = as.numeric(lubridate::hm(MP))/3600, 
         PPM = PTS/MP, 
         Won = str_detect(X8, 'W')) %>% 
  left_join(data.frame(players) %>% rownames_to_column(var = 'Player'), by = c('Player')) %>% 
  select(-Player) %>% 
  rename(Player = players) %>% 
  mutate(Player = str_to_title(Player)) %>% 
  filter(!is.na(G))

gsw <- players_df %>% distinct(Rk, Won) %>% mutate(Player = "GSW", Outcome = ifelse(Won, 'W', 'L'))
p1 <- ggplot(players_df, aes(x = Rk, y = PPM, color = Player)) + 
  geom_point(size = 2, alpha = 0.2) +
  geom_labelsmooth(aes(label = Player), text_smoothing = 30, 
                   method = 'loess', formula = y ~ x, se = F, span = 0.7, 
                   hjust = 'auto') + 
  geom_textvline(xintercept = 30, linewidth = 1, linetype = 'dashed', label = 'Last 15 games', hjust = 0.7) +
  geom_text(data = gsw, aes(x = Rk, y = 0, label = Outcome), size = 3.5, color = 'black') +
  theme_bw() + 
  theme(legend.position = 'none') + 
  labs(x = 'Game', title = 'GSW Points Per Minute Comparison by Player', subtitle = 'Only players with >20 MPG included; minus Klay')

p2 <- ggplot(players_df, aes(x = Rk, y = `FG%`, color = Player)) + 
  geom_point(size = 2, alpha = 0.2) +
  geom_labelsmooth(aes(label = Player), text_smoothing = 30, 
                   method = 'loess', formula = y ~ x, se = F, span = 0.7, 
                   hjust = 'auto') + 
  geom_textvline(xintercept = 30, linewidth = 1, linetype = 'dashed', label = 'Last 15 games', hjust = 0.7) +
  # geom_textvline(xintercept = 35, linetype = 'dotdash', label = 'Last 10 games', hjust = 0.7) +
  geom_text(data = gsw, aes(x = Rk, y = 0, label = Outcome), size = 3.5, color = 'black') +
  theme_bw() + 
  theme(legend.position = 'none') + 
  labs(x = 'Game', title = 'GSW Field Goal % Comparison by Player', subtitle = 'Only players with >20 MPG included; minus Klay')

p1 + p2
