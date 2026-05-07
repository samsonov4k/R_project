

library(tidyverse)
library(lubridate)
library(readr)
library(scales)

df <- read_tsv("data/purchases_clustered.tsv", show_col_types = FALSE) %>%
  rename(Характеристика = Характеристики)


Подкатегория_ <- "Яйцо отборное"
Характеристика_ <- "10ШТ"

df_eggs <- df %>%
  mutate(
    Дата_obj = as.POSIXct(Дата, format = "%Y-%m-%d %H:%M:%S"),
    Цена = as.numeric(gsub(",", ".", Цена)),
    Квартал = floor_date(Дата_obj, unit = "quarter")
  ) %>%
  filter(
    Подкатегория == Подкатегория_,
    Характеристика == Характеристика_
  ) %>%
  arrange(Дата_obj)




candles <- df_eggs %>%
  group_by(Квартал) %>%
  summarise(
    Open = first(Цена),
    High = max(Цена, na.rm = TRUE),
    Low = min(Цена, na.rm = TRUE),
    Close = last(Цена),
    .groups = "drop"
  ) %>%
  mutate(Direction = if_else(Close >= Open, "up", "down"))

candles_linked <- df_eggs %>%
  group_by(Квартал) %>%
  summarise(
    High = max(Цена, na.rm = TRUE),
    Low = min(Цена, na.rm = TRUE),
    Close_real = last(Цена),
    Open_real = first(Цена),
    .groups = "drop"
  ) %>%
  arrange(Квартал) %>%
  mutate(
    Open = lag(Close_real, default = first(Open_real)),
    Close = Close_real,
    Direction = if_else(Close >= Open, "up", "down")
  )


candles_linked <- candles_linked %>%
  mutate(Квартал_центр = Квартал %m+% months(1) %m+% days(15)) %>%
  mutate(IsDoji = Open == Close)


body_half_width <- 35


p <- ggplot(candles_linked, aes(x = Квартал_центр)) +
  geom_linerange(aes(ymin = Low, ymax = High), linewidth = 0.8, colour = "#B0B0B0") +
  geom_rect(
    data = filter(candles_linked, !IsDoji),
    aes(
      xmin = Квартал_центр - days(body_half_width),
      xmax = Квартал_центр + days(body_half_width),
      ymin = pmin(Open, Close),
      ymax = pmax(Open, Close),
      fill = Direction
    ),
    colour = NA
  ) +
  geom_segment(
    data = filter(candles_linked, IsDoji),
    aes(
      x = Квартал_центр - days(body_half_width),
      xend = Квартал_центр + days(body_half_width),
      y = Open,
      yend = Close
    ),
    linewidth = 1.1,
    colour = "white",
    lineend = "butt"
  ) +
  scale_fill_manual(values = c(up = "#2ecc71", down = "#ff5c5c"), guide = "none") +
  scale_x_datetime(
    breaks = candles_linked$Квартал_центр,
    labels = function(x) paste0(year(x), "-Q", quarter(x)),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", suffix = " ₽")) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  labs(
    title = paste("График изменения цены", Подкатегория_, Характеристика_),
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#111111", colour = NA),
    plot.background = element_rect(fill = "#111111", colour = NA),
    panel.grid.major.x = element_line(colour = "#3A3A3A", linewidth = 0.4),
    panel.grid.major.y = element_line(colour = "#3A3A3A", linewidth = 0.4),
    panel.grid.minor.x = element_line(colour = "#232323", linewidth = 0.25),
    panel.grid.minor.y = element_line(colour = "#232323", linewidth = 0.25),
    axis.text = element_text(colour = "#E8E8E8"),
    axis.title = element_text(colour = "#E8E8E8"),
    plot.title = element_text(colour = "#FFFFFF", face = "bold"),
    axis.ticks = element_line(colour = "#666666"),
    plot.margin = margin(t = 10, r = 25, b = 10, l = 10)
  )

print(p)




