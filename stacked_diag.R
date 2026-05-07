library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(scales)

# 1. Загружаем данные
df <- read_tsv("data/purchases_clustered.tsv", show_col_types = FALSE)



df_area <- df %>%
  mutate(
    Дата_obj = as.POSIXct(Дата, format = "%Y-%m-%d %H:%M:%S"),
    Сумма = as.numeric(gsub(",", ".", Сумма)),    
    Квартал = floor_date(Дата_obj, unit = "quarter"),
    Квартал_середина = Квартал %m+% months(1) %m+% days(15)
  ) %>%
  group_by(Квартал_середина, Категория) %>%
  summarise(Сумма = sum(Сумма, na.rm = TRUE), .groups = "drop")

cat_order <- df_area %>%
  group_by(Категория) %>%
  summarise(total = sum(Сумма), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(Категория)

df_area$Категория <- factor(df_area$Категория, levels = cat_order)

df_total <- df_area %>%
  group_by(Квартал_середина) %>%
  summarise(Сумма = sum(Сумма), .groups = "drop")



# Палитра двух графиков совпадает
custom_palette <- c(
  "#1b1b2f", "#2c2c54", "#3b3b98", "#474787", "#6c5ce7",
  "#8e44ad", "#a64d79", "#c0392b", "#d35400", "#e74c3c",
  "#ff6b6b", "#ff7f50", "#ff9f43", "#f39c12", "#c97b2a"
)


#Возьмём палитру с конца, т.к. логика данного графика чуть-чуть другая
n <- nlevels(factor(df_area$Категория))
m <- length(custom_palette)
custom_palette_ <- custom_palette[seq(m - n + 1, m)]



cat_order <- df_area %>%
  group_by(Категория) %>%
  summarise(total = sum(Сумма), .groups = "drop") %>%
  arrange(total) %>%
  pull(Категория)

df_area$Категория <- factor(df_area$Категория, levels = cat_order)



p <- ggplot(df_area, aes(x = Квартал_середина, y = Сумма, fill = Категория)) +
  # geom_area(alpha = 0.9, colour = NA) +
  geom_area(alpha = 0.9, colour = "grey80", linewidth = 0.5)+
  geom_line(
    data = df_total,
    aes(x = Квартал_середина, y = Сумма),
    inherit.aes = FALSE,
    colour = NA,
    linewidth = 0.9
  ) +
#  scale_fill_manual(values = custom_palette) +
  scale_fill_manual(values = custom_palette_) +
  scale_x_datetime(
    breaks = seq(
      from = floor_date(min(df_area$Квартал_середина), unit = "year"),
      to   = ceiling_date(max(df_area$Квартал_середина), unit = "year"),
      by   = "1 year"
    ),
    minor_breaks = seq(
      from = floor_date(min(df_area$Квартал_середина), unit = "quarter"),
      to   = ceiling_date(max(df_area$Квартал_середина), unit = "quarter"),
      by   = "3 months"
    ),
    date_labels = "%Y"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", suffix = " ₽")) +
  labs(
    title = "Динамика расходов поквартально",
    subtitle = "Расходы по категориям поквартально по карте лояльности продуктовой сети",
    x = "Период поквартально с II квартала 2023 года по I квартал 2026 года",
    y = "Сумма расходов за квартал",
    fill = "Категория"
  ) +
  theme_classic(base_size = 12, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey30"),
    legend.position = "right",
    panel.border = element_blank(),
    axis.line.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    panel.grid.major.x = element_line(colour = "grey70", linewidth = 0.4),
    panel.grid.minor.x = element_line(colour = "grey85", linewidth = 0.3)
  )

print(p)
