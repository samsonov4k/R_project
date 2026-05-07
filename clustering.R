library(tidyverse)
library(lubridate)

#ЧАСТЬ ПЕРВАЯ. КЛАСТЕРИЗАЦИЯ ТОВАРОВ

#Этап 1 Загрузка и полготовка данных


file_name <- "data/purchases2.tsv"
#file_name <- "data/part.tsv"



clean_df <- read_tsv(
  file_name,
  col_select = c(Дата, Наименование, Количество, Сумма),
  col_types = cols(
    Дата = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    Наименование = col_character(),
    Количество = col_character(),
    Сумма = col_character()
  ),
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
) %>%
  mutate(
    Количество = parse_number(Количество, locale = locale(decimal_mark = ",", grouping_mark = " ")),
    Сумма = parse_number(Сумма, locale = locale(decimal_mark = ",", grouping_mark = " ")),
    Цена = Сумма / Количество,
    Характеристики = str_extract(Наименование, "\\d+([.,]\\d+)?\\s?(Г|КГ|ШТ|МЛ|Л)\\b"),
    Номенклатура = Наименование %>%
      str_remove("^П/Ф\\s+") %>%
      str_remove("\\d+([.,]\\d+)?\\s?(Г|КГ|ШТ|МЛ|Л)\\b") %>%
      str_trim()
  ) %>%
  relocate(Цена, .before = Сумма) %>%
  relocate(Номенклатура, .before = Характеристики) %>%
  identity()




#Этап 2 Подготовка признаков



# 1. Задаем списки
# развернуть <- c("набор", "крабовые", "салат", "паста")
предлоги <- c("из", "изо", "с", "со", "в", "во", "по", "для")
стоп_слова <- c("ратимир", "с пылу жару", "TOKYO")
стоп_слова_pattern <- paste0("\\b(", paste(sort(стоп_слова, decreasing = TRUE), collapse = "|"), ")\\b")

get_header <- function(text_input) {
  # 1. Склеиваем, приводим к нижнему регистру (надежная база для обработки)
  full_string <- str_to_lower(paste(text_input, collapse = " "))
  
  # 2. Теперь делаем замену по всей строке
  clean_string <- str_replace_all(full_string, regex(стоп_слова_pattern, ignore_case = TRUE), "")
  
  # 3. Теперь разбиваем обратно на слова для дальнейшей обработки
  # Используем str_split и удаляем лишние пробелы
  words_vec <- unlist(str_split(str_trim(clean_string), "\\s+"))
  words_vec <- words_vec[words_vec != ""]
  
  # 3. Токенизация
  words_vec <- unlist(str_split(str_trim(clean_string), "\\s+"))
  words_vec <- words_vec[words_vec != ""]
  
  result <- c()
  i <- 0
  
  for (word in words_vec) {
    if (i >= 2) break
    
    # Если предлог - добавляем, но счетчик i не растим
    if (str_to_lower(word) %in% предлоги) {
      result <- c(result, word)
      next
    }
    
    # Если обычное слово - добавляем и растим счетчик
    i <- i + 1
    result <- c(result, word)
  }
  
  return(paste(result, collapse = " "))
}


# Применяем
clusters_df <- clean_df %>%
  mutate(
    clean_name = tolower(str_trim(Номенклатура)),
    words = str_split(clean_name, "\\s+")
  ) %>%
  mutate(
    Начало = map_chr(words, get_header),
    Слово = map_chr(words, ~ .x[1])
  ) %>% 



  # 1. Группировка и суммирование
  group_by(Слово, Начало, Номенклатура, Характеристики) %>%
  summarise(Сумма = sum(Сумма, na.rm = TRUE), .groups = 'drop') %>%
  
  # 2. Вычисляем общую сумму по каждому "Слову"
  group_by(Слово) %>%
  mutate(Сумма_Группы = sum(Сумма)) %>%
  ungroup() %>%
  
  # 3. Сортировка:
  # - Сначала по общей сумме группы (desc)
  # - Затем по сумме конкретной позиции внутри группы (desc)
  arrange(desc(Сумма_Группы), desc(Сумма)) %>%
  
  # Удаляем временную колонку суммы группы, если она больше не нужна
  select(-Сумма_Группы) %>%
  
  identity() # Заглушка

# clusters_df %>%
# head(1500) %>%            # Выбираем первые 1000 строк
# pull(Слово) %>%           # Извлекаем колонку как вектор
# unique() %>%              # (Опционально) Оставляем только уникальные, если нужно
# paste(collapse = " ") %>% # Склеиваем через пробел
# cat()                     # Выводим в терминал



# Исходно Подкатегорию формирует первое слово номенклатуры, затем мы корректируем некоторые категории правилом ниже

# здесь сначала идёт новая подкатегория, затем знак ":" потом пробельыне символы(пробелы, табуляции), 
# После двоеточия первое слово должно полностью до символа совпасить со старой категорией (первым словом номенклатуры),
# а затем через пробел могут идти (а могут отсутствовать) любые слова или из части, которые должны найтись в наименовании номенклатуры

category_text <- "
Масло растительное: масло подсолнечное соевое оливковое раст нераф кукурузное
Масло сливочное:	масло сливочное
Пиво безалкогольное:	пиво безалкогольное
Крабовые палочки:	крабовые палочки
Помидоры консервированные:	помидоры мл
Голень куриная:	голень куриная цыпленка
Яйцо отборное:	яйцо отборное
Яйцо перепелиное:	яйцо перепелиное
Яйцо 1 категории:	яйцо 1
Яйцо 2 категории:	яйцо 2
Морская капуста:	морская капуста
Икра красная:	икра лососевая
Икра минтая:	икра минтая
Икра баклажанная:	икра баклаж
Паста из морепродуктов:	паста море рыб кальм
Макароны:	паста птитим
Сырок творожный:	сырок творож
Пиво:	пивной напиток
Свинина:	шашлык
Салат пресервы: салат
Кальмар сушеный:	кальмар суш аромат вял
Кальмар мороженый:	кальмар зам
Огурцы консервированные:	огурцы маринован
Капуста квашеная:	капуста кваш заксоч
Грибы замороженые:	грибы зам
Грибы маринованные:	грибы марин консерв остр
Корнишоны маринованные:	корнишоны марин ст/б
Горошек	консервированый: горошек
Бумага:	туалетная бумага
Зубная паста: зубная паста
Стиральный порошок:	порошок стирал
Гель для душа:	гель душ
Пакеты и мешки:	мешки
Пакеты и мешки:	пакеты
Пакеты и мешки:	пакет
Пакеты и мешки:	пакетики
Сахар:	сахар-рафинад
Пицца:	чебупицца
Оладушки:	жар-ладушки
Курица:	филе цыпленка бедра кур
Рыба:	филе кеты минтая тунца
Моющее средство:	ср-во моющ чист посуд domestos мытья
Моющее средство:	моющее средств
Моющее средство:	моющая жидк
Зелень:	набор зелен
Чай:	набор чая
Колбаса и сосиски: колбаса
Колбаса и сосиски: колбаски
Колбаса и сосиски: сосиски
Пресервы: ассорти подложка
Ассорти маринованное: ассорти ст/б
"

#Загружаем правило в таблицу, для дальнейшего исследования
schema <- read_lines(I(category_text)) %>%
  enframe(name = NULL, value = "line") %>%
  filter(line != "") %>%
  separate_wider_delim(
    line,
    delim = ":",
    names = c("Result", "Rule")
  ) %>%
  mutate(
    Result = str_trim(Result),
    Rule = str_trim(Rule)
  )


# 2. Функция для проверки условий
classify_item <- function(nom_text, word_text) {
  nom_lower <- str_to_lower(nom_text)
  word_lower <- str_to_lower(word_text)
  
  # Проходим по строкам схемы
  for (i in 1:nrow(schema)) {
    #rule_parts <- unstr_split(schema$Rule[i], " ")
    rule_parts <- str_split(schema$Rule[i], " ")[[1]]  # ← [[1]] для извлечения вектора    
    target_word <- rule_parts[1]
    features <- rule_parts[-1]
    
    # Условие: слово совпадает И (признаков нет ИЛИ какой-то признак найден в номенклатуре)
    if (word_lower == target_word) {
      if (length(features) == 0 || any(str_detect(nom_lower, features))) {
        return(schema$Result[i])
      }
    }
  }
  return(str_to_title(word_lower))
}



#добавляем подкатегорию

clusters_df <- clusters_df %>%
  mutate(Подкатегория = map2_chr(Номенклатура, Слово, classify_item))




# Создаем строковую структуру
groups_text <- "
Мясо и птица: свинина, говядина,  бедро, ребро, грудка, окорочок, поджарка, гуляш, фарш, голень куриная, курица, мясо
Молочные продукты: молоко, сыр, сырники, творог, кефир, ряженка, сметана, йогурт, сливки, сырок творожный, варенец, масло сливочное, биокефир
Яйца: яйцо отборное, яйцо 1 категории, яйцо 2 категории, яйцо перепелиное
Рыба и морепродукты: крабовые палочки, кальмар, икра красная, икра минтая, форель, мидии, корюшка, сельдь, минтай, палтус, горбуша, зубатка, нерка, креветки, шпроты, трубач, полосатик, иваси, скумбрия, камбала, кальмар мороженый, кета, рыба
Овощи и зелень: помидоры, огурцы, капуста, зелень, морковь, картофель, дайкон, перец, баклажаны, кабачки, лук, кукуруза, имбирь, укроп, свекла, шпинат, капуста квашеная, грибы замороженые, грибы маринованные, лоба, корнишоны маринованные
Фрукты и ягоды: яблоки, мандарины, апельсины, груша, черешня, грейпфрут, авокадо, дыня, банан, персик, слива, хурма, виноград, ананас, киви, манго, абрикосы, помело, арбуз, облепиха, бананы, нектарин
Бакалея: кофе, чай, паста, крупа, хлеб, лапша, мука, мак.изделия, рис, фасоль, семена, хлопья, хлебцы, приправа, кетчуп, майонез, хрен, смесь, маслины, оливки, огурцы консервированные, масло растительное, соус, икра баклажанная, горошек консервированный, лаваш, сахар, мюсли, отруби, лепёшка, мед, семечки, ассорти маринованное
Напитки: вода, пиво, пиво безалкогольное, вино, водка, напиток, сок, нектар, виски, джин, аперитив, ром
Полуфабрикаты: пельмени, вареники, блинчики, блины, пицца, гедзе, дамплинги, круггетсы, наггетсы, оладушки, 
Закуски и готовая еда: солянка, набор,  скоблянка, пресервы, крем-паста, паста из морепродуктов, салат пресервы, кальмар сушеный, колбаса и сосиски, грудинка, ветчина, морская капуста, карбонат, буженина, язык, плов
Кондитерские изделия: шоколад, конфеты, печенье, курага, орех, вафельный, кекс, десерт, крекер
Непродовольственные: бумага, салфетки, зубная паста, полотенца, шампунь, уголь, стиральный порошок, гель для душа, пакеты и мешки, отбеливатель, сода, моющее средство, батарейки, мыло
"

# Парсим эту строку в удобный для R список
groups_list <- str_split(str_trim(groups_text), "\n")[[1]] %>%
  map(~ {
    parts <- str_split(.x, ":", n = 2)[[1]]
    list(cat = str_trim(parts[1]), items = str_split(str_trim(parts[2]), ",\\s*")[[1]])
  })

# Функция для сопоставления слова с категорией
get_category <- function(word) {
  for (g in groups_list) {
    if (str_to_lower(word) %in% g$items) return(g$cat)
  }
  return("Прочее")
}

# glimpse(clusters_df)

#добавляем категорию
clusters_df <- clusters_df %>%
  mutate(Категория = map_chr(Подкатегория, get_category))

# # Проверка распределения
# table(clusters_df$Категория)

# Выгружаем clusters_df в TSV файл
write_tsv(clusters_df, "data/clusters.tsv")



# Фильтруем только "Прочие" и группируем Подкатегория → Начало

others_df<- clusters_df %>%
  filter(Категория == "Прочее") %>%


  identity() # Заглушка

# glimpse(others_df)


# install.packages("treemapify")
library(ggplot2)
library(treemapify)


print("строим схему...")


# Подготавливаем данные
treemap_df <- clusters_df %>%
  group_by(Категория, Подкатегория) %>%
  summarise(Сумма = sum(Сумма), .groups = 'drop')

library(dplyr)
library(forcats) # Пакет для удобной работы с факторами

# Сортируем Категории по сумме затрат
treemap_df <- treemap_df %>%
  mutate(Категория = fct_reorder(Категория, Сумма, sum, .desc = TRUE))


#Варианты кастомных палитр с градиентом
# custom_palette <- viridisLite::plasma(15, begin = 0.2, end = 0.8)
custom_palette <- viridisLite::rocket(15, begin = 0.2, end = 0.8)
#custom_palette <- viridisLite::mako(15, begin = 0.2, end = 0.8)


# Генератор светлой палитры с одной яркостью
#custom_palette <- hex(polarLUV(L = 60, C = 40, H = runif(15, 0, 360)))
# Генератор ещё один
#custom_palette <- qualitative_hcl(15, palette = "Dark 3", l = runif(15, 30, 80))

custom_palette <- sample(custom_palette)

# Палитра без разброса яркости
# custom_palette = c("#9D87BB","#BD7E9C", "#B180B1", "#929459", "#B18968", "#B4876D",
# "#BE8088", "#BD8184", "#3B9F9A", "#A98C60", "#BB837C", "#888EBD",
# "#44A08A", "#5C9E76", "#449DA7")


custom_palette <- c(
 "#1b1b2f", "#2c2c54", "#3b3b98", "#474787", "#6c5ce7",
 "#8e44ad", "#a64d79", "#c0392b", "#d35400", "#e74c3c",
 "#ff6b6b", "#ff7f50", "#ff9f43", "#f39c12", "#c97b2a"
)

# # library(ggthemes)
# custom_palette <- rev(custom_palette)


# print(custom_palette)


p <- ggplot(treemap_df, aes(area = Сумма,
                       fill = Категория,
                       subgroup = Категория,
                       label = Подкатегория)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 2, alpha = 1) +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 2.5,
                    grow = TRUE,          # ← ЭТО КЛЮЧЕВАЯ НАСТРОЙКА
                    reflow = TRUE,        # ← Перенос слов, если не влезает
                    alpha = 0.1,
                    fontface = "bold",
#                    family = "mono",
                    min.size = 4) +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 2.5,
                    grow = TRUE,          # ← ЭТО КЛЮЧЕВАЯ НАСТРОЙКА
                    reflow = TRUE,        # ← Перенос слов, если не влезает
                    #                   family = "mono",
                    min.size = 4) +
  scale_fill_manual(values = rev(custom_palette), name = "Категория товаров") +
#  scale_fill_tableau(palette = "Tableau 20", name = "Категория товаров") + # 20 спокойных цветов
#  scale_fill_viridis_d(option = "plasma", name = "Категория товаров") +

  labs(title = "Структура расходов по продуктам",
       subtitle = "Размер плиток пропорционален сумме затрат за весь период",
       caption = "Данные: чеки за трёхлетний период по одной карте лояльности продуктовой сети «Реми»") +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, colour = "grey40"),
        plot.caption = element_text(size = 9, colour = "grey50", hjust = 0))

print(p)
print("Схема построена")

# СОХРАНЯЕМ РЕЗУЛЬТАТ

# Объединяем по двум ключам
purchases_clustered <- clean_df %>%
  left_join(clusters_df %>% select(Номенклатура, Характеристики, Категория, Подкатегория), 
            by = c("Номенклатура", "Характеристики"))

# purchases_clustered <- purchases_clustered %>%
#   arrange(Дата)

#Дату оставим в том же формате, как она была
purchases_clustered <- purchases_clustered %>%
  mutate(Дата = format(as.POSIXct(Дата), "%Y-%m-%d %H:%M:%S"))


purchases_clustered <- purchases_clustered %>%
  mutate(
    Цена = round(Цена, 2),
    Сумма = round(Сумма, 2)
  )
# Сохраняем в формате TSV
purchases_clustered_to_save <- purchases_clustered %>%
  mutate(across(where(is.numeric), ~ format(.x, decimal.mark = ",", nsmall = 2)))

readr::write_delim(purchases_clustered_to_save, "data/purchases_clustered.tsv", delim = "\t")


part_df <- purchases_clustered %>%
  filter(Категория %in% c("Яйца", "Кондитерские изделия")) %>%
  select(Дата, Количество, Сумма, Наименование)

write.table(
  part_df,
  "data/part.tsv",
  sep = "\t",
  dec = ",",
  row.names = FALSE,
  quote = FALSE
)

