library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

dfn <- read_excel("Jumlah Pengunjung yang Memberi.xlsx")
dfn <- dfn %>%
  mutate(across(cukup, ~replace(., is.na(.), 0)))
df_long <- dfn %>%
  pivot_longer(cols = c(sangatpuas, puas, cukup, tidakpuas, sangattidakpuas),
               names_to = "Kategori", values_to = "Jumlah") %>%
  group_by(sex, Kategori) %>%
  summarise(Jumlah = sum(Jumlah), .groups = "drop")
df_long$Kategori <- factor(df_long$Kategori, levels = c("sangatpuas", "puas", "cukup", "tidakpuas", "sangattidakpuas"))

plot1 <- ggplot(df_long, aes(x = Kategori, y = Jumlah, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = Jumlah), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  
  labs(title = "Jumlah Pengunjung Berdasarkan Kategori Penilaian dan Jenis Kelamin",
       x = "Kategori Penilaian", y = "Jumlah Pengunjung") +
  scale_fill_manual(values = c("Laki-Laki" = "#34a246", "Perempuan" = "#105879")) +  
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+annotate("text", x = 3, y = max(df_long$Jumlah) * 0.75, label = "Tingkat Kepuasan Mencapai 98%", color = "black", 
             size = 5, fontface = "bold", hjust = 0.5, vjust = 0.5)

total_pengunjung <- sum(df_long$Jumlah)
sangatpuas_puas <- df_long %>%
  filter(Kategori %in% c("sangatpuas", "puas")) %>%
  summarise(total_sangatpuas_puas = sum(Jumlah))
persentase <- (sangatpuas_puas$total_sangatpuas_puas / total_pengunjung) * 100
round(persentase, 1)

############
dd <- paste("Januari","2023", sep = " ")
dfnn <- dfn %>% filter(periode_update==dd)
dfn2023 <- dfn %>% filter(Tahun==2023)
dfn2024 <- dfn %>% filter(Tahun==2024)


###############
# Menghitung jumlah total berdasarkan jenis kelamin
df_sex <- df_long %>%
  group_by(sex) %>%
  summarise(total = sum(Jumlah))
# Compute percentages
df_sex$fraction <-  df_sex$total / sum(df_sex$total)
df_sex$ymax = cumsum(df_sex$fraction)
df_sex$ymin = c(0, head(df_sex$ymax, n=-1))
custom_colors <- c("Laki-Laki" = "#34a246", "Perempuan" = "#105879")
# Make the plot
ggplot(df_sex, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = sex)) +
  geom_rect() +
  coord_polar(theta = "y") +  # Convert to pie chart
  xlim(c(2, 4)) +  # Create space in the center for donut effect
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme_void() +
  theme(legend.position = "top") +
  # Add percentage labels
  geom_text(aes(x = 3.5, y = (ymax + ymin) / 2, label = paste0(round(fraction * 100, 1), "%")),
            color = "white", size = 6)


###############

month_map <- c("Januari" = "January", "Februari" = "February", "Maret" = "March", "April" = "April", "Mei" = "May",
               "Juni" = "June", "Juli" = "July", "Agustus" = "August", "September" = "September", "Oktober" = "October",
               "November" = "November", "Desember" = "December")

# Process and calculate total visitors
dfnp <- dfn %>%
  mutate(Bulan = recode(Bulan, !!!month_map)) %>% 
  mutate(periode_update = dmy(paste("01", Bulan," ",Tahun))) %>%  # Convert to Date format# Recode month names
  arrange(periode_update)  # Arrange by date

# Summarize total visitors
df_total <- dfnp %>%
  mutate(total_visitors = sangatpuas + puas + tidakpuas + sangattidakpuas + cukup) %>%  # Calculate total visitors
  select(periode_update, sex, total_visitors)

# Create the line chart
ggplot(df_total, aes(x = periode_update, y = total_visitors, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_point(aes(shape = sex), size = 3) +
  labs(
    title = "Tren Jumlah Pengunjung Berdasarkan Jenis Kelamin",
    x = "Periode Update",
    y = "Jumlah Pengunjung",
    color = "Jenis Kelamin",
    shape = "Jenis Kelamin"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Laki-Laki" = "#34a246", "Perempuan" = "#105879")) +
  scale_shape_manual(values = c(16, 17))  # Different shapes for the sexes
