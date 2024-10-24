library(sf)
jatim <- st_read("jatim.shp")
jatim$ADM2_EN

library(readxl)
data <- read_excel("Jumlah pengaduan berdasarkan t.xlsx", 
                sheet = "Worksheet")
paste("Indo","lur", sep = " ")
unique(data$Tahun)
dd <- paste("Januari","2023", sep = " ")
df <- data %>% filter(periode_update==dd)
colnames(data) <- c("periode_update","Bulan","Tahun","KabupatenKota","Pengaduan",
                    "Belum Terverifikasi","Belum Ditindaklanjuti",
                    "Proses","Selesai","Tunda","Arsip")
coln <- c("Pengaduan",
          "Belum Terverifikasi","Belum Ditindaklanjuti",
          "Proses","Selesai","Tunda","Arsip")

df2023 <- data %>% filter(Tahun==2023)
df2024 <- data %>% filter(Tahun==2024)

aa <- data %>% filter(periode_update == "Januari 2023")
md <- jatim %>% 
  left_join(aa, by = c("ADM2_EN" = "KabupatenKota"))

p1 <- ggplot(data = md) +
  geom_sf(aes(fill = Pengaduan)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Pengaduan") +
  theme_minimal()+
  xlim(111, 115) + 
  ylim(9, 6.5) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20))

dtotma <- data %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(Pengaduan, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_max(order_by = total_pengaduan, n = 5)
dtot23ma <- df2023 %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(Pengaduan, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_max(order_by = total_pengaduan, n = 5)
dtot24ma <- df2024 %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(Pengaduan, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_max(order_by = total_pengaduan, n = 5)
dtotmi <- data %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(Pengaduan, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_min(order_by = total_pengaduan, n = 5)
dtot23mi <- df2023 %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(Pengaduan, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_min(order_by = total_pengaduan, n = 5)
dtot24mi <- df2024 %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(Pengaduan, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_min(order_by = total_pengaduan, n = 5)


p2 <- ggplot(dtot24, aes(x = reorder(KabupatenKota, -total_pengaduan), y = total_pengaduan)) + 
  geom_bar(stat = "identity", fill = "maroon") +  
  labs(x = "Kabupaten/Kota", y = "Jumlah Pengaduan") +  
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, margin = margin(t = 10))) + 
  geom_text(aes(label = total_pengaduan), vjust = -0.5, 
            color = "black", size = 3) +
  coord_flip()
p2

kota <- data[grepl("Kota", data$KabupatenKota), ]
kab <- data[!grepl("Kota", data$KabupatenKota), ]

kota2023 <- kota %>% filter(Tahun==2023)
kota2024 <- kota %>% filter(Tahun==2024)
kab2023 <- kab %>% filter(Tahun==2023)
kab2024 <- kab %>% filter(Tahun==2024)
