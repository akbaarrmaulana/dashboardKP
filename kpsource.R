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
                    "belum Terverifikasi","Belum Ditindaklanjuti",
                    "Proses","Selesai","Tunda","Arsip")
coln <- c("Pengaduan",
          "belum Terverifikasi","Belum Ditindaklanjuti",
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
p1
