library(sf)
library(lubridate)
jatim <- st_read("jatim.shp")
jatim$ADM2_EN

library(readxl)
data <- read_excel("Jumlah pengaduan berdasarkan t.xlsx", 
                sheet = "Worksheet")
paste("Indo","lur", sep = " ")
unique(data$Tahun)
dd <- paste("Januari","2023", sep = " ")
df <- data %>% filter(periode_update==dd)
colnames(data) <- c("periode_update","Bulan","Tahun","KabupatenKota","All",
                    "Belum Terverifikasi","Belum Ditindaklanjuti",
                    "Diproses","Selesai","Ditunda","Diarsip")
coln <- c("All",
          "Belum Terverifikasi","Belum Ditindaklanjuti",
          "Diproses","Selesai","Ditunda","Diarsip")

df2023 <- data %>% filter(Tahun==2023)
df2024 <- data %>% filter(Tahun==2024)

aa <- data %>% filter(periode_update == "Januari 2023")
md <- jatim %>% 
  left_join(aa, by = c("ADM2_EN" = "KabupatenKota"))

p1 <- ggplot(data = md) +
  geom_sf(aes(fill = All)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(fill = "Pengaduan") +
  theme_minimal()+
  xlim(111, 115) + 
  ylim(9, 6.5) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20))

dtotma <- data %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(All, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_max(order_by = total_pengaduan, n = 5)
dtot23ma <- df2023 %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(All, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_max(order_by = total_pengaduan, n = 5)
dtot24ma <- df2024 %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(All, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_max(order_by = total_pengaduan, n = 5)
dtotmi <- data %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(All, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_min(order_by = total_pengaduan, n = 5)
dtot23mi <- df2023 %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(All, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))%>% 
  slice_min(order_by = total_pengaduan, n = 5)
dtot24mi <- df2024 %>% 
  group_by(KabupatenKota) %>% 
  summarize(total_pengaduan = sum(All, na.rm = TRUE)) %>% 
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

kabb <- kab
kotaa <- kota
kabb$kabkott <- paste("Kabupaten",kab$KabupatenKota)
kotaa$kabkott <- kotaa$KabupatenKota
dataa <- bind_rows(kabb, kotaa) %>%
  arrange(Tahun, Bulan)
dataa <- dataa %>%
  mutate(
    periode_update = factor(periode_update, levels = c(
      "Januari 2023", "Februari 2023", "Maret 2023", 
      "April 2023", "Mei 2023", "Juni 2023", 
      "Juli 2023", "Agustus 2023", "September 2023", 
      "Oktober 2023", "November 2023", "Desember 2023",
      "Januari 2024", "Februari 2024", "Maret 2024", 
      "April 2024", "Mei 2024", "Juni 2024", 
      "Juli 2024", "Agustus 2024", "September 2024", 
      "Oktober 2024", "November 2024", "Desember 2024"
    ), ordered = TRUE)
  ) %>%
  arrange(periode_update)

data2023 <- dataa %>% filter(Tahun==2023)
data2024 <- dataa %>% filter(Tahun==2024)
d2023 <- data2023 %>% 
  group_by(kabkott) %>% 
  summarize(total_pengaduan = sum(All, na.rm = TRUE)) %>% 
  arrange(desc(total_pengaduan))
d2023[d2023$kabkott=="Kota Surabaya", "total_pengaduan"]

colm <- c("Total Pengaduan","Pengaduan Belum Terverifikasi",
          "Pengaduan Belum Ditindaklanjuti","Pengaduan Diproses",
          "Pengaduan Selesai","Pengaduan Ditunda","Pengaduan Diarsip")
peng <- data.frame(colm,coln)
aaa <- peng[peng$colm=="Total Pengaduan","coln"]

df2023t <- data2023 %>%
  group_by(kabkott) %>%
  summarise(across(All:Diarsip,sum))
df2024t <- data2024 %>%
  group_by(kabkott) %>%
  summarise(across(All:Diarsip,sum))
dataat <- dataa %>%
  group_by(kabkott) %>%
  summarise(across(All:Diarsip,sum))
df2023t[df2023t$kabkott=="Kota Surabaya","All"][[1]]
df2023k <- data2023 %>% filter(kabkott=="Kota Surabaya")

Sys.setlocale("LC_TIME", "id_ID.UTF-8")
dfk23 <- data2023
dfk24 <- data2024
dfk <- dataa

dfk23$periode_update <- dmy(paste("01",dfk23$periode_update))
dfk24$periode_update <- dmy(paste("01",dfk24$periode_update))
dfk$periode_update <- dmy(paste("01",dfk$periode_update))

df2023k <- dfk23 %>% filter(kabkott=="Kota Surabaya")

ggplot(df2023k, aes(x = periode_update, y = All)) +
  geom_line(color = "maroon", size = 1) +
  geom_point(color = "orange", size = 4) + # menambahkan titik pada garis
  labs(title = "Jumlah Pengaduan di Kota Surabaya per Bulan",
       x = "Periode Update",
       y = "Jumlah Pengaduan") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month")

####
defi <- data.frame(
  Variabel = c("Kabupaten/Kota", "All", "Belum Terverifikasi", 
               "Belum Ditindaklanjuti", "Diproses", "Selesai", "Ditunda", "Diarsip"),
  Definisi_Operasional = c(
    "Nama kabupaten atau kota tempat pengaduan terjadi. ",
    "Total jumlah pengaduan yang masuk pada periode tersebut untuk kabupaten atau kota yang bersangkutan.",
    "Jumlah pengaduan yang belum diverifikasi atau dicek kebenarannya oleh pihak terkait.",
    "Jumlah pengaduan yang sudah diverifikasi tetapi belum mendapatkan tindak lanjut atau respons dari pihak terkait.",
    "Jumlah pengaduan yang sedang dalam proses penyelesaian atau sedang ditangani.",
    "Jumlah pengaduan yang sudah selesai ditindaklanjuti atau diselesaikan oleh pihak terkait.",
    "Jumlah pengaduan yang ditunda penyelesaiannya karena alasan tertentu yang memerlukan penundaan.",
    "Jumlah pengaduan yang telah diarsipkan atau disimpan untuk catatan, mungkin karena sudah tidak memerlukan tindak lanjut lebih lanjut."
  )
)
