

#=========================================================================
#     A Project for Funny
#     Data Used: http://www.mediafire.com/file/rwma97dtnbdars3/giao_su.rar
#=========================================================================

# Thiết lập đường dẫn cho các file dữ liệu cần phân tích: 
rm(list = ls())
path <- dir("F:/R_project/giao_su", full.names = TRUE)
path


#----------------------------
#  Viết một số  hàm hỗ trợ
#----------------------------
library(readxl)
library(tidyverse)
library(magrittr)

# Hàm đọc và xử lí thô dữ liệu cho năm 2011: 

import_gs <- function(path_to_file, sheet) {
  u <- read_excel(path_to_file, sheet = sheet) %>% 
    slice(-c(1:5))
  ten <- c("tt", "ho", "ten", "ngay_sinh", "gioi_tinh", 
           "nganh", "organization1", "que_quan", "code1", 
           "code2", "pre", "nganh2", "organization2", "organization3")
  names(u) <- ten
  return(u %>% select(tt, ho, ten, ngay_sinh, gioi_tinh, nganh, organization1, que_quan))
  
}


# Hàm tách  ra dữ  liệu về nơi sinh của các GS + PGS: 
library(stringr)

province <- function(x) {
  x %>% 
    str_replace_all(".*,", "") %>% 
    str_replace_all("Tỉnh", "") %>% 
    str_replace_all("tỉnh", "") %>% 
    str_replace_all("-", " ") %>% 
    str_replace_all("Thành phố", "") %>% 
    str_replace_all("  ", " ") %>% 
    str_replace_all("Tp. ", "") %>% 
    str_replace_all("\r\n", " ") %>% 
    str_trim() %>% 
    return()
}


#-----------------------------
#      Dữ liệu năm 2011
#-----------------------------

# Các Giáo Sư: 

gs2011 <- import_gs(path[3], 1) %>% 
  mutate(title = "GS", nam = 2011)

# Các PGS: 
pgs2011 <- import_gs(path[3], 2) %>% 
  mutate(title = "PGS", nam = 2012)

#  Hợp nhất các dữ liệu: 
all_df2011 <- bind_rows(gs2011, pgs2011)

# Tạo ra cột biến về năm được thụ phong: 

all_df2011 %<>% mutate(nam = 2011)

#-------------------------------------------------
#    Dữ liệu năm 2012  (bạn này cá biệt) 
#    không có thông tin về nơi sinh - quê quán
#-------------------------------------------------

gs2012 <- read_excel(path[1], sheet = 1) %>% 
  slice(-c(1:8))

ten2 <- c("tt", "ho", "ten", "ngay_sinh", "gioi_tinh", "nganh")
names(gs2012) <- ten2
gs2012 %<>% mutate(title = "GS")
pgs2012 <- read_excel(path[1], sheet = 2) %>% 
  slice(-c(1:8))

names(pgs2012) <- ten2
pgs2012 %<>% mutate(title = "PGS")

all_df2012 <- bind_rows(gs2012, pgs2012)
all_df2012 %<>% mutate(nam = 2012, que_quan = NA, organization1 = NA)

#---------------------
#  Dữ liệu năm 2013
#---------------------

gs2013 <- read_excel(path[4], sheet = 1) %>% 
  slice(-c(1:5))

ten3 <- c("tt", "ho",  "ten", "ngay_sinh", "gioi_tinh", "nganh",
          "organization1", "que_quan", "x8", "x9")

names(gs2013) <- ten3
gs2013 %<>% select(-x8, -x9) %>% mutate(title = "GS")

pgs2013 <- read_excel(path[4], sheet = 2) %>% 
  slice(-c(1:5))

names(pgs2013) <- ten3

pgs2013 %<>% select(-x8, -x9) %>% mutate(title = "PGS")

all_df2013 <- bind_rows(gs2013, pgs2013)

all_df2013 %<>% mutate(que_quan = province(que_quan), 
                       nam = 2013)


#------------------
#     Năm 2014
#------------------

gs2014 <- read_excel(path[2], sheet = 1) %>% 
  slice(-c(1:5))

gs2014 <- gs2014[, 1:8]
names(gs2014) <- c("tt", "ho", "ten", "ngay_sinh", "gioi_tinh", "nganh", 
                   "organization1", "que_quan")

gs2014 %<>% mutate(title = "GS")

pgs2014 <- read_excel(path[2], sheet = 2) %>% 
  slice(-c(1:5))

pgs2014 <- pgs2014[, 1:8]
names(pgs2014) <- c("tt", "ho", "ten", "ngay_sinh", "gioi_tinh", "nganh", 
                    "organization1", "que_quan")

pgs2014 %<>% mutate(title = "PGS")

all_df2014 <- bind_rows(gs2014, pgs2014)
all_df2014 %<>% mutate(que_quan = province(que_quan), 
                       nam = 2014)

#------------------
#     Năm 2015
#------------------

gs2015 <- read_excel(path[6], sheet = 1)

ten <- c("tt",  "ho_ten", "ngay_sinh", "gioi_tinh", "nganh", 
         "organization1", "que_quan", "X")

names(gs2015) <- ten
pgs2015 <- read_excel(path[6], sheet = 2)
names(pgs2015) <- ten

all_df2015 <- bind_rows(gs2015 %>% 
                          select(-X) %>% 
                          mutate(title = "GS"), 
                        pgs2015 %>% 
                          select(-X) %>% 
                          mutate(title = "PGS"))

all_df2015 %<>% mutate(que_quan = province(que_quan), 
                       nam = 2015, 
                       ho = NA, 
                       ten = NA)
#------------------
#     Năm 2016
#------------------

gs2016 <- read_excel(path[5], sheet = 1) %>% 
  slice(-c(1:9))

ten <- names(gs2016)
ten[1] <- "X__0"
names(gs2016) <- ten

gs2016 %<>% rename(tt = X__0, 
                   ho = X__1, 
                   ten = X__2, 
                   ngay_sinh = X__3, 
                   gioi_tinh = X__4, 
                   nganh = X__5, 
                   organization1 = X__6, 
                   que_quan = X__7) %>% select(-X__8, - X__9)



pgs2016 <- read_excel(path[5], sheet = 2) %>% 
  slice(-c(1:9))

names(pgs2016) <- ten

pgs2016 %<>% rename(tt = X__0, 
                    ho = X__1, 
                    ten = X__2, 
                    ngay_sinh = X__3, 
                    gioi_tinh = X__4, 
                    nganh = X__5, 
                    organization1 = X__6, 
                    que_quan = X__7) %>% select(-X__8, - X__9)


all_df2016 <- bind_rows(gs2016 %>% mutate(title = "GS"), 
                        pgs2016 %>% mutate(title =  "PGS"))

all_df2016 %<>% mutate(nam = 2016)

#------------------
#     Năm 2017
#------------------

gs2017 <- read_excel(path[7], sheet = 1) %>% 
  slice(-c(1:5))

names(gs2017) <- ten[-c(9, 10)]

gs2017 %<>% rename(tt = X__0, 
                   ho = X__1, 
                   ten = X__2, 
                   ngay_sinh = X__3, 
                   gioi_tinh = X__4, 
                   nganh = X__5, 
                   organization1 = X__6, 
                   que_quan = X__7)

gs2017 %<>% mutate(que_quan = province(que_quan), 
                   nam = 2017, 
                   title = "GS")


pgs2017 <- read_excel(path[7], sheet = 2) %>% 
  slice(-c(1:5))

names(pgs2017) <- ten[-c(9, 10)]

pgs2017 %<>% rename(tt = X__0, 
                    ho = X__1, 
                    ten = X__2, 
                    ngay_sinh = X__3, 
                    gioi_tinh = X__4, 
                    nganh = X__5, 
                    organization1 = X__6, 
                    que_quan = X__7)

pgs2017 %<>% mutate(que_quan = province(que_quan), 
                    nam = 2017, 
                    title = "PGS")

all_df2017 <- bind_rows(gs2017, pgs2017)

#------------------------------
#  Phân  tích dữ liệu có được
#------------------------------

# Số lượng PGS + GS được thụ phong: 

all_data <- rbind(all_df2011 %>% select(gioi_tinh, nganh, title, que_quan, organization1, nam), 
                  all_df2012 %>% select(gioi_tinh, nganh, title, que_quan, organization1, nam), 
                  all_df2013 %>% select(gioi_tinh, nganh, title, que_quan, organization1, nam), 
                  all_df2014 %>% select(gioi_tinh, nganh, title, que_quan, organization1, nam), 
                  all_df2015 %>% select(gioi_tinh, nganh, title, que_quan, organization1, nam), 
                  all_df2016 %>% select(gioi_tinh, nganh, title, que_quan, organization1, nam), 
                  all_df2017 %>% select(gioi_tinh, nganh, title, que_quan, organization1, nam))


library(ggthemes)
library(hrbrthemes)

# Có thể thấy năm 2017 số lượng tăng vọt:  
all_data %>% 
  group_by(nam) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(nam = factor(nam)) %>% 
  ggplot(aes(nam, n)) + 
  geom_col(fill = c("#104E8B")) + 
  theme_fivethirtyeight() + 
  geom_text(aes(label = n), color = "white", vjust = 1.3, size = 5) + 
  labs(x = NULL, y = NULL, 
       title = "The Number of of Associate Professors and Professors from 2011 to 2017", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn")


all_data %>% 
  group_by(nam, title) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(nam = factor(nam)) %>% 
  ggplot(aes(nam, n, fill = title)) + 
  geom_col() + 
  labs(x = NULL, y = NULL, 
       title = "The Number of of Associate Professors and Professors", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn") + 
  theme_fivethirtyeight() + 
  scale_fill_wsj() + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank())


all_data %>% 
  group_by(nam, title) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(nam = factor(nam)) %>% 
  ggplot(aes(nam, n, fill = title)) + 
  geom_col(position = "fill") + 
  labs(x = NULL, y = NULL, 
       title = "The percentage between Associate Professors and Professors", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn") + 
  theme_fivethirtyeight() + 
  scale_fill_wsj() + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  scale_y_percent()



# Viết hàm xử lí thông tin về ngành: 
library(stringr)

nganh_rename <- function(x) {
  x %>% 
    str_replace_all("\r\n", " ") %>% 
    str_trim() %>% 
    return()
}

all_data %<>% mutate(nganh = nganh_rename(nganh))

all_data$nganh %>% unique() -> k
k <- k[order(k)]

nganh_rename_lan2 <- function(x) {
  ELSE <- TRUE
  case_when(x == k[6] ~ k[5], 
            x == k[8] ~ k[7], 
            x == k[17] ~ k[16],
            x == k[20] ~ k[19], 
            x == k[22] ~ k[21], 
            x == k[26] ~ k[25], 
            x == k[40] ~ k[39], 
            x == k[44] ~ k[42], 
            ELSE ~ x)
}

all_data %<>% mutate(nganh = nganh_rename_lan2(nganh))

# 25 ngành có nghiều GS + PGS nhất: 
all_data %>% 
  filter(!is.na(nganh)) %>% 
  group_by(nganh) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n) -> u



u %>% 
  slice(1:25) %>% 
  ggplot(aes(reorder(nganh, n), n)) + 
  geom_col(fill = "#104E8B") + 
  geom_col(data = u %>% filter(nganh %in% c(k[19], k[21])), fill = c("#CD2626")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  geom_text(aes(label = n), color = "white", hjust = 1.2) + 
  labs(x = NULL, y = NULL, 
       title = "25 Fields with the Largest Number of\nAssociate Professors and Professors", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn")

# 20 ngành còn lại có số GS/PGS  thấp nhất: 
u %>% 
  slice(26:45) %>% 
  ggplot(aes(reorder(nganh, n), n)) + 
  geom_col(fill = "#104E8B") +   
  coord_flip() + 
  theme_fivethirtyeight() + 
  geom_text(aes(label = n), color = "white", hjust = 1.2) + 
  labs(x = NULL, y = NULL, 
       title = "20 Fields with the Smallest Number of\nAssociate Professors and Professors", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn")



# Sử dụng hàm: 
all_data %<>% 
  mutate(que_quan = province(que_quan)) %>% 
  filter(!is.na(que_quan), que_quan != "Trung Quốc")

all_data$que_quan %>% unique() -> m
m <- m[order(m)]

# Lấy dữ liệu địa lí từ gói raster: 
library(raster)
# Lấy dữ liệu địa lí cho VN ở cấp tỉnh: 
vietnam <- getData("GADM", country = "Vietnam", level = 1)
detach(package:raster)
vietnam_df_province <- vietnam %>% fortify(region = "NAME_1")
vietnam_df_province$id %>% unique() -> v

# Viết hàm rename lại tỉnh: 
rename_province <- function(x) {
  case_when(x == m[1] ~ v[1], 
            x == m[2] ~ v[2], 
            x == m[3] ~ v[3], 
            x == m[4] ~ v[3], 
            x == m[5] ~ v[4], 
            x == m[6] ~ v[5], 
            x == m[7] ~ v[6], 
            x == m[8] ~ v[7], 
            x == m[9] ~ v[8],
            x == m[10] ~ v[8], 
            x == m[11] ~ v[9], 
            x == m[12] ~ v[11], 
            x == m[13] ~ v[54], 
            x == m[14] ~ v[12], 
            x == m[15] ~ v[13],
            x == m[16] ~ v[14], 
            x == m[17] ~ v[15], 
            x == m[18] ~ v[16],
            x == m[19] ~ v[17], 
            x == m[20] ~ v[18], 
            x == m[21] ~ v[17],
            x == m[22] ~ v[25], 
            x == m[23] ~ v[22], 
            x == m[24] ~ v[23], 
            x == m[25] ~ v[24], 
            x == m[26] ~ v[25], 
            x == m[27] ~ v[27], 
            x == m[28] ~ v[28], 
            x == m[29] ~ v[27], 
            x == m[30] ~ v[29], 
            x == m[31] ~ v[29], 
            x == m[32] ~ v[22], 
            x == m[33] ~ v[30],
            x == m[34] ~ v[54], 
            x == m[35] ~ v[54], 
            x == m[36] ~ v[31], 
            x == m[37] ~ v[32],
            x == m[38] ~ v[33], 
            x == m[39] ~ v[34], 
            x == m[40] ~ v[35], 
            x == m[41] ~ v[36], 
            x == m[42] ~ v[37], 
            x == m[43] ~ v[38], 
            x == m[44] ~ v[39], 
            x == m[45] ~ v[40],
            x == m[46] ~ v[28], 
            x == m[47] ~ v[41], 
            x == m[48] ~ v[41], 
            x == m[49] ~ v[48], 
            x == m[50] ~ v[42], 
            x == m[51] ~ v[43],
            x == m[52] ~ v[43], 
            x == m[53] ~ v[44],
            x == m[54] ~ v[45], 
            x == m[55] ~ v[47],
            x == m[56] ~ v[46], 
            x == m[57] ~ v[47],
            x == m[58] ~ v[47], 
            x == m[59] ~ v[48],
            x == m[60] ~ v[49], 
            x == m[61] ~ v[50],
            x == m[62] ~ v[50], 
            x == m[63] ~ v[51],
            x == m[64] ~ v[52], 
            x == m[65] ~ v[62],
            x == m[66] ~ v[53], 
            x == m[67] ~ v[54],
            x == m[68] ~ v[54], 
            x == m[69] ~ v[55],
            x == m[70] ~ v[55], 
            x == m[71] ~ v[56],
            x == m[72] ~ v[57],
            x == m[73] ~ v[57],
            x == m[74] ~ v[57],
            x == m[75] ~ v[58],
            x == m[76] ~ v[59],
            x == m[77] ~ v[50],
            x == m[78] ~ v[60],
            x == m[79] ~ v[61],
            x == m[80] ~ v[62],
            x == m[81] ~ v[63])
}


all_data %<>% mutate(que_quan = rename_province(que_quan))

all_data %>% 
  filter(!is.na(que_quan)) %>% 
  group_by(que_quan) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n) -> giao_su


giao_su %<>% rename(id = que_quan)

dplyr::setdiff(vietnam_df_province$id %>% unique(), 
               giao_su$id %>% unique()) -> khong_co_gs

df1 <- data.frame(id = khong_co_gs, n = c(rep(0, 5)))
# Sô lượng các PGS/GS theo nơi sinh (thứ tự giảm dần): 
giao_su %>% knitr::kable()
giao_su <- bind_rows(giao_su, df1)
giao_su_prov <- inner_join(giao_su, vietnam_df_province, by = "id")



library(viridis)

giao_su_prov %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = n), color = "grey30") +  
  scale_fill_viridis(direction = -1, 
                     option = "B", "Density") + 
  theme(legend.position = c(0.22, 0.45), 
        axis.text = element_blank(),
        panel.grid = element_blank()) + 
  labs(title = "Associate Professors and Professors\nDensity by Birth Place", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn")



# 20 tỉnh  thành sinh ra nhiều GS/PGS  nhất. Điều đáng ngạc 
# nhiên  là SG - khu vực địa lí đông  dân nhất nước và cũng 
# là một trung tâm giáo dục của  cả nước lại có vị trí thấp: 
giao_su %>% 
  slice(1:20) %>% 
  ggplot(aes(reorder(id, n), n)) + 
  geom_col(fill = c("#104E8B")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  geom_text(aes(label = n), color = "white", hjust = 1.2) + 
  labs(x = NULL, y = NULL, 
       title = "The Number of of Associate Professors and Professors\nby Birth Place for top 20 Provinces", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn")

# Nhóm 20 tỉnh thuộc nhóm giữa: 
giao_su %>% 
  slice(21:40) %>% 
  ggplot(aes(reorder(id, n), n)) + 
  geom_col(fill = c("#104E8B")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  geom_text(aes(label = n), color = "white", hjust = 1.2) + 
  labs(x = NULL, y = NULL)

# Nhóm 23 tỉnh thuộc nhóm cuối: 
giao_su %>% 
  slice(41:63) %>% 
  ggplot(aes(reorder(id, n), n)) + 
  geom_col(fill = c("#104E8B")) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  geom_text(aes(label = n), color = "white", hjust = 1.2) + 
  labs(x = NULL, y = NULL) + 
  scale_y_continuous(breaks = seq(1, 10, by = 1))



# Tỉ lệ  chung có vẻ là cứ 3 Nam thì có một đồng nghiệp nữ. 
# Và tỉ lệ GS/PGS là nữ có xu hướng tăng theo thời gian: 
all_data %>% 
  group_by(nam, gioi_tinh) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(nam = as.factor(nam)) %>% 
  ggplot(aes(nam, n, fill = gioi_tinh)) + 
  geom_col(position = "fill") + 
  labs(x = NULL, y = NULL, 
       title = "The Percentage between Associate Professors\nand Professors by Gender", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn") + 
  theme_fivethirtyeight() + 
  scale_fill_wsj() + 
  theme(legend.position = "top") + 
  theme(legend.title = element_blank()) + 
  scale_y_percent()

# Phân bố GS/PGS theo giới tính ứng với các ngành: 
u <- all_data %>% 
  group_by(nganh, gioi_tinh) %>% 
  count() %>% 
  ungroup()

u %>% 
  spread(gioi_tinh, n) -> u_wide

names(u_wide) <- c("field", "Female", "Male")

convert_na <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

u_wide %<>% mutate_at(.vars = c("Female", "Male"), 
                      .funs = convert_na)

u_wide %<>% mutate(total = Male + Female, 
                   m_rate = Male / total, 
                   f_rate = Female / total, 
                   label = paste0(100*round(m_rate, 4), "%"))

u_wide %<>% 
  ungroup() %>% 
  arrange(-m_rate) %>% 
  mutate(field = factor(field, levels = field))


u_wide %>% 
  ggplot() + 
  geom_segment(aes(x = 0, 
                   xend = m_rate, 
                   y = field, 
                   yend = field, 
                   color = "m_rate"), size = 5) + 
  geom_segment(aes(x = m_rate, 
                   xend = m_rate + f_rate, 
                   y = field, 
                   yend = field, 
                   color = "f_rate"), size = 5) + 
  geom_text(aes(x = 0, y = field, label = label), 
            color = "white", hjust = -0.2) + 
  theme_minimal() + 
  theme(legend.position = "top") + 
  labs(x = NULL, y = NULL, 
       title = "The ratio is promoted from Associate Professor to Professor", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn") + 
  scale_color_manual(values = c('#e41a1c','#4daf4a'), 
                     name = "", 
                     labels = c("Female", "Male"))


# Hà Tĩnh, Quảng Bình và Sài Gòn là ba  tỉnh  có tỉ lệ 
# được phong GS từ PGS cao nhất: 
all_data %>% 
  filter(que_quan %in% giao_su$id[1:25]) %>% 
  group_by(title, que_quan) %>% 
  count() %>% 
  ungroup() -> giao_su_top25

giao_su_top25 %>% 
  spread(title, n) -> u_wide

u_wide %<>% mutate(total = GS + PGS, 
                   m_rate = GS / total, 
                   f_rate = PGS / total, 
                   label = paste0(100*round(m_rate, 4), "%"))

u_wide %<>% 
  ungroup() %>% 
  arrange(m_rate) %>% 
  mutate(que_quan = paste(que_quan, label, sep = ": ")) %>% 
  mutate(que_quan = str_replace_all(que_quan, "city|-", "")) %>% 
  mutate(que_quan = factor(que_quan, levels = que_quan))


u_wide %>% 
  ggplot() + 
  geom_segment(aes(x = 0, 
                   xend = m_rate, 
                   y = que_quan, 
                   yend = que_quan, 
                   color = "m_rate"), size = 5) + 
  geom_segment(aes(x = m_rate, 
                   xend = m_rate + f_rate, 
                   y = que_quan, 
                   yend = que_quan, 
                   color = "f_rate"), size = 5) + 
  theme_minimal() + 
  theme(legend.position = "top") + 
  labs(x = NULL, y = NULL, 
       title = "Distribution of Associate Professors by Field and Gender", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn") + 
  scale_color_manual(values = c('#e41a1c','#4daf4a'), 
                     name = "", 
                     labels = c("Associate Professor", "Professor"))

# Hệ số hồi quy của PGS là 0.084 (biến phụ thuộc là GS): 

all_data %>% 
  filter() %>% 
  group_by(que_quan, title) %>% 
  count() %>% 
  spread(title, n) %>% 
  ungroup() -> gs_pgs

gs_pgs %>% 
  lm(GS ~ PGS, data = .) %>% 
  summary()
