
#==================================
#     A Project for Funny
#==================================

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

import_gs <- function(path_to_file, sheet) {
  u <- read_excel(path_to_file, sheet = sheet) %>% 
    slice(-c(1:5))
  ten <- c("tt", "ho", "ten", "ngay_sinh", "gioi_tinh", 
           "nganh", "organization1", "que_quan", "code1", 
           "code2", "pre", "nganh2", "organization2", "organization3")
  names(u) <- ten
  return(u %>% select(tt, ho, ten, ngay_sinh, gioi_tinh, nganh, organization1, que_quan))
  
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

# Hàm tách  ra dữ  liệu về nơi sinh của các GS + PGS: 

library(stringr)
province <- function(x) {
  x %>% 
    str_replace_all(".*,", "") %>% 
    return()
}

# Dùng hàm và tạo ra cột biến về năm được thụ phong: 

all_df2011 %<>% mutate(que_quan = province(que_quan), 
                       nam = 2011)



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

all_df2012 %<>% mutate(nam = 2012, que_quan = NA)

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
                       nam = 2015)

all_df2015 %<>% mutate(ho = NA, ten = NA)

# all_df2015$que_quan %>% unique() -> u
# 
# # Cần xử lí đặc biệt với các tỉnh thành cho dữ liệu của năm 2015: 
# rename_province <- function(x) {
#   ELSE <- TRUE
#   case_when(x == u[17] ~ "Bắc Giang", 
#             x == u[27] ~ "Nam Định", 
#             x == u[28] ~ "Hậu Giang", 
#             x == u[29] ~ "Tây Ninh", 
#             x == u[35] ~ "Hồ Chí Minh", 
#             x == u[39] ~ "Thừa Thiên Huế", 
#             x == u[44] ~ "Phú Yên", 
#             x == u[48] ~ "Thanh Hóa", 
#             x == u[49] ~ "Ninh Bình", 
#             x == u[51] ~ "Hồ Chí Minh", 
#             x == u[54] ~ "Hải Dương", 
#             x == u[55] ~ "Bạc Liêu", 
#             x == u[56] ~ "Bắc Ninh", 
#             x == u[58] ~ "Long An", 
#             x == u[59] ~ "Tuyên Quang", 
#             x == u[60] ~ "Bình Định", 
#             x == u[61] ~ "Quảng Bình", 
#             x == u[62] ~ "Khánh Hòa", 
#             x == u[63] ~ "Ninh Thuận", 
#             x == u[65] ~ "Hưng Yên", 
#             x == u[69] ~ "Hồ Chí Minh", 
#             ELSE ~ x)
# 
# }



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

all_data <- rbind(all_df2011 %>% select(nganh, nam, title), 
                  all_df2012 %>% select(nganh, nam, title), 
                  all_df2013 %>% select(nganh, nam, title), 
                  all_df2014 %>% select(nganh, nam, title), 
                  all_df2015 %>% select(nganh, nam, title), 
                  all_df2016 %>% select(nganh, nam, title), 
                  all_df2017 %>% select(nganh, nam, title))


theme_set(theme_minimal())
all_data %>% 
  group_by(nam) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(nam = factor(nam)) %>% 
  ggplot(aes(nam, n)) + 
  geom_col() + 
  geom_text(aes(label = n), color = "white", vjust = 1.3) + 
  labs(x = NULL, y = NULL, 
       title = "The Number of of Associate Professors and Professors", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn")


library(ggthemes)

# Có thể thấy năm 2017 số lượng tăng vọt:  
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


library(hrbrthemes)
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
  coord_flip() + 
  scale_y_percent()

  
  
 


