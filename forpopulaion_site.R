#課題:90年から24年までの人口比率の変化を
library(readxl)
library(tidyverse)
library(lubridate)
#データの読み込み
population95_99<- read_excel("05k3d-05.xls")
population00_04<- read.csv("FEH_00200524_250416132429.csv",fileEncoding = "Shift_JIS")

population05_09<- read.csv("FEH_00200524_250416132859.csv",fileEncoding = "Shift_JIS")
population10_14 <- read.csv("FEH_00200524_250416133015.csv",fileEncoding = "Shift_JIS")
population15_19 <- read.csv("FEH_00200524_250416133159.csv",fileEncoding = "Shift_JIS")

population20_24<- read.csv("FEH_00200524_250416133655.csv",fileEncoding = "Shift_JIS")


#データの加工
#初めに同じ変数を持つデータを結合する
#00~14,15~24はそれぞれ同じ変数の数なのでそれを縦に結合する
population00_14 <- bind_rows(population00_04,population05_09,population10_14)

population15_24 <- bind_rows(population15_19,population20_24)

View(population00_14)
View(population15_24)
#これらをさらに結合するために、行の整理をして、その後結合する
colnames(population00_14)
colnames(population15_24)

#名称の変更
population00_14 <- population00_14 |> 
rename(resion="全国.都道府県",gender="男女別",allage="年齢5歳階級",time="時間軸.年月日現在.")

population15_24 <- population15_24 |> 
  rename(resion="全国.都道府県",gender="男女別",allage="年齢5歳階級",pop="人口",time="時間軸.年月日現在.")

#行の抽出
pop00_14a <- population00_14 |>
  filter(gender=="男女計"&allage=="総数")


#15~24についても
pop15_24a <- population15_24 |>
  filter(gender=="男女計",allage=="総数",pop=="総人口")
#列の抽出
pop00_14b <- pop00_14a |> 
  select("gender","allage","resion","time","unit","value")

pop15_24b <- pop15_24a |> 
  select("gender","allage","resion","time","unit","value")

pop00_24 <- bind_rows(pop00_14b,pop15_24b)
#データのさらなる調整(日付や人口を計算しやすい数値にする)
pop00_24 <- pop00_24 |> 
  mutate(value=value*1000,time=ymd(time)) |> 
  select(resion,time,value) 
#年だけにすしてnumericにする
pop00_24$time <- format(pop00_24$time, "%Y") 
pop00_24 <-pop00_24 |> 
  mutate(time=as.numeric(pop00_24$time))

##95年から99年の加工(いらない行と列を消す)##
colnames(population95_99)
pop95_99 <- population95_99 |> select(-1,-2,-c(9:18)) |> 
  slice(-c(1:11)) |> slice(-c(49:51))
#列名変更など
pop95_99[1,1] <- "全国" 
names(pop95_99)[c(1:6)] <- c("resion",1995,1996,1997,1998,1999)
#データ型を変更
pop95_99[,2:6] <- lapply(pop95_99[, 2:6], as.numeric)
#数値をそろえる
pop95_99[,2:6] <- pop95_99[,2:6]*1000 

#結合するためにデータを縦にする
pop95_99long <- pop95_99 |> 
  pivot_longer(cols="1995":"1999",
                             names_to="time",
                             values_to = "value")
#年データをdateにして結合する
pop95_99long <-   pop95_99long |> 
  mutate(time=as.numeric(time))
pop <- bind_rows(pop00_24,pop95_99long)
#空白を消す
library(stringr)
pop <- pop |> 
  mutate(resion=str_trim(pop$resion))
#各年各都道府県の人口比率を出す
japan<- pop |> 
  filter(resion=="全国")
pop_up <- merge(pop,japan,by="time")
pop_up <- pop_up |> 
  mutate(rperj=value.x/value.y)
#年別人口増加率を連鎖式で計算

popchange <- pop_up |> 
  select(-resion.y) |> 
  rename(resion=resion.x,eachpop=value.x)|> 
  arrange(resion, time) |>   # 都道府県と年でソート
  group_by(resion) |>     # 都道府県ごとにグループ化
  mutate(change_rate = (rperj - lag(rperj)) / lag(rperj)) |> 
  ungroup() 
##gglotの出番##
popchange %>% filter(resion %in% c("山形県", "東京都")) |> 
ggplot() +
  geom_line(aes(x = time, y = change_rate, color = resion)) +
  scale_x_continuous(
    breaks = seq(1995, 2024, by = 2))+
  labs(x = "年", y = "人口比率の変化率", color = "都道府県") +
  theme_minimal()