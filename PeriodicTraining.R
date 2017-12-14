# ExcelPeriodicTraining

## 重複のないデータ

library(tidyverse)
# tidyverseではread.csvではなくread_csvを使う
batters <- read.csv("data/NPL_batters.csv")
# 文字化けするようなら文字コードを指定するread.tableを使う
batters <- read.table("data/NPL_batters.csv", header=T, skip=0, sep=",", stringsAsFactors=F, fileEncoding="UTF-8")
head(batters, 5)

# A tibble: 5 x 23
チーム    年     選手名   左右 試合数  打席  打数  得点  安打 二塁打 三塁打 本塁打  塁打
<chr> <int>      <chr>  <chr>  <int> <int> <int> <int> <int>  <int>  <int>  <int> <int>
1 日本ハム  2017 淺間　大基 左打ち     19    44    42     2     7      3      0      0    10
2 日本ハム  2017 新垣　勇人 右打ち      1     0     0     0     0      0      0      0     0
3 日本ハム  2017 有原　航平 右打ち     25     4     4     1     1      0      0      0     1
4 日本ハム  2017 飯山　裕志 右打ち     10     4     4     0     0      0      0      0     0
5 日本ハム  2017 井口　和朋 右打ち     17     0     0     0     0      0      0      0     0
# ... with 10 more variables: 打点 <int>, 盗塁 <int>, 盗塁刺 <int>, 犠打 <int>, 犠飛 <int>,
#   四球 <int>, 故意四球 <int>, 死球 <int>, 三振 <int>, 併殺打 <int>

#### Q1.打率の列を作る(打率＝安打/打数)
batters$打率 <- batters$安打/batters$打数
# tidyverseの作法ではmutateを使う
batters <- mutate(batters, 打率=安打/打数)

#### Q2.苗字の列を作る（選手名の空白より前の部分）
# Excelでは簡単なこともRでは難しい
# stringrパッケージのstr_splitは文字列を切断してリストにする関数
#  a <- "淺間　大基"
#  str_split(a, "　")[[1]][1]
# あらかじめ空の苗字列を作っておく
batters$苗字 <- ""
for(i in 1:nrow(batters)){
  batters[i,]$苗字 <- str_split(batters[i,]$選手名, "　")[[1]][1]
}

#### Q3.行番号を作る
batters$行番号 <- 1:nrow(batters)

#### Q4.本塁打ゼロを作る（本塁打を売っていればTRUE）
batters$本塁打あり = batters$本塁打 > 0
# tidyverseなら
batters <- mutate(batters, 本塁打あり = 本塁打 > 0)
batters$本塁打あり

### 並べ替え
#### Q5.打率の高い順、低い順に並べ直す
batters <- batters[order(batters$打率, decreasing=TRUE), ]
# tidyverseなら
batters <- arrange(batters, desc(打率))
# 今村信貴と榎田大樹が10割

#### Q6.本塁打の多い順、ただし同じ場合は打率の高い順に並べ直す
batters <- batters[order(batters$本塁打,batters$打率, decreasing=TRUE), ]
# tidyverseなら
batters <- arrange(batters, desc(本塁打), desc(打率))
# 35本のゲレーロ、デスパイネでゲレーロが打率は上

### フィルター
#### Q7.中日の選手だけにする。その人数を表示する
chunichi <- batters[batters$チーム=="中日",]
print( nrow(chunichi) )
# tidyverseなら
filter(batters, チーム=="中日") %>% count()
# 61人

#### Q8.阪神か広島の選手だけ（or条件）
# ベクトルの論理演算子は&と|
TigersANDCarp <- batters[(batters$チーム=="阪神") | (batters$チーム=="広島") ,]
# tidyverseなら
filter(batters, (チーム=="阪神") | (チーム=="広島") ) %>% count()
# 106人

#### Q9.打率が0.25以上の選手だけ
batters[batters$打率>=0.25,]
# 330人

#### Q10.打率が0.2以上、0.25以下の選手だけ（and条件）
batters[(batters$打率>=0.2) & (batters$打率<=0.5),]
# 429人

#### Q11.打率が0.1以下で本塁打が5本以下の選手だけ
# tidyverseなら
filter(batters, (打率<=0.1) | (本塁打<=5) ) %>% count()
# 589人

#### Q12.右打ちで0.3以上の選手
# tidyverseなら
filter(batters, (打率>=0.3) | (左右=="右打ち") ) %>% count()
# 395人

### 集計１
#### Q13.球団別に、打数と安打の合計をそれぞれ求める
# 球団ごとに作業するしかない
sumAtBat <- sum(chunichi$打数)
sumHits <- sum(chunichi$安打)
print( sum(chunichi$安打)/sum(chunichi$打数)  )
# 中日のチーム打率は0.2467668
# tidyverseはこういう作業が簡潔になるように作られた
batters %>%
  group_by(チーム) %>%
  summarise( チーム打率=sum(安打)/sum(打数)  )

#### Q14.チーム打率を計算し、高い順に並べ直す
batters %>%
  group_by(チーム) %>%
  summarise( チーム打率=sum(安打)/sum(打数)  ) %>%
  arrange( desc(チーム打率) )


### 集計２
#### Q15.球団別に最高打率選手だけを抜き出す
chunichiByRate <- chunichi[order(chunichi$打率, decreasing = TRUE),]
print(chunichiByRate[1,])
#### Q16.球団別に打率上位５人だけを抜き出す
print(chunichiByRate[1:5,])
#### Q17.全員に球団内打率順位の列を作る
chunichiByRate$RankInTeam <- 1:nrow(chunichiByRate)
# この作業を12球団繰り返すしかない

#### Q15のtidyverse版
batters %>%
  group_by(チーム) %>%
  arrange( desc(打率) ) %>%
  summarise( チーム首位打者= first(選手名), チーム首位打者打率= first(打率) )

#### Q16のtidyverse版
batters %>%
  group_by(チーム) %>%
  arrange( desc(打率) ) %>%
  do( head(., 3) ) %>%
  data.frame()

# 参考:tibbleがtibbleを要素にできることを利用する例
top5 <- function(d){
  # 並べ替えて上位５人を返す関数
  d %>% arrange( desc(打率) ) %>% head(5)
}
read_csv("data/NPL_batters.csv") %>%
  mutate(打率=安打/打数) %>%
  select(チーム, 選手名, 打率) %>%
  group_by(チーム) %>%
  do( top5 = top5(.) ) %>%
  transmute( チーム, topPlayer=as.character(top5[1,][2]), topAverage=as.numeric(top5[1,][3]) ) %>%
  ungroup() %>%
  data.frame()

### テーブル
#### Q18.縦に球団、横に安打、２塁打、３塁打、本塁打の合計となる表を作る

# 1)テーブルをテーブルのまま集計する方法（素朴なやり方）
batters <- read_csv("data/NPL_batters.csv")
# byは、battersの９列目を、１列目でグループ化してsumで計算するという意味
SingleHit <- by(batters[,9], batters[, 1], sum)
TwoBase <- by(batters[,10], batters[, 1], sum)
ThreeBase <- by(batters[,11], batters[, 1], sum)
HRs <- by(batters[,12], batters[, 1], sum)
# チームの並びが同じだと想定して...
t <- rbind(SingleHit,TwoBase,ThreeBase,HRs)
# tは転置（行と列を入れ替える関数）
t(t)

# 2)reshape2でテーブルを溶かす方法(Excelのreverse pivotと同じ)
library(reshape2)

batters <- read_csv("data/NPL_batters.csv")
# 中身が文字列の列を全てつかって「溶かす」
meltedTable <- melt(batters, id=c("チーム","選手名","左右") )
チーム             選手名     左右 variable value
1         日本ハム         淺間　大基   左打ち       年  2017
2         日本ハム         新垣　勇人   右打ち       年  2017
3         日本ハム         有原　航平   右打ち       年  2017
...
# variableを欲しい要素のみにする
hitTable <- meltedTable[(meltedTable$variable=="安打" | meltedTable$variable=="二塁打" | meltedTable$variable=="三塁打" | meltedTable$variable=="本塁打"),]
# 溶かしたものをチーム基準で戻す（ExcelのPivotTableと同じ作業）
dcast(hitTable,チーム ~ variable, sum)

# 3)tidyverse的作法（2の方法をカッコよく書いただけ）
batters <- read_csv("data/NPL_batters.csv")
# select行は不要
batters %>% 
  # select('チーム', "選手名", '安打', '二塁打', '三塁打', '本塁打') %>%
  gather('安打', '二塁打', '三塁打', '本塁打', key='HitType', value="Count") %>%
  dcast(チーム ~ HitType, value.var="Count", sum)

## 重複のあるデータ
#### Q5.1 重複なしの上映映画リストを表示する
movies <- read.csv("data/BoxOffice2016.csv")
summary(movies)
unique(movies$Title)
# tidyverse風なら
movies %>% distinct(Title)

#### Q5.2 映画の最終上映週を表示する
# ４列目（題名）でグループ化して、１列目の最大値を求める
by(movies[,1], movies[,4], max)
# tidyverse風なら
movies %>% 
  group_by(Title) %>%
  summarise( lastWeek=max(WeekOfYear) ) %>%
  ungroup()

#### Q5.3 映画の上映期間（最初の週と最後の週の差）
# ちょっと難しい
firstWeek <- by(movies[,1], movies[,4], min)
firstWeekDf <- data.frame(title=rownames(firstWeek), first=as.vector(firstWeek) )
lastWeek <- by(movies[,1], movies[,4], max)
lastWeekDf <- data.frame(title=rownames(lastWeek), last=as.vector(lastWeek) )
movieTable <- merge(firstWeekDf,lastWeekDf, all=TRUE)
movieTable$duration <- movieTable$last - movieTable$first
# tidyverse風なら恐ろしく簡単
movies %>% 
  group_by(Title) %>%
  summarise( duration=max(WeekOfYear) - min(WeekOfYear) ) %>%
  ungroup()


## 分かれたデータ
#### Q6.1 ２本のファイルを名前で結合する（内部結合）
batters <- read.csv("data/NPL_batters.csv")
moneys <- read.csv("data/moneyball2017.csv")
# 文字化けするようならread.tableを使う（cp932はshift-jisのこと）
moneys <- read.table("data/moneyball2017.csv", header=T, skip=0, sep=",", stringsAsFactors=F, fileEncoding="cp932")
summary(batters)
summary(moneys)
merge(batters, moneys, by.x="選手名", by.y="name")
# これでは苗字のない外国人だけしか一致しない。空白は削除しなければならない
# a <- "山田　太郎"
# gsub("　","",a) --> "山田太郎"
batters$name <- gsub("　", "", batters$選手名)
mergedTable <- merge(batters, moneys, by.x="name", by.y="name")
# 597人
# all=TRUEにすると外部結合になる（一致していないものも表示される）
mergedTable <- merge(batters, moneys, by.x="name", by.y="name", all = TRUE)
# 微妙な表記の乱れがあるため、完全には一致していない

#### Q6.2 ２本のファイルをチームと名前で結合する（同姓同名対策）
# チームと名前を結合した列を作れば良い
batters$TeamAndName <- paste(batters$チーム, batters$name)
moneys$TeamAndName <- paste(moneys$team, moneys$name)
mergedTable <- merge(batters, moneys, by.x="TeamAndName", by.y="TeamAndName")
# 542人

# (tidyverse)同姓同名をさぐると...
batters %>%
  group_by(name) %>% 
  summarise(count=n()) %>%
  arrange( desc(count) )
# 実は、同姓同名はペーニャ、メヒアのみ。あとはトレードで記録が分かれている選手
# このように別のデータを結合するのは大仕事

#### Q6.3 NPL_battersにはあり、moneyballにはない選手のリスト
# all=Tで外部結合を求め、moneyが空欄(na)になっているもの
mergedTable <- merge(batters, moneys, by.x="name", by.y="name", all = TRUE)
onlyInBatters <- mergedTable[is.na(mergedTable$money), ]
# これが年俸調査が必要な選手のリストになる

#### Q6.4 安打１本の値段の列を作り、並べ替える
mergedTable$moneyPerHit <- mergedTable$money/mergedTable$安打
# 問題は無安打の選手は無限大になってしまうこと。is.finite関数はそれをチェックしてくれる
mergedTable <- mergedTable[is.finite(mergedTable$moneyPerHit), ]
mergedTable[order(mergedTable$moneyPerHit,decreasing=TRUE), ]
# 巨人の山口俊投手は2.3億円で安打1本

# tidyverse風なら綺麗に書けるが、これは試行錯誤の末なので誤解しないこと
moneys <- read.table("data/moneyball2017.csv", header=T, skip=0, sep=",", stringsAsFactors=F, fileEncoding="cp932")
read.csv("data/NPL_batters.csv") %>%
  mutate( name=gsub("　", "", 選手名) ) %>%
  inner_join( moneys, by=c("name"="name", "チーム"="team")) %>%
  select(チーム,選手名,安打,money) %>%
  mutate( moneyPerHit=money/安打 ) %>%
  filter( is.finite(moneyPerHit) ) %>%
  arrange( desc(moneyPerHit) )


## 汚いデータ
1. 単位付きデータを数値にする
2. 年号処理