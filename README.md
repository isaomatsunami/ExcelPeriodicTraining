# ExcelPeriodicTraining
Excel/R/Pandas（どんなツールでも）を忘れないための練習帳。実務の95%はカバーするのでは？

## 重複のないデータ
data/NPL_batters.csvを使うこと（文字コードはUTF-8、選手名は姓名の間に空白がある）

#### Q1.1 打率の列を作る(打率＝安打/打数)
#### Q1.2 空白のない名前の列を作る（選手名の空白を削除する）
#### Q1.3 行番号の列を作る
#### Q1.4 本塁打ゼロの論理列を作る（本塁打を打っていれば1、なければ0）

### 並べ替え
#### Q1.5 打率の高い順、低い順に並べ直す
#### Q1.6 本塁打の多い順、ただし同じ場合は打率の高い順に並べ直す

### フィルター
#### Q2.1 中日の選手だけにする。その人数を表示する
#### Q2.2 阪神か広島の選手だけを表示（or条件）
#### Q2.3 打率が0.25以上の選手だけ
#### Q2.4 打率が0.2以上、0.25以下の選手だけ（and条件）
#### Q2.5 打率が0.1以下で本塁打が5本以下の選手だけ
#### Q2.6 右打ちで0.3以上の選手

### 集計
#### Q3.1 球団別に、打数と安打の合計をそれぞれ求める
#### Q3.2 チーム打率を計算し、高い順に並べ直す
#### Q3.3 球団別に最高打率選手だけを抜き出す
#### Q3.4 球団別に打率上位５人だけを抜き出す
#### Q3.5 全員に球団内打率順位の列を作る

### テーブル
#### Q4.1 縦に球団、横に安打、２塁打、３塁打、本塁打の合計となる表を作る

## 重複のあるデータ
data/BoxOffice2016.csvを使うこと
（１年間53週の順位、興行収入、映画館数などのデータ。WeekOfYearは第何週目か、ReleaseWeekは封切り何週目かを表す）

#### Q5.1 重複のない映画のリストを表示する
#### Q5.2 各映画の最終上映週を表示する
#### Q5.3 各映画の上映期間（ReleaseWeekを使わず、最初の週と最後の週の差+1を計算すること）

## 分かれたデータ
NPL_batters.csvと一緒にdata/moneyball2017.csvも使うこと（文字コードはshift-jis）
最終的に得たい情報は、安打1本の値段が安い選手。

#### Q6.1 ２本のファイルを名前で結合する（内部結合）
#### Q6.2 ２本のファイルをチームと名前で結合する（同姓同名対策）
#### Q6.3 NPL_battersにはあり、moneyballにはない選手のリスト
#### Q6.4 安打１本の値段の列を作り、並べ替える

この練習に使ったような綺麗なデータファイルが常に手に入るとは限らない。次の課題はデータ洗浄

#### Q7 NPL_batters.csvとmoneyball2017.csvで、表記の揺れ（変換ミスなど）で一致しない名前を探す

