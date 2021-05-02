###################################################################
# いじっておぼえるR
# 7th. April. 2021
# Yuzuru Utsunomiya, Ph. D.
#
# よく使うキーバインド
# Shift+矢印:部分選択
# Ctrl+Enter:選択部分を実行
# Ctrl+s:上書き保存
# Ctrl+Shift+c:選択部分をコメントアウト
# 
# くわしいことはHelp -> Cheetsheetsからチートシートをダウンロード

# tidyverseパッケージをインストール
# 1回インストールするだけでよい。起動するたびにしなくてもよい
# インストールさえしてしまえばコメントアウトしてもよい。
# install.packages("tidyverse")
# install.packages("khroma")

# ---- read.library ----
# tidyverseパッケージを読み込む
# 起動するたびにする。
library(tidyverse)
library(khroma)

# irisデータで散布図を書いてみる
# irisデータは、統計解析パッケージのお勉強に最もよく使われるデータ
# アヤメの花のパーツ寸法に関するデータ
# オブジェクトに図を代入
iris_scatterplot <- 
  # データをパイプで送り込む
  iris %>% 
  # 作図領域をつくる
  ggplot2::ggplot(
    data = ., 
    aes(
      x = Sepal.Length, # x軸に使う変数
      y = Petal.Length, # y軸に使う変数
      color = Species   # 塗り分けに使う変数
    )
  ) +
  # 散布図を描く。
  geom_point() + 
  # テーマを適用する。
  theme_classic()

# オブジェクトを表示する
iris_scatterplot

# オブジェクトを事後的に編集もできますよ
# もちろん各形式にて保存もOK
iris_scatterplot + 
  labs(
    title = "S.Length and P. Length by species"
  ) + 
  scale_color_okabeito() +
  theme(
    axis.text = element_text(
      size = 12
    ),
    legend.text = element_text(
      size = 12
    ),
    legend.position = c(0.8,0.2),
  )
# save the figure
ggsave("iris_scatterplot.png")
#
##
### --- END --- ###
# 
# 
# ---- line.plot ----
# 折れ線グラフを描く
# 
# 1.折れ線グラフ用データセットを読み込んでTidyな形にする。
# データはILOからダウンロードしたものを加工してつくった。
working_hours <- 
  # データを読み込む
  # readxlパッケージはtidyverseパッケージにバンドルされる。
  # library(tidyverse)すれば自動的に読み込まれる
  readxl::read_excel(
    # ファイル名と場所を指定する
    path = "working_hours.xlsx",
    # MSExcelファイル内にあるシートを選ぶ
    sheet = "Thailand",
    # 列名が設定されているか指定する
    col_names = TRUE
    ) %>% 
  # データを整形する
  # tidyrパッケージはtidyverseパッケージにバンドルされる。
  # library(tidyverse)すれば自動的に読み込まれる
  # 読み込んだ元データとtidyに整形したデータとを見比べると、差異がわかる
  # Rはtidyなデータが大好き。
  tidyr::pivot_longer(
    # 整形対象となる列を選ぶ
    # くわしくは竹澤先生のサイトを参照
    # http://cse.naro.affrc.go.jp/takezawa/r-tips/r/13.html
    cols = colnames(.)[c(6:9)], # .は、.があるところにデータが入るという意味。
    # 整形対象列名を格納する列につける名前を決める
    names_to = "Status",
    # 整形対象列にはいっていたデータを格納する列につける名前を決める
    values_to = "Number"
  ) 
# 
# 2.基本の折れ線グラフを描く
# 黒い線とマーカーがついてるだけ。
working_hours_thailand_01 <- 
  working_hours %>% 
  # 必要なデータだけを取り出す。
  # 性別：合計
  # 産業：総計
  # 就業状態：ぜんぶ
  # 
  # 演算子(==やら&やら)は重要だから覚えましょー
  # くわしくは竹澤先生のサイト参照
  # http://cse.naro.affrc.go.jp/takezawa/r-tips/r/28.html
  dplyr::filter(
    Sex == "Total" & Economic_activities == "Total" & Status == "Total"
  ) %>% 
  # おなじみggplot
  ggplot2::ggplot(
    # 作図領域をつくる
    aes(
      x = Time,
      y = Number
      )
  ) +
  # 折れ線グラフを描く
  geom_line() +
  # 折れ線グラフx軸メモリを設定する
  # 設定しないと中途半端な目盛りになる。年号なのになぜか小数点がつくとか。
  scale_x_continuous(
    limits = c(2010, 2019),# 区間の両端を設定する
    breaks = seq(2010, 2019, 1)　# x軸目盛り間隔を設定する
  ) + 
  # 折れ線グラフy軸目盛りを設定する
  # 内容はx軸と同様
  scale_y_continuous(
    limits = c(40,45),
    breaks = seq(40, 45, 1)
  ) +
  # 折れ線グラフにマーカーをつける
  geom_point(
    size = 3 # マーカーサイズ 
  ) +
  # 表題と副題とx軸名とy軸名を設定する
  # 日本語も使えるが、設定が面倒。日本語をつかわなければ面倒はない
  labs(
    title = "Mean weekly working hours per capita",
    subtitle = "(Thailand, 2010-2019, by Labour Force Survey)",
    x = "Year",
    y = "Working hours (Unit: hour)"
  ) +
  # おおまかなテーマを決める
  # いろいろなテーマがあるよ
  # くわしくは、?theme_classicするとRが教えてくれる
  theme_classic() +
  # より詳細なテーマを決める
  theme(
    axis.text = element_text(size = 12) # 軸テキストサイズ指定
  )
# 
# 
# 層別して並べる折れ線グラフ
working_hours_thailand_02 <- 
  working_hours %>% 
  dplyr::filter(Sex != "Total" & Economic_activities == "Total") %>% 
  ggplot2::ggplot(
    aes(
      x = Time,
      y = Number,
      colour = Status,
      shape = Status
    )
  ) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean weekly working hours per capita",
    subtitle = "(Thailand, 2010-2019, by Labour Force Survey)",
    x = "Year",
    y = "Working hours (Unit: hour)"
  ) +
  scale_x_continuous(
    limits = c(2010, 2019),
    breaks = seq(2010, 2019, 1)
  ) + 
  scale_color_okabeito() + 
  # グラフを性別にならべる
  facet_wrap(~ Sex) +
  theme_classic() +
  # 見た目を詳細に設定する
  theme(
    axis.text = element_text(
      size = 10
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(size = 10)
  )
#
##
### --- END --- ###
