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
# install.packages("viridis")
# install.packages("ggtext")
# install.packages("GGally")

# ---- read.library ----
# tidyverseパッケージを読み込む
# 起動するたびにする。
library(tidyverse)
library(khroma)
library(viridis)
library(ggtext)
library(GGally)

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
# 
# 課題用
working_hours_thailand_03 <- 
  working_hours %>% 
  dplyr::filter(Sex != "Total" & Economic_activities != "Not classified") %>% 
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
  scale_x_continuous(
    limits = c(2010, 2019),
    breaks = seq(2010, 2019, 2)
  ) + 
  scale_color_smoothrainbow(
    discrete = TRUE
  ) + 
  labs(
    title = "Mean weekly working hours per capita by Economic activity and gender",
    subtitle = "(Thailand, 2010-2019, by Labour Force Survey)",
    x = "Year",
    y = "Working hours (Unit: hour)"
  ) +
  facet_grid(Sex ~ Economic_activities) +
  theme_classic() +
  theme(
    axis.text = element_text(
      size = 10
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  )

# save the figure
ggsave(
  filename = "working_hours_thailand_03.pdf",
  plot = working_hours_thailand_03
  )
#
##
### --- END --- ###


# ---- density.plot.and.scatter.plot ----
# 密度プロットを描画する
# 基本の描き方
iris_density_01 <- 
  iris %>% 
  ggplot2::ggplot(
    aes(
      x = Sepal.Length,
      fill = Species,
      color = Species
    )
  ) +
  # 密度プロットを描く
  geom_density(alpha =0.5)
# 基本的な散布図描き方
# データはおなじみiris
iris_scatter_01 <- 
  iris %>% 
  ggplot2::ggplot(
    aes(
      x = Sepal.Length,
      y = Sepal.Width
    )
  ) + 
  geom_point()
# 散布図その2
iris_scatter_02 <- 
  iris %>% 
  ggplot2::ggplot(
    aes(
      x = Sepal.Length,
      y = Sepal.Width,
      colour = Species
    )
  ) + 
  # 点を描くにはgeom_point()
  # くわしい設定は?geom_point()をみてね
  geom_point() + 
  # これもおなじみ岡部・伊藤のカラーユニバーサルデザインを使ったカラーパレット
  scale_color_okabeito() +
  theme_classic() +
  theme(
    legend.position = "bottom",　　　　　　# 凡例はx軸下に
    legend.text = element_text(size =12),
    axis.text = element_text(size =12)
  )
#
##
### --- END --- ###
#
#
# ---- density.scatter.assignment ----
# 模範解答だよーん
# ここから先は課題です。
# 課題は2つあります。
# 品種毎に点を塗り分ける
# やっぱりデータはiris
# 実データもいいけれど、これで練習を積めるならばそれはそれで。
# 密度プロットを修飾する
iris_density_02 <- 
  iris %>% 
  pivot_longer(
    cols = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
    names_to = "Traits",
    values_to = "size"
  ) %>% 
  ggplot2::ggplot(
    aes(
      x = size,
      fill = Species,
      colour = Species
    )
  ) +
  geom_density(
    alpha = 0.5
  ) +
  labs(
    title = "Density plot of size by trait", 
    caption = "Source: Edgar Anderson (1936). The species problem in Iris. *Annals of the Missouri Botanical Garden*. 23 (3): 457–509. ",
    x = "Size (Unit:cm)", 
    y = "Density"
  ) + 
  facet_wrap(
    facets = ~ Traits,
    scales = "free"
  ) +
  scale_fill_muted(reverse = TRUE) + 
  scale_colour_muted(reverse = TRUE) + 
  theme_classic() + 
  theme(
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    plot.caption = ggtext::element_markdown(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  )
# save the figure above
ggsave("iris_density_02.png", plot = iris_density_02)
# 
# 
# 散布図行列
# 多変量データにおいて、１枚ずつ散布図やら密度プロットを描くことは面倒極まりない。
# 一度に書いてしまおうというggplotのラッパー関数だよーん。
# 課題はこういう感じで描きますよ。
iris_pairs_01 <- 
  iris %>% 
  # GGally::ggpairs()とは：
  # 散布図行列と相関行列と密度プロットをいっぺんに作画する関数
  GGally::ggpairs(
    aes(
      color = Species,
      fill = Species,
      alpha = 0.5　　　# 透明度。0.0で完全に透明。1.0で完全に不透明
    ),
    # プロットするのは下記4変数のみ。
    # Speciesは色や塗りつぶしに使うだけ。
    columns = c(
      "Sepal.Length", 
      "Sepal.Width", 
      "Petal.Length", 
      "Petal.Width"
      ),
    # 右上に埋めるものを指定
    # 連続量同士で散布図を作ってね、と指定
    # 対角線にはデフォルトで要因別密度プロット指定済
    # 詳細は下記ページ参照
    # https://k-metrics.netlify.app/post/2018-09/modern_pairplot/
    upper=list(continuous="points"),
    # 左下に埋めるものを指定
    # 連続量
    lower=list(continuous="cor")
        ) +
  # ラベル設定
  # サブタイトルに貴君氏名を書き込んでね
  labs(
    title = "Pair plot by traits and species of iris data",
    subtitle = "Drawn by your name"
    ) +
  # khromaパッケージにあるカラーパレットだよん。
  # 視認性は最高。
  # くすぐりに、カラーパレット順番を入れ替えた
  scale_color_contrast(reverse = TRUE) +
  scale_fill_contrast(reverse = TRUE) +
  # このあたりはいつものやつ。
  # 設定用共通オブジェクトをつくってもいいんじゃない
  theme_classic() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(size = 10)
  )
# save the figure above
ggsave("iris_pairs_01.png", plot = iris_pairs_01)
# 
##
### --- END --- ###
