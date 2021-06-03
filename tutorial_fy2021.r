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


# ---- stat.working.hours ----
# 目的
# 1.労働時間年次/月次推移を表現する統計モデリング開発する。
# 2.特に、生産システム伸長と労働組合衰退とが与える影響を加味する
# 3.同じ現象が海外においても観察されるか検討する。
# 
# Rを使ってできるようになりたいテクニック
# 1.記述統計量算出
# 2.日本語が混在するグラフ作図
# 3.統計的仮説検定 
# 4.回帰分析 
# 5.初歩的な時系列モデリング（ARとARIMA） 
# データを眺める限り、なんとかなりそうな気がする。
# 
# 手順
# 1. 日本における労働時間に関するデータをあつめる
# 2. 作図する
# 3. 記述統計量を検討する
# 4. モデルをつくる
# 5. 海外データを集める
# 6. 2.-4.と同じ手順を実行する
# 7. 結果を味わう
# 8. 長崎大学経済学部懸賞論文に投稿する
# 
# 役立ちそうな情報源
# 中澤先生
# http://minato.sip21c.org/statlib/stat-all-r9.pdf
# Rを使った基本的な分析について。
# 上記はだいたいカバーしている。ただし解説はそこまで親切ではない。
# 
# 小波先生
# http://konamih.sakura.ne.jp/Stats/Text/Statistics.pdf
# 統計学に関する簡潔明瞭な解説。学部共通科目教科書と併読するとよい。
# 
# Logistics of Blue
# https://logics-of-blue.com/tag/%E6%99%82%E7%B3%BB%E5%88%97%E5%88%86%E6%9E%90/page/3/
# 水産資源学が専門。時系列モデリングに関する解説が網羅的ですばらしい。
# Rコードつき。
# 実は状態空間モデルから勉強してもいいんじゃないかという気がする。
# 
# 
# 1. 日本における労働時間に関するデータをあつめる
# あらかじめ総務省統計局ホームページにて、apiを使うを取得する。
# https://www.e-stat.go.jp/api/api-info/api-guide
# 
# API：Application Programming Interface
# いろいろな機能を提供するサーバを制御するための暗証番号みたいなもの
# APIをコードに直書きすると、漏洩したときに大変なことになる。なので、別ファイルに書いて、読み込むようにする
# 
# APIのIDを書いたRファイルを読み込む
# Rファイルは別に自分でつくる
# File -> New file -> R scriptでつくれる。拡張子は.r
appId <- source("appId.r")
# 
# 総務省estatからAPIを用いてデータ読み込む
# estatapiパッケージを使う。
# install.packages()を使ってインストールする。1回でいい。
# 使うたびにlibrary()を使って読み込む。
# 詳細は
# https://yutannihilation.github.io/estatapi/　を参照。
# 
# まずデータリストをつくる
wh_data_list <- 
  estatapi::estat_getStatsList(
    # APIのIDを読み込む
    # appId$valueは、
    # appIdというオブジェクトのvalueという変数
    # を意味する。
    # $の使い方は、Rでデータを扱うにはとっても重要。なれておくように。
    appId = appId$value,
    searchWord = "労働時間"
  )%>% 
  # 長期時系列データのみを取り出す。
  # 時間が経過するにつれてどう変化するか知りたいのですから。
  dplyr::filter(
    # stringrパッケージは、文字列処理に便利。
    # tidyverseに含まれるから、あらためてインストールする必要はない。
    # stringr::str_detect()は、条件に合う文字列を含むデータを取り出す。
    # ^や*の使い方、その他詳細はチートシート参照。
    # help -> cheetsheetsでstringrに関するチートシートをみられる。
    stringr::str_detect(
      STATISTICS_NAME, 
      "^毎月勤労統計調査　全国調査 長期時系列表" 
    )
  ) %>% 
  dplyr::filter(
    stringr::str_detect(
      TITLE, 
      "^実数・指数累積データ 所定外労働時間　季節実数・指数累積データ 実数関連・月次"
    )
  )
# リストに含まれる必要なデータだけを抽出する
# とりあえず労働時間に関する系列のみ
wh_data_table <- 
  estatapi::estat_getStatsData(
    appId = appId$value,
    # リストにある使えそうなデータを選ぶ。
    # ここでは月次労働時間を含むデータを取り出す。
    # wh_data_list[2,1]$`@id`の意味は、以下に示すとおり。
    # wh_data_listはリストを収納したオブジェクト名
    # [2,1]はオブジェクトの2行一列目
    # $は列を選ぶときに使う記号
    # `@id`は列名。
    statsDataId = wh_data_list[2,1]$`@id`
  ) %>% 
  dplyr::filter(表章項目 %in% c(
    "一人平均月間所定内労働時間数",
    "一人平均月間所定外労働時間数",
    "一人平均月間総実労働時間数"
  )
  )
# 
# ---
# おまけ　その1
# MSExcelなどでデータを一覧したい場合、保存すればいい。
# csv形式が手っ取り早い
# write_excel_csv()で保存しないと文字化けする
# 参考
# https://qiita.com/nozma/items/6de3e636d175cc3d6e3f
readr::write_excel_csv(
  x = wh_data_table, 
  file = "wh_data_table.csv"
)
# おまけ　その2
# どこに何が書いてあるかは、列名を使って参照する。
# 使う関数は以下に示す通り。
# base::levels() カッコ内にある因子型変数について、水準を示す
# base::factor() カッコ内にある変数を因子型に変換する
# カッコ内にある変数を因子型に変換する
# df$col01 とあるデータフレームdfから、col01という名前を持つ列だけを表示する
# 列名は取得したデータを表示させるか、MSExcelに保存してコピペするか、
# colnames()で取得する
levels(
  factor(
    wh_data_table$表章項目
  )
)
levels(factor(wh_data_table$就業形態))
levels(factor(wh_data_table$事業所規模))
levels(factor(wh_data_table$`産業分類(200711改定)`))
levels(factor(wh_data_table$年月２))
# ---
# 
# データを取得する
wh_data_table_01 <- 
  # データをサーバか読み込む
  # 読み込む前に、データを収納したオブジェクトを実行、変数名や構造を
  # 確認するとよい。
  wh_data_table %>% 
  # つかうデータだけを選ぶ
  # コードでもいいけれど、後で作図するときに使えることがあるので、
  # ひと目見てわかる名前がデータとして含まれる変数を取り出したほうがいい。
  dplyr::select(
    "表章項目", 
    "就業形態", 
    "事業所規模", 
    "産業分類(200711改定)", 
    "年月２", 
    "value"
  ) %>% 
  # 変数名を変える
  # 変数名はアルファベットにしたほうが扱いやすい。
  # 使うパッケージは、data.table()
  # これもtidyverseに含まれるますから
  data.table::setnames(
    c(
      "trait",
      "status",
      "size",
      "industry",
      "year.month",
      "hours"
    )
  ) %>% 
  # データ型を変える
  # 論理値や整数型、文字列、因子、その他色々。
  # あとの分析でデータ型が物を言うので、適切に変換する。
  # tidyverseが取り扱うデータ型は、以下を参照
  # https://tibble.tidyverse.org/articles/types.html
  dplyr::mutate_if(
    is.character,# 文字列型ならば
    .funs = factor# 因子型に置き換える
  ) %>% 
  # 年月データを年月日にして年月日データ型に変換する。
  # まず、base::paste0()を用いて、
  # 年月しか書いてないデータに日付を表現する文字列を加える。
  # 何日でもよいが、ここでは便宜的に1日にした。すなわち、
  # 1960/01（1960年1月） -> 1960/01/01（1960年1月1日）にする
  # つぎに、lubridate::ymd()を用いて、
  # 日付っぽく見える文字列を年月日データ型に変換する。
  dplyr::mutate(
    year.month = lubridate::ymd(
      base::paste0(
        year.month, 
        "/01"
      )
    )
  ) %>% 
  # 条件に合うデータのみを取り出す
  # データ読み込みはここでおわり
  dplyr::filter(
    status == "就業形態計" &
      size %in% c(
        "100～499人",
        "30～99人",
        "500人以上"
      )
  ) %>% 
  # 因子型の順番を指定する
  # 企業規模にあわせてあとから作るグラフが並ぶようにする。
  # ここで処理しないと、Rは企業規模先頭にある数値から順番を評価する。
  # 結果、100名~499名を30~99名よりも先に表示、順番がおかしくなる。
  # 詳細は以下を参照。
  # https://www.jaysong.net/RBook/factor.html
  dplyr::mutate(
    size = factor(
      size,
      levels = c(
        "30～99人",
        "100～499人",
        "500人以上"
      )
    )
  )
# ダウンロードしたデータを保存する
# 毎度毎度保存すると大変なので、保存。
saveRDS(wh_data_table_01, "wh_data_table_01.rds")
# 保存したデータを読み込む
wh_data_table_01 <- readRDS("wh_data_table_01.rds")
#
# 2. 作図する
# 折れ線グラフ作図
# 時系列といえば折れ線グラフ。だから折れ線グラフを作図する
# グラフオブジェクトをつくる
wh_data_line_01 <- 
  # データを読み込む
  wh_data_table_01 %>%
  # グループ毎にわけてネストする
  # cheetsheetの"List manipulation with purrr"をみながらやるといいですよ
  group_by(industry) %>% 
  nest() %>%
  # グループ毎に折れ線グラフを作図、figureと名付けた新たな変数に収納する
  # purrr::map()は、繰り返し作業にきわめて便利な関数。使い方を身につけるといい
  dplyr::mutate(
    figure = purrr::map(
      # nestすると、特になにも変数名を指定しなくても
      # dataと名付けられたリストが作成される
      # 内部には、nestにつかった因子以外のデータが収納されている
      data,
      ~
        # グラフエリアを宣言する
        ggplot2::ggplot(
          data = .,#使うデータは、dataです
          aes(
            x = year.month, 
            y = hours, 
            colour = trait#色は区分ごとに変える
          )
        ) + 
        # 折れ線グラフをつくると宣言する
        geom_line() +
        labs(
          title = industry,
          x = "年次",
          y = "労働時間（単位：時間）",
          colour = "区分"
        ) +
        # カラーユニバーサルデザインを意識した配色にする
        scale_colour_okabeito() +
        # 複数のグラフをタイル状にならべる
        facet_wrap(
          ~ size, 
          nrow = 2,
          ncol = 2,
          scale = "free"
        ) +
        # いつものテーマ
        theme_classic() +
        # 日本語をRでつくるグラフに埋め込むにはちょっとコツがいる
        # フォントを指定してあげないと、RStudio上で表示されるかもしれないが
        # pdf形式やpng形式にて保存したときに文字化けする。
        # ここに書いてあるフォントファミリーは、諸君が使うRStudioだとないかも
        # しれない。なので、ちょっと工夫が必要。
        # 詳細は以下を参照
        # https://ill-identified.hatenablog.com/entry/2020/10/03/200618
        theme(
          text = element_text(family =  "Noto Serif CJK JP", face = "plain"),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          strip.background = element_blank()
        )
    )
  )
# 保存する
# 複数ページあるpdfファイルにグラフを保存するおまじない
grDevices::cairo_pdf(
  filename = "wh_data_line_01.pdf",# ファイル名
  family =  "Noto Serif CJK JP",# フォントファミリー
  onefile = TRUE # 複数あるグラフオブジェクトを単一pdfファイル内にまとめる
)
# グラフオブジェクトを実行
wh_data_line_01$figure
# おまじないはおしまい
dev.off()

# 
##
### --- END --- ###

