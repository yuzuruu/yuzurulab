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



# 宿題
# https://kazutan.github.io/fukuokaR11/intro_ggplot2.html
# を眺めて、いろいろ設定をいじって挙動を確認する。
# 結果をggplot2::ggsave()関数を使ってpng形式にて保存、Google chat上に投稿。
# メインタイトルに自分の名前をローマ字で記入すること。