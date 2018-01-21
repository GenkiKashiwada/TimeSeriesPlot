#柿岡観測所の2012年 3月 5日
#X座標データをフーリエ変換
#30分ごとに分割して横に並べてスペクトログラムを作成
#x軸に時間
#y軸に周波数
#intencityを色で分ける
################################
##30min間隔の時間を1~48で選択
No = 10
################################
##表示する周波数幅の調整
Bottom_Freq =25
Top_Freq    =100
Indicate_Freq = Bottom_Freq:Top_Freq
################################

library(ggplot2)
library(gridExtra)

#データの取り込み
dataKAK <- read.table("/home/saitaken/tokken/DATA/Download_Data/Geomagnetism/Kakioka/sec/kak20120305-20120311dsec.sec/kak20120305dsec.sec",header=T, skip=19)
alltime <- (1:86400)

#画像データの定義
png ("20120305柿岡_スペクトログラム_No07.png", width = 2000, height = 1200, pointsize = 16);
#png ("20120305柿岡_波形とフーリエ.png", width = 3000, height = 1800, pointsize = 16);


#********フーリエ変換*********
Time_interval = 1800              #時間間隔 秒数で設定する
SAMPLING_FREQ = Time_interval      #サンプリング周波数
#nof_SampleData = 1:SAMPLING_FREQ        #データ数分の配列
nof_SampleData = (1+1800*(No-1)):(1800*No)
#tt = nn / SAMPLING_FREQ     #時間
#ff = nn * (length(tt) / SAMPLING_FREQ )   #周波数


#オブジェクトの初期化
fft_data <- data.frame(initialize <- Indicate_Freq)

#変換の実行      そのままだとcomplexを返すためabsで絶対値を取る
fft_data$KAKX <- abs( fft( dataKAK$KAKX[nof_SampleData] - mean(dataKAK$KAKX[nof_SampleData])))[Indicate_Freq]
fft_data$KAKY <- abs( fft( dataKAK$KAKY[nof_SampleData] - mean(dataKAK$KAKY[nof_SampleData])))[Indicate_Freq]
fft_data$KAKZ <- abs( fft( dataKAK$KAKZ[nof_SampleData] - mean(dataKAK$KAKZ[nof_SampleData])))[Indicate_Freq]
fft_data$KAKF <- abs( fft( dataKAK$KAKF[nof_SampleData] - mean(dataKAK$KAKF[nof_SampleData])))[Indicate_Freq]
fft_data$frequency <- Indicate_Freq
fft_data$Xaxis  <- Indicate_Freq
for ( i in 1:(Top_Freq - Bottom_Freq +1)){
    fft_data$Xaxis[i] = No
}


#スペクトルのプロット
#fig_x   <- ggplot( fft_data, aes( x=Xaxis, y=frequency, colour=KAKX)) + geom_tile() + scale_y_log10() #+ scale_colour_log10()
fig_fft <- ggplot( fft_data, aes(x = frequency, y = KAKX )) + geom_line( color = "red")  #+ scale_y_log10()  #+ scale_x_log10()
#fft_data$KAKX = log10(fft_data$KAKX)
#fig_x   <- ggplot( fft_data, aes( x=Xaxis, y=frequency, colour=KAKX)) + geom_tile() #+ scale_y_log10()  #+ scale_color_gradientn(colours=c('springgreen4','yellow','red'))

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
fig_x   <- ggplot( fft_data, aes( x=Xaxis, y=frequency )) + geom_tile(aes(fill = KAKX )) + scale_fill_gradientn(colours=jet.colors(7))#, legend_param=list(colorbar=T, colorbar_nbin=100))
# レイアウトを行列にし、layout1 に保存しておく
layout1 <- rbind(c(1,2))
# まとめて1枚に出力
print(grid.arrange(fig_x, fig_fft,
                   layout_matrix = layout1))


#デバイスドライバのクローズ
graphics.off();
