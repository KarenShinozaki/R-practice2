getwd()
setwd("/Users/karen/Desktop/計量政治分析")
hyou <- read.csv("keiryo2.csv")
hokkaido<- subset(hyou,subset=(pref == "北海道"))
tohoku<- subset(hyou,subset=c(pref == "青森県"|pref == "秋田県"|pref =="岩手県"|pref =="山形県"|pref =="宮城県"|pref =="福島県"))
kanto <- subset(hyou,subset=c(pref == "茨城県"|pref == "栃木県"|pref =="群馬県"|pref =="埼玉県"|pref =="千葉県"|pref =="神奈川県"|pref =="東京都"))
tokyo<- subset(hyou,subset=(pref == "東京都"))
chubu <- subset(hyou,subset=c(pref == "新潟県"|pref == "富山県"|pref =="石川県"|pref =="福井県"|pref =="山梨県"|pref =="静岡県"|pref =="長野県"|pref=="愛知県"|pref=="岐阜県"))
kinki <- subset(hyou,subset=c(pref == "滋賀県"|pref == "三重県"|pref =="京都府"|pref =="大阪府"|pref =="奈良県"|pref =="和歌山県"|pref =="兵庫県"))
chugoku <- subset(hyou,subset=c(pref == "鳥取県"|pref =="岡山県"|pref =="広島県"|pref =="島根県"|pref =="山口県"))
shikoku <- subset(hyou,subset=c(pref == "香川県"|pref =="徳島県"|pref =="愛媛県"|pref =="高知県"))
kyushu <- subset(hyou,subset=c(pref == "福岡県"|pref =="佐賀県"|pref =="大分県"|pref =="長崎県"|pref =="熊本県"|pref =="宮崎県"|pref =="鹿児島県"|pref =="沖縄県"))

1.
tohoku.wage <- tapply(tohoku$mini.wage, tohoku$year, mean)
kanto.wage <- tapply(kanto$mini.wage, kanto$year, mean)
tokyo.wage <- tapply(tokyo$mini.wage, tokyo$year, mean)
chubu.wage <- tapply(chubu$mini.wage, chubu$year, mean)
hokkaido.wage <- tapply(hokkaido$mini.wage, hokkaido$year, mean)
kinki.wage <- tapply(kinki$mini.wage, kinki$year, mean)
chugoku.wage <- tapply(chugoku$mini.wage, chugoku$year, mean)
shikoku.wage <- tapply(shikoku$mini.wage, shikoku$year, mean)
kyushu.wage <- tapply(kyushu$mini.wage, kyushu$year, mean)
pref.wage <- cbind(tohoku.wage,kanto.wage,tokyo.wage,chubu.wage,hokkaido.wage,kinki.wage,chugoku.wage,shikoku.wage,kyushu.wage)
year <- unique(hyou$year)
matplot(year,pref.wage,type="l",col = c("red","pink","gray","green","yellow","orange","blue","purple","black"),xlab="years",ylab="minimum wage",xaxp=c(1997,2018,21))
legend("topleft", legend=c("tohoku","kanto", "tokyo" ,"chubu","hokkaido","kinki","chugoku","shikoku","kyushu"),col=c("red","pink","gray","green","yellow","orange","blue","purple","black"), pch=c(15,0), lwd=2, lty=1)
abline(v = 2008)



2.#失業率を出す

kanto.rate<- tapply(kanto$unemployed, kanto$year, mean) / tapply(kanto$force.pop, kanto$year, mean) *100
kyushu.rate<- tapply(kyushu$unemployed, kyushu$year, mean) / tapply(kyushu$force.pop, kyushu$year, mean) *100
chubu.rate<- tapply(chubu$unemployed, chubu$year, mean) / tapply(chubu$force.pop, chubu$year, mean) *100
un.rate <- cbind(kanto.rate,kyushu.rate,chubu.rate)
un.rate
matplot(year,un.rate,type = "l",col=c("royalblue3","red","green"),xlab="years",ylab="unemployment-rate",xlim=c(1997,2018),xaxp=c(1997,2018,21))
legend("topright", legend=c("kanto", "kyushu","chubu"),col=c("royalblue3","red","green"), pch=c(15,0), lwd=2, lty=1)

3.#失業率と最低賃金の相関性
matplot(un.wage,un.rate,col = c("royalblue3","red","green"),type="l",xlab ="minimum-wage" ,ylab ="unemployed-rate" )
legend("topright", legend=c("kanto","kyushu", "chubu"),col = c("royalblue3","red","green"),pch=c(15,0))
un.wage <- cbind(kanto.wage,kyushu.wage,chubu.wage)
matplot(un.rate,un.wage,col = c("royalblue3","red","green"),type="l",xlab = "unemployed-rate",ylab ="minimum-wage")
legend("topright", legend=c("kanto","kyushu", "chubu"),col = c("royalblue3","red","green"),pch=c(15,0))
