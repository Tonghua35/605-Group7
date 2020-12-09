##########################################################################################
# Process data that has been cleaned                                                     #
##########################################################################################

aa<-read.csv("clean_data.csv",header = T)

aa$VCF0104[which(aa$VCF0104==0)]<-NA
aa$VCF0106[which(aa$VCF0106==9)]<-NA
aa$VCF0108[which(aa$VCF0108==9|aa$VCF0108==8)]<-NA
aa$VCF0110[which(aa$VCF0110==0)]<-NA
aa$VCF0114[which(aa$VCF0114==0)]<-NA
aa$VCF0118[which(aa$VCF0118==9)]<-NA
aa$VCF0127[which(aa$VCF0127==0)]<-NA
aa$VCF0128[which(aa$VCF0128==0)]<-NA
aa$VCF0130a[which(aa$VCF0130a==9|aa$VCF0130a==8)]<-NA
aa$VCF0140[which(aa$VCF0140==9|aa$VCF0140==8)]<-NA
aa$VCF0143[which(aa$VCF0143==9|aa$VCF0143==8)]<-NA
aa$VCF0146[which(aa$VCF0146==9|aa$VCF0146==8)]<-NA
aa$VCF0147[which(aa$VCF0147==9)]<-NA
aa$VCF0148a[which(aa$VCF0148a==9)]<-NA
aa$VCF0149[which(aa$VCF0149==9)]<-NA
aa$VCF0156[which(aa$VCF0156==9|aa$VCF0156==8)]<-NA
aa$VCF0157[which(aa$VCF0157==9|aa$VCF0157==8)]<-NA
aa$VCF0209[which(aa$VCF0209==99|aa$VCF0209==98)]<-NA
aa$VCF0210[which(aa$VCF0210==99|aa$VCF0210==98)]<-NA
aa$VCF0211[which(aa$VCF0211==99|aa$VCF0211==98)]<-NA
aa$VCF0212[which(aa$VCF0212==99|aa$VCF0212==98)]<-NA
aa$VCF0218[which(aa$VCF0218==99|aa$VCF0218==98)]<-NA
aa$VCF0223[which(aa$VCF0223==99|aa$VCF0223==98)]<-NA
aa$VCF0224[which(aa$VCF0224==99|aa$VCF0224==98)]<-NA
aa$VCF0227[which(aa$VCF0227==99|aa$VCF0227==98)]<-NA
aa$VCF0228[which(aa$VCF0228==99|aa$VCF0228==98)]<-NA
aa$VCF0232[which(aa$VCF0232==99|aa$VCF0232==98)]<-NA
aa$VCF0233[which(aa$VCF0233==99|aa$VCF0233==98)]<-NA
aa$VCF0253[which(aa$VCF0253==99|aa$VCF0253==98)]<-NA
aa$VCF0290[which(aa$VCF0290==99|aa$VCF0290==98)]<-NA
aa$VCF0291[which(aa$VCF0291==999|aa$VCF0291==998)]<-NA
aa$VCF0303[which(aa$VCF0303==0)]<-NA
aa$VCF0305[which(aa$VCF0305==0)]<-NA
aa$VCF0310[which(aa$VCF0310==9|aa$VCF0310==0)]<-NA
aa$VCF0346[which(aa$VCF0346==9|aa$VCF0346==0|aa$VCF0346==8)]<-NA
aa$VCF0349[which(aa$VCF0349==9|aa$VCF0349==0|aa$VCF0349==8)]<-NA
aa$VCF0358[which(aa$VCF0358==9|aa$VCF0358==8)]<-NA
aa$VCF0359[which(aa$VCF0359==9|aa$VCF0359==8)]<-NA
aa$VCF0360[which(aa$VCF0360==9|aa$VCF0360==8)]<-NA
aa$VCF0361[which(aa$VCF0361==9|aa$VCF0361==8)]<-NA
aa$VCF0370[which(aa$VCF0370==9|aa$VCF0370==8)]<-NA
aa$VCF0371[which(aa$VCF0371==9|aa$VCF0371==8)]<-NA
aa$VCF0372[which(aa$VCF0372==9|aa$VCF0372==8)]<-NA
aa$VCF0373[which(aa$VCF0373==9|aa$VCF0373==8)]<-NA
aa$VCF0374[which(aa$VCF0374==9|aa$VCF0374==8)]<-NA
aa$VCF0380[which(aa$VCF0380==9|aa$VCF0380==8)]<-NA
aa$VCF0386[which(aa$VCF0386==9|aa$VCF0386==8)]<-NA
aa$VCF0392[which(aa$VCF0392==9|aa$VCF0392==8)]<-NA
aa$VCF0412[which(aa$VCF0412==99|aa$VCF0412==98)]<-NA
aa$VCF0413[which(aa$VCF0413==999|aa$VCF0413==998)]<-NA
aa$VCF0414[which(aa$VCF0414==99|aa$VCF0414==98)]<-NA
aa$VCF0415[which(aa$VCF0415==999|aa$VCF0415==998)]<-NA
aa$VCF0424[which(aa$VCF0424==99|aa$VCF0424==98)]<-NA
aa$VCF0425[which(aa$VCF0425==99|aa$VCF0425==98)]<-NA
aa$VCF0426[which(aa$VCF0426==99|aa$VCF0426==98)]<-NA
aa$VCF0427[which(aa$VCF0427==99|aa$VCF0427==98)]<-NA
aa$VCF0428[which(aa$VCF0428==99|aa$VCF0428==98)]<-NA
aa$VCF0450[which(aa$VCF0450==0|aa$VCF0450==8)]<-NA
aa$VCF0475[which(aa$VCF0475==9|aa$VCF0475==8)]<-NA
aa$VCF0481[which(aa$VCF0481==9|aa$VCF0481==8)]<-NA
aa$VCF0487[which(aa$VCF0487==9|aa$VCF0487==8)]<-NA
aa$VCF0493[which(aa$VCF0493==9|aa$VCF0493==8)]<-NA
aa$VCF0501[which(aa$VCF0501==9|aa$VCF0501==0)]<-NA
aa$VCF0502[which(aa$VCF0502==9|aa$VCF0502==0)]<-NA
aa$VCF0503[which(aa$VCF0503==8|aa$VCF0503==0)]<-NA
aa$VCF0504[which(aa$VCF0504==8|aa$VCF0504==0)]<-NA
aa$VCF0605[which(aa$VCF0605==9|aa$VCF0605==0)]<-NA
aa$VCF0606[which(aa$VCF0606==9|aa$VCF0606==0)]<-NA
aa$VCF0609[which(aa$VCF0609==9|aa$VCF0609==0)]<-NA
aa$VCF0613[which(aa$VCF0613==9|aa$VCF0613==0)]<-NA
aa$VCF0624[which(aa$VCF0624==9|aa$VCF0624==0)]<-NA
aa$VCF0648[which(aa$VCF0648==999)]<-NA
aa$VCF0649[which(aa$VCF0649==999)]<-NA
aa$VCF0656[which(aa$VCF0656==999)]<-NA
aa$VCF0700[which(aa$VCF0700==9|aa$VCF0700==8)]<-NA
aa$VCF0702[which(aa$VCF0702==0)]<-NA
aa$VCF0703[which(aa$VCF0703==0)]<-NA
aa$VCF0704[which(aa$VCF0704==0)]<-NA
aa$VCF0705[which(aa$VCF0705==0)]<-NA
aa$VCF0707[which(aa$VCF0707==0)]<-NA
aa$VCF0708[which(aa$VCF0708==0)]<-NA
aa$VCF0709[which(aa$VCF0709==0)]<-NA
aa$VCF0710[which(aa$VCF0710==0)]<-NA
aa$VCF0712[which(aa$VCF0712==0)]<-NA
aa$VCF0713[which(aa$VCF0713==0|aa$VCF0713==9)]<-NA
aa$VCF0714[which(aa$VCF0714==0)]<-NA
aa$VCF0717[which(aa$VCF0717==0)]<-NA
aa$VCF0718[which(aa$VCF0718==0)]<-NA
aa$VCF0719[which(aa$VCF0719==0)]<-NA
aa$VCF0720[which(aa$VCF0720==0)]<-NA
aa$VCF0721[which(aa$VCF0721==0)]<-NA
aa$VCF0723a[which(aa$VCF0723a==0)]<-NA
aa$VCF0729[which(aa$VCF0729==0)]<-NA
aa$VCF0731[which(aa$VCF0731==0|aa$VCF0731==8|aa$VCF0731==9)]<-NA
aa$VCF0733[which(aa$VCF0733==9)]<-NA
aa$VCF0734[which(aa$VCF0734==0)]<-NA
aa$VCF0736[which(aa$VCF0736==0|aa$VCF0736==7)]<-NA
aa$VCF0740[which(aa$VCF0740==0)]<-NA
aa$VCF0741[which(aa$VCF0741==0)]<-NA
aa$VCF0742[which(aa$VCF0742==0)]<-NA
aa$VCF0801[which(aa$VCF0801==99|aa$VCF0801==98)]<-NA
aa$VCF0804[which(aa$VCF0804==0|aa$VCF0804==9)]<-NA
aa$VCF0806[which(aa$VCF0806==0|aa$VCF0806==9)]<-NA
aa$VCF0809[which(aa$VCF0809==0|aa$VCF0809==9)]<-NA
aa$VCF0823[which(aa$VCF0823==0|aa$VCF0823==9)]<-NA
aa$VCF0830[which(aa$VCF0830==0|aa$VCF0830==9)]<-NA
aa$VCF0838[which(aa$VCF0838==0|aa$VCF0838==9)]<-NA
aa$VCF0839[which(aa$VCF0839==0|aa$VCF0839==9)]<-NA
aa$VCF0843[which(aa$VCF0843==0|aa$VCF0843==9)]<-NA
aa$VCF0846[which(aa$VCF0846==0|aa$VCF0846==8)]<-NA
aa$VCF0847[which(aa$VCF0847==0|aa$VCF0847==8)]<-NA
aa$VCF0849[which(aa$VCF0849==0|aa$VCF0849==9)]<-NA
aa$VCF0850[which(aa$VCF0850==0|aa$VCF0850==9)]<-NA
aa$VCF0851[which(aa$VCF0851==9|aa$VCF0851==8)]<-NA
aa$VCF0852[which(aa$VCF0852==9|aa$VCF0852==8)]<-NA
aa$VCF0853[which(aa$VCF0853==9|aa$VCF0853==8)]<-NA
aa$VCF0854[which(aa$VCF0854==9|aa$VCF0854==8)]<-NA
aa$VCF0867[which(aa$VCF0867==9|aa$VCF0867==8)]<-NA
aa$VCF0870[which(aa$VCF0870==0|aa$VCF0870==8)]<-NA
aa$VCF0871[which(aa$VCF0871==0|aa$VCF0871==8|aa$VCF0871==9)]<-NA
aa$VCF0872[which(aa$VCF0872==8|aa$VCF0872==9)]<-NA
aa$VCF0876[which(aa$VCF0876==8|aa$VCF0876==9)]<-NA
aa$VCF0876[which(aa$VCF0876==8|aa$VCF0876==9)]<-NA
aa$VCF0878[which(aa$VCF0878==8|aa$VCF0878==9)]<-NA
aa$VCF0879a[which(aa$VCF0879a==8|aa$VCF0879a==9)]<-NA
aa$VCF0880[which(aa$VCF0880==0|aa$VCF0880==9)]<-NA
aa$VCF0881[which(aa$VCF0881==0|aa$VCF0881==9)]<-NA
aa$VCF0886[which(aa$VCF0886==8|aa$VCF0886==9)]<-NA
aa$VCF0887[which(aa$VCF0887==8|aa$VCF0887==9)]<-NA
aa$VCF0888[which(aa$VCF0888==8|aa$VCF0888==9)]<-NA
aa$VCF0887[which(aa$VCF0887==8|aa$VCF0887==9)]<-NA
aa$VCF0890[which(aa$VCF0890==8|aa$VCF0890==9)]<-NA
aa$VCF0894[which(aa$VCF0894==8|aa$VCF0894==9)]<-NA
aa$VCF0903[which(aa$VCF0903==9)]<-NA
aa$VCF0904[which(aa$VCF0904==0|aa$VCF0904==9)]<-NA
aa$VCF0905[which(aa$VCF0905==9)]<-NA
aa$VCF0991[which(aa$VCF0991==0|aa$VCF0991==8|aa$VCF0991==9)]<-NA
aa$VCF0992[which(aa$VCF0992==8|aa$VCF0992==9)]<-NA
aa$VCF1004[which(aa$VCF1004==9)]<-NA
aa$VCF1005[which(aa$VCF1005==9)]<-NA
aa$VCF1011[which(aa$VCF1011==9)]<-NA
aa$VCF1012[which(aa$VCF1012==9)]<-NA
aa$VCF1013[which(aa$VCF1013==9)]<-NA
aa$VCF1014[which(aa$VCF1014==9)]<-NA
aa$VCF9005[which(aa$VCF9005==99|aa$VCF9005==98)]<-NA
aa$VCF9009[which(aa$VCF9009==8|aa$VCF9009==9)]<-NA
aa$VCF9013[which(aa$VCF9013==8|aa$VCF9013==9)]<-NA
aa$VCF9016[which(aa$VCF9016==8|aa$VCF9016==9)]<-NA
aa$VCF9017[which(aa$VCF9017==8|aa$VCF9017==9)]<-NA
aa$VCF9018[which(aa$VCF9018==8|aa$VCF9018==9)]<-NA
aa$VCF9021[which(aa$VCF9021==8|aa$VCF9021==9)]<-NA
aa$VCF9022[which(aa$VCF9022==8|aa$VCF9022==9|aa$VCF9022==0)]<-NA
aa$VCF9023[which(aa$VCF9023==7|aa$VCF9023==9|aa$VCF9022==0)]<-NA
aa$VCF9025[which(aa$VCF9025==0|aa$VCF9025==9)]<-NA
aa$VCF9027[which(aa$VCF9027==0|aa$VCF9027==9)]<-NA
aa$VCF9028[which(aa$VCF9028==8|aa$VCF9028==9)]<-NA
aa$VCF9029[which(aa$VCF9029==8|aa$VCF9029==9)]<-NA
aa$VCF9030a[which(aa$VCF9030a==8|aa$VCF9030a==9)]<-NA
aa$VCF9030b[which(aa$VCF9030b==8|aa$VCF9030b==9)]<-NA
aa$VCF9030c[which(aa$VCF9030c==8|aa$VCF9030c==9)]<-NA
aa$VCF9031[which(aa$VCF9031==8|aa$VCF9031==9)]<-NA
aa$VCF9032[which(aa$VCF9032==8|aa$VCF9032==9)]<-NA
aa$VCF9039[which(aa$VCF9039==8|aa$VCF9039==9)]<-NA
aa$VCF9040[which(aa$VCF9040==8|aa$VCF9040==9)]<-NA
aa$VCF9041[which(aa$VCF9041==8|aa$VCF9041==9)]<-NA
aa$VCF9042[which(aa$VCF9042==8|aa$VCF9042==9)]<-NA
aa$VCF9045[which(aa$VCF9045==8|aa$VCF9045==9)]<-NA
aa$VCF9047[which(aa$VCF9047==8|aa$VCF9047==9)]<-NA
aa$VCF9048[which(aa$VCF9048==8|aa$VCF9048==9)]<-NA
aa$VCF9049[which(aa$VCF9049==8|aa$VCF9049==9)]<-NA
aa$VCF9054[which(aa$VCF9054==9)]<-NA
aa$VCF9069[which(aa$VCF9069==0|aa$VCF9069==8|aa$VCF9069==9)]<-NA
aa$VCF9081[which(aa$VCF9081==0|aa$VCF9081==8|aa$VCF9081==9)]<-NA
aa$VCF9084[which(aa$VCF9084==0|aa$VCF9084==8|aa$VCF9084==9)]<-NA
aa$VCF9085[which(aa$VCF9085==0|aa$VCF9085==8|aa$VCF9085==9)]<-NA
aa$VCF9086[which(aa$VCF9086==0|aa$VCF9086==8|aa$VCF9086==9)]<-NA
aa$VCF9087[which(aa$VCF9087==0|aa$VCF9087==8|aa$VCF9087==9)]<-NA
aa$VCF9088[which(aa$VCF9088==0|aa$VCF9088==8|aa$VCF9088==9)]<-NA
aa$VCF9089[which(aa$VCF9089==0|aa$VCF9089==8|aa$VCF9089==9)]<-NA
aa$VCF9092[which(aa$VCF9092==0|aa$VCF9092==8|aa$VCF9092==9)]<-NA
aa$VCF9093[which(aa$VCF9093==0|aa$VCF9093==8|aa$VCF9093==9)]<-NA
aa$VCF9094[which(aa$VCF9094==0|aa$VCF9094==8|aa$VCF9094==9)]<-NA
aa$VCF9095[which(aa$VCF9095==0|aa$VCF9095==8|aa$VCF9095==9)]<-NA
aa$VCF9096[which(aa$VCF9096==0|aa$VCF9096==8|aa$VCF9096==9)]<-NA
aa$VCF9122[which(aa$VCF9112==9)]<-NA
aa$VCF9131[which(aa$VCF9131==8|aa$VCF9131==9)]<-NA
aa$VCF9132[which(aa$VCF9132==8|aa$VCF9132==9)]<-NA
aa$VCF9133[which(aa$VCF9133==8|aa$VCF9133==9)]<-NA
aa$VCF9201[which(aa$VCF9201==-7|aa$VCF9201==-8|aa$VCF9201==-9)]<-NA
aa$VCF9202[which(aa$VCF9202==-7|aa$VCF9202==-8|aa$VCF9202==-9)]<-NA
aa$VCF9203[which(aa$VCF9203==-8|aa$VCF9203==-9)]<-NA
aa$VCF9204[which(aa$VCF9204==-8|aa$VCF9204==-9)]<-NA
aa$VCF9205[which(aa$VCF9205==-8|aa$VCF9205==-9)]<-NA
aa$VCF9206[which(aa$VCF9206==-8|aa$VCF9206==-9)]<-NA
aa$VCF9207[which(aa$VCF9207==-7|aa$VCF9207==-8|aa$VCF9207==-9)]<-NA
aa$VCF9208[which(aa$VCF9208==-7|aa$VCF9208==-8|aa$VCF9208==-9)]<-NA
aa$VCF9209[which(aa$VCF9209==-8|aa$VCF9209==-9)]<-NA
aa$VCF9210[which(aa$VCF9210==-8|aa$VCF9210==-9)]<-NA
aa$VCF9211[which(aa$VCF9211==-8|aa$VCF9211==-9)]<-NA
aa$VCF9212[which(aa$VCF9212==-8|aa$VCF9212==-9)]<-NA
aa$VCF9213[which(aa$VCF9213==-8|aa$VCF9213==-9)]<-NA
aa$VCF9214[which(aa$VCF9214==-8|aa$VCF9214==-9)]<-NA
aa$VCF9215[which(aa$VCF9215==-8|aa$VCF9215==-9)]<-NA
aa$VCF9216[which(aa$VCF9216==-8|aa$VCF9216==-9)]<-NA
aa$VCF9217[which(aa$VCF9217==-8|aa$VCF9217==-9)]<-NA
aa$VCF9218[which(aa$VCF9218==-8|aa$VCF9218==-9)]<-NA
aa$VCF9219[which(aa$VCF9219==-8|aa$VCF9219==-9)]<-NA
aa$VCF9220[which(aa$VCF9220==-8|aa$VCF9220==-9)]<-NA
aa$VCF9221[which(aa$VCF9221==-8|aa$VCF9221==-9)]<-NA
aa$VCF9222[which(aa$VCF9222==-8|aa$VCF9222==-9)]<-NA
aa$VCF9223[which(aa$VCF9223==-8|aa$VCF9223==-9)]<-NA
aa$VCF9224[which(aa$VCF9224==-8|aa$VCF9224==-9)]<-NA
aa$VCF9225[which(aa$VCF9225==-8|aa$VCF9225==-9)]<-NA
aa$VCF9226[which(aa$VCF9226==-8|aa$VCF9226==-9)]<-NA
aa$VCF9227[which(aa$VCF9227==-8|aa$VCF9227==-9)]<-NA
aa$VCF9228[which(aa$VCF9228==-8|aa$VCF9228==-9)]<-NA
aa$VCF9229[which(aa$VCF9229==-8|aa$VCF9229==-9)]<-NA
aa$VCF9230[which(aa$VCF9230==-8|aa$VCF9230==-9)]<-NA
aa$VCF9231[which(aa$VCF9231==-8|aa$VCF9231==-9|aa$VCF9231==-7)]<-NA
aa$VCF9232[which(aa$VCF9232==-8|aa$VCF9232==-9)]<-NA
aa$VCF9233[which(aa$VCF9233==-8|aa$VCF9233==-9)]<-NA
aa$VCF9234[which(aa$VCF9234==-8|aa$VCF9234==-9)]<-NA
aa$VCF9235[which(aa$VCF9235==-8|aa$VCF9235==-9)]<-NA
aa$VCF9236[which(aa$VCF9236==-8|aa$VCF9236==-9)]<-NA
aa$VCF9237[which(aa$VCF9237==-8|aa$VCF9237==-9)]<-NA
aa$VCF9238[which(aa$VCF9238==-8|aa$VCF9238==-9)]<-NA
aa$VCF9239[which(aa$VCF9239==-8|aa$VCF9239==-9)]<-NA
aa$VCF9240[which(aa$VCF9240==-8|aa$VCF9240==-9)]<-NA
aa$VCF9241[which(aa$VCF9241==-8|aa$VCF9241==-9|aa$VCF9241==-7)]<-NA
aa$VCF9242[which(aa$VCF9242==-8|aa$VCF9242==-9|aa$VCF9242==-7)]<-NA
aa$VCF9243[which(aa$VCF9243==-8|aa$VCF9243==-9|aa$VCF9243==-7)]<-NA
aa$VCF9244[which(aa$VCF9244==-8|aa$VCF9244==-9)]<-NA
aa$VCF9245[which(aa$VCF9245==-8|aa$VCF9245==-9)]<-NA
aa$VCF9246[which(aa$VCF9246==-8|aa$VCF9246==-9)]<-NA
aa$VCF9247[which(aa$VCF9247==-8|aa$VCF9247==-9)]<-NA
aa$VCF9248[which(aa$VCF9248==-8|aa$VCF9248==-9)]<-NA
aa$VCF9249[which(aa$VCF9249==-8|aa$VCF9249==-9)]<-NA
aa$VCF9250[which(aa$VCF9250==-8|aa$VCF9250==-9)]<-NA
aa$VCF9251[which(aa$VCF9251==-8|aa$VCF9251==-9)]<-NA
aa$VCF9252[which(aa$VCF9252==-8|aa$VCF9252==-9)]<-NA
aa$VCF9253[which(aa$VCF9253==-8|aa$VCF9253==-9)]<-NA
aa$VCF9254[which(aa$VCF9254==-8|aa$VCF9254==-9)]<-NA
aa$VCF9255[which(aa$VCF9255==-8|aa$VCF9255==-9)]<-NA
aa$VCF9256[which(aa$VCF9256==-8|aa$VCF9256==-9)]<-NA
aa$VCF9257[which(aa$VCF9257==-8|aa$VCF9257==-9)]<-NA
aa$VCF9258[which(aa$VCF9258==-8|aa$VCF9258==-9)]<-NA
aa$VCF9259[which(aa$VCF9259==-8|aa$VCF9259==-9)]<-NA
aa$VCF9260[which(aa$VCF9260==-9)]<-NA
aa$VCF9261[which(aa$VCF9261==-9)]<-NA
aa$VCF9262[which(aa$VCF9262==-9)]<-NA
aa$VCF9263[which(aa$VCF9263==-8|aa$VCF9263==-9)]<-NA
aa$VCF9264[which(aa$VCF9264==-8|aa$VCF9264==-9)]<-NA
aa$VCF9265[which(aa$VCF9265==-8|aa$VCF9265==-9)]<-NA
aa$VCF9266[which(aa$VCF9266==-8|aa$VCF9266==-9)]<-NA
aa$VCF9267[which(aa$VCF9267< -1)]<-NA
aa$VCF9268[which(aa$VCF9268< -1)]<-NA
aa$VCF9269[which(aa$VCF9269< -1)]<-NA
aa$VCF9270[which(aa$VCF9270< -1)]<-NA
aa$VCF9271[which(aa$VCF9271< -1)]<-NA
aa$VCF9272[which(aa$VCF9272< -1)]<-NA
aa$VCF9273[which(aa$VCF9273< -1)]<-NA
aa$VCF9275[which(aa$VCF9275< -1)]<-NA
aa$VCF9271[which(aa$VCF9271< -1)]<-NA
aa$VCF9277[which(aa$VCF9277< -1)]<-NA
aa$VCF9278[which(aa$VCF9278< -1)]<-NA
aa$VCF9279[which(aa$VCF9279< -1)]<-NA
aa$VCF9280[which(aa$VCF9280< -1)]<-NA
aa$VCF9280[which(aa$VCF9280< -1)]<-NA
aa$VCF9281[which(aa$VCF9281< -1)]<-NA
aa$VCF9282[which(aa$VCF9282< -1)]<-NA




del.name=c("VCF0006","VCF0006a","VCF0070b","VCF0071d","VCF0072b","VCF0101","VCF0103","VCF0105a","VCF0105b",
           "VCF0107","VCF0116","VCF0127a","VCF0127b","VCF0130","VCF0140a","VCF0148","VCF0150","VCF0152",
           "VCF0205","VCF0206","VCF0207","VCF0207","VCF0214","VCF0217","VCF0234","VCF0301","VCF0302",
           "VCF0447","VCF0451","VCF0471","VCF0502a","VCF0704a","VCF0706","VCF0723","VCF0803","VCF0824",
           "VCF0824","VCF0867a","VCF0876a","VCF0879","VCF0880a","VCF0900","VCF0900b","VCF0900c","VCF0901a",
           "VCF0902","VCF1015","VCF1016","VCF9030","VCF9055","VCF9056","VCF9057","VCF9058","VCF9059",
           "VCF9060","VCF9123","VCF9124","X")
selected_data = aa[ , !(names(aa) %in% del.name)]
write.csv(selected_data,file = "selected_data.csv",row.names =T)


##########################################################################################
# Imput NAs by mice                                                                      #  
##########################################################################################

library(mice)
imputed1=mice(selected_data,printFlag=T,m=1)
imputed <- complete(imputed1)
write.csv(imputed,file = "mice_imputed.csv",row.names =T)