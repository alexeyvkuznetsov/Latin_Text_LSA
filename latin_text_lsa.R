setwd("D:/GitHub/Latin_Text_LSA/")

# load required libraries
library(tm)
library(udpipe)
library(lsa)
library(ggplot2)
library(scatterplot3d)
library(corrplot)
#library(readr)
#library(quanteda)
#library(tidytext)

prologus<-paste(scan(file ="files/01 prologus.txt",what='character'),collapse=" ")
historia_g<-paste(scan(file ="files/02 historia_g.txt",what='character'),collapse=" ")
recapitulatio<-paste(scan(file ="files/03 recapitulatio.txt",what='character'),collapse=" ")
historia_w<-paste(scan(file ="files/04 historia_w.txt",what='character'),collapse=" ")
historia_s<-paste(scan(file ="files/05 historia_s.txt",what='character'),collapse=" ")

prologus<-data.frame(texts=prologus)
historia_g<-data.frame(texts=historia_g)
recapitulatio<-data.frame(texts=recapitulatio)
historia_w<-data.frame(texts=historia_w)
historia_s<-data.frame(texts=historia_s)

prologus$book<-"1 Prologus"
historia_g$book<-"2 Historia Gothorum"
recapitulatio$book<-"3 Recapitulatio"
historia_w$book<-"4 Historia Wandalorum"
historia_s$book<-"5 Historia Suevorum"

historia<-rbind(prologus,historia_g,recapitulatio,historia_w,historia_s)

#historia$texts <- stripWhitespace(historia$texts)
historia$texts <- tolower(historia$texts)
historia$texts <- removePunctuation(historia$texts)
historia$texts <- removeNumbers(historia$texts)

# Stopwords

rome_number<-paste(scan(file ="rom number 1000.txt",what='character'),collapse=" ")
rome_number<-tolower(rome_number)
rome_number
save(rome_number, file="rome_number_v.txt")

rome_number_1000 <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", "XX", "XXI", "XXII", "XXIII", "XXIV", "XXV", "XXVI", "XXVII", "XXVIII", "XXIX", "XXX", "XXXI", "XXXII", "XXXIII", "XXXIV", "XXXV", "XXXVI", "XXXVII", "XXXVIII", "XXXIX", "XL", "XLI", "XLII", "XLIII", "XLIV", "XLV", "XLVI", "XLVII", "XLVIII", "XLIX", "L", "LI", "LII", "LIII", "LIV", "LV", "LVI", "LVII", "LVIII", "LIX", "LX", "LXI", "LXII", "LXIII", "LXIV", "LXV", "LXVI", "LXVII", "LXVIII", "LXIX", "LXX", "LXXI", "LXXII", "LXXIII", "LXXIV", "LXXV", "LXXVI", "LXXVII", "LXXVIII", "LXXIX", "LXXX", "LXXXI", "LXXXII", "LXXXIII", "LXXXIV", "LXXXV", "LXXXVI", "LXXXVII", "LXXXVIII", "LXXXIX", "XC", "XCI", "XCII", "XCIII", "XCIV", "XCV", "XCVI", "XCVII", "XCVIII", "XCIX", "C", "CI", "CII", "CIII", "CIV", "CV", "CVI", "CVII", "CVIII", "CIX", "CX", "CXI", "CXII", "CXIII", "CXIV", "CXV", "CXVI", "CXVII", "CXVIII", "CXIX", "CXX", "CXXI", "CXXII", "CXXIII", "CXXIV", "CXXV", "CXXVI", "CXXVII", "CXXVIII", "CXXIX", "CXXX", "CXXXI", "CXXXII", "CXXXIII", "CXXXIV", "CXXXV", "CXXXVI", "CXXXVII", "CXXXVIII", "CXXXIX", "CXL", "CXLI", "CXLII", "CXLIII", "CXLIV", "CXLV", "CXLVI", "CXLVII", "CXLVIII", "CXLIX", "CL", "CLI", "CLII", "CLIII", "CLIV", "CLV", "CLVI", "CLVII", "CLVIII", "CLIX", "CLX", "CLXI", "CLXII", "CLXIII", "CLXIV", "CLXV", "CLXVI", "CLXVII", "CLXVIII", "CLXIX", "CLXX", "CLXXI", "CLXXII", "CLXXIII", "CLXXIV", "CLXXV", "CLXXVI", "CLXXVII", "CLXXVIII", "CLXXIX", "CLXXX", "CLXXXI", "CLXXXII", "CLXXXIII", "CLXXXIV", "CLXXXV", "CLXXXVI", "CLXXXVII", "CLXXXVIII", "CLXXXIX", "CXC", "CXCI", "CXCII", "CXCIII", "CXCIV", "CXCV", "CXCVI", "CXCVII", "CXCVIII", "CXCIX", "CC", "CCI", "CCII", "CCIII", "CCIV", "CCV", "CCVI", "CCVII", "CCVIII", "CCIX", "CCX", "CCXI", "CCXII", "CCXIII", "CCXIV", "CCXV", "CCXVI", "CCXVII", "CCXVIII", "CCXIX", "CCXX", "CCXXI", "CCXXII", "CCXXIII", "CCXXIV", "CCXXV", "CCXXVI", "CCXXVII", "CCXXVIII", "CCXXIX", "CCXXX", "CCXXXI", "CCXXXII", "CCXXXIII", "CCXXXIV", "CCXXXV", "CCXXXVI", "CCXXXVII", "CCXXXVIII", "CCXXXIX", "CCXL", "CCXLI", "CCXLII", "CCXLIII", "CCXLIV", "CCXLV", "CCXLVI", "CCXLVII", "CCXLVIII", "CCXLIX", "CCL", "CCLI", "CCLII", "CCLIII", "CCLIV", "CCLV", "CCLVI", "CCLVII", "CCLVIII", "CCLIX", "CCLX", "CCLXI", "CCLXII", "CCLXIII", "CCLXIV", "CCLXV", "CCLXVI", "CCLXVII", "CCLXVIII", "CCLXIX", "CCLXX", "CCLXXI", "CCLXXII", "CCLXXIII", "CCLXXIV", "CCLXXV", "CCLXXVI", "CCLXXVII", "CCLXXVIII", "CCLXXIX", "CCLXXX", "CCLXXXI", "CCLXXXII", "CCLXXXIII", "CCLXXXIV", "CCLXXXV", "CCLXXXVI", "CCLXXXVII", "CCLXXXVIII", "CCLXXXIX", "CCXC", "CCXCI", "CCXCII", "CCXCIII", "CCXCIV", "CCXCV", "CCXCVI", "CCXCVII", "CCXCVIII", "CCXCIX", "CCC", "CCCI", "CCCII", "CCCIII", "CCCIV", "CCCV", "CCCVI", "CCCVII", "CCCVIII", "CCCIX", "CCCX", "CCCXI", "CCCXII", "CCCXIII", "CCCXIV", "CCCXV", "CCCXVI", "CCCXVII", "CCCXVIII", "CCCXIX", "CCCXX", "CCCXXI", "CCCXXII", "CCCXXIII", "CCCXXIV", "CCCXXV", "CCCXXVI", "CCCXXVII", "CCCXXVIII", "CCCXXIX", "CCCXXX", "CCCXXXI", "CCCXXXII", "CCCXXXIII", "CCCXXXIV", "CCCXXXV", "CCCXXXVI", "CCCXXXVII", "CCCXXXVIII", "CCCXXXIX", "CCCXL", "CCCXLI", "CCCXLII", "CCCXLIII", "CCCXLIV", "CCCXLV", "CCCXLVI", "CCCXLVII", "CCCXLVIII", "CCCXLIX", "CCCL", "CCCLI", "CCCLII", "CCCLIII", "CCCLIV", "CCCLV", "CCCLVI", "CCCLVII", "CCCLVIII", "CCCLIX", "CCCLX", "CCCLXI", "CCCLXII", "CCCLXIII", "CCCLXIV", "CCCLXV", "CCCLXVI", "CCCLXVII", "CCCLXVIII", "CCCLXIX", "CCCLXX", "CCCLXXI", "CCCLXXII", "CCCLXXIII", "CCCLXXIV", "CCCLXXV", "CCCLXXVI", "CCCLXXVII", "CCCLXXVIII", "CCCLXXIX", "CCCLXXX", "CCCLXXXI", "CCCLXXXII", "CCCLXXXIII", "CCCLXXXIV", "CCCLXXXV", "CCCLXXXVI", "CCCLXXXVII", "CCCLXXXVIII", "CCCLXXXIX", "CCCXC", "CCCXCI", "CCCXCII", "CCCXCIII", "CCCXCIV", "CCCXCV", "CCCXCVI", "CCCXCVII", "CCCXCVIII", "CCCXCIX", "CD", "CDI", "CDII", "CDIII", "CDIV", "CDV", "CDVI", "CDVII", "CDVIII", "CDIX", "CDX", "CDXI", "CDXII", "CDXIII", "CDXIV", "CDXV", "CDXVI", "CDXVII", "CDXVIII", "CDXIX", "CDXX", "CDXXI", "CDXXII", "CDXXIII", "CDXXIV", "CDXXV", "CDXXVI", "CDXXVII", "CDXXVIII", "CDXXIX", "CDXXX", "CDXXXI", "CDXXXII", "CDXXXIII", "CDXXXIV", "CDXXXV", "CDXXXVI", "CDXXXVII", "CDXXXVIII", "CDXXXIX", "CDXL", "CDXLI", "CDXLII", "CDXLIII", "CDXLIV", "CDXLV", "CDXLVI", "CDXLVII", "CDXLVIII", "CDXLIX", "CDL", "CDLI", "CDLII", "CDLIII", "CDLIV", "CDLV", "CDLVI", "CDLVII", "CDLVIII", "CDLIX", "CDLX", "CDLXI", "CDLXII", "CDLXIII", "CDLXIV", "CDLXV", "CDLXVI", "CDLXVII", "CDLXVIII", "CDLXIX", "CDLXX", "CDLXXI", "CDLXXII", "CDLXXIII", "CDLXXIV", "CDLXXV", "CDLXXVI", "CDLXXVII", "CDLXXVIII", "CDLXXIX", "CDLXXX", "CDLXXXI", "CDLXXXII", "CDLXXXIII", "CDLXXXIV", "CDLXXXV", "CDLXXXVI", "CDLXXXVII", "CDLXXXVIII", "CDLXXXIX", "CDXC", "CDXCI", "CDXCII", "CDXCIII", "CDXCIV", "CDXCV", "CDXCVI", "CDXCVII", "CDXCVIII", "CDXCIX", "D", "DI", "DII", "DIII", "DIV", "DV", "DVI", "DVII", "DVIII", "DIX", "DX", "DXI", "DXII", "DXIII", "DXIV", "DXV", "DXVI", "DXVII", "DXVIII", "DXIX", "DXX", "DXXI", "DXXII", "DXXIII", "DXXIV", "DXXV", "DXXVI", "DXXVII", "DXXVIII", "DXXIX", "DXXX", "DXXXI", "DXXXII", "DXXXIII", "DXXXIV", "DXXXV", "DXXXVI", "DXXXVII", "DXXXVIII", "DXXXIX", "DXL", "DXLI", "DXLII", "DXLIII", "DXLIV", "DXLV", "DXLVI", "DXLVII", "DXLVIII", "DXLIX", "DL", "DLI", "DLII", "DLIII", "DLIV", "DLV", "DLVI", "DLVII", "DLVIII", "DLIX", "DLX", "DLXI", "DLXII", "DLXIII", "DLXIV", "DLXV", "DLXVI", "DLXVII", "DLXVIII", "DLXIX", "DLXX", "DLXXI", "DLXXII", "DLXXIII", "DLXXIV", "DLXXV", "DLXXVI", "DLXXVII", "DLXXVIII", "DLXXIX", "DLXXX", "DLXXXI", "DLXXXII", "DLXXXIII", "DLXXXIV", "DLXXXV", "DLXXXVI", "DLXXXVII", "DLXXXVIII", "DLXXXIX", "DXC", "DXCI", "DXCII", "DXCIII", "DXCIV", "DXCV", "DXCVI", "DXCVII", "DXCVIII", "DXCIX", "DC", "DCI", "DCII", "DCIII", "DCIV", "DCV", "DCVI", "DCVII", "DCVIII", "DCIX", "DCX", "DCXI", "DCXII", "DCXIII", "DCXIV", "DCXV", "DCXVI", "DCXVII", "DCXVIII", "DCXIX", "DCXX", "DCXXI", "DCXXII", "DCXXIII", "DCXXIV", "DCXXV", "DCXXVI", "DCXXVII", "DCXXVIII", "DCXXIX", "DCXXX", "DCXXXI", "DCXXXII", "DCXXXIII", "DCXXXIV", "DCXXXV", "DCXXXVI", "DCXXXVII", "DCXXXVIII", "DCXXXIX", "DCXL", "DCXLI", "DCXLII", "DCXLIII", "DCXLIV", "DCXLV", "DCXLVI", "DCXLVII", "DCXLVIII", "DCXLIX", "DCL", "DCLI", "DCLII", "DCLIII", "DCLIV", "DCLV", "DCLVI", "DCLVII", "DCLVIII", "DCLIX", "DCLX", "DCLXI", "DCLXII", "DCLXIII", "DCLXIV", "DCLXV", "DCLXVI", "DCLXVII", "DCLXVIII", "DCLXIX", "DCLXX", "DCLXXI", "DCLXXII", "DCLXXIII", "DCLXXIV", "DCLXXV", "DCLXXVI", "DCLXXVII", "DCLXXVIII", "DCLXXIX", "DCLXXX", "DCLXXXI", "DCLXXXII", "DCLXXXIII", "DCLXXXIV", "DCLXXXV", "DCLXXXVI", "DCLXXXVII", "DCLXXXVIII", "DCLXXXIX", "DCXC", "DCXCI", "DCXCII", "DCXCIII", "DCXCIV", "DCXCV", "DCXCVI", "DCXCVII", "DCXCVIII", "DCXCIX", "DCC", "DCCI", "DCCII", "DCCIII", "DCCIV", "DCCV", "DCCVI", "DCCVII", "DCCVIII", "DCCIX", "DCCX", "DCCXI", "DCCXII", "DCCXIII", "DCCXIV", "DCCXV", "DCCXVI", "DCCXVII", "DCCXVIII", "DCCXIX", "DCCXX", "DCCXXI", "DCCXXII", "DCCXXIII", "DCCXXIV", "DCCXXV", "DCCXXVI", "DCCXXVII", "DCCXXVIII", "DCCXXIX", "DCCXXX", "DCCXXXI", "DCCXXXII", "DCCXXXIII", "DCCXXXIV", "DCCXXXV", "DCCXXXVI", "DCCXXXVII", "DCCXXXVIII", "DCCXXXIX", "DCCXL", "DCCXLI", "DCCXLII", "DCCXLIII", "DCCXLIV", "DCCXLV", "DCCXLVI", "DCCXLVII", "DCCXLVIII", "DCCXLIX", "DCCL", "DCCLI", "DCCLII", "DCCLIII", "DCCLIV", "DCCLV", "DCCLVI", "DCCLVII", "DCCLVIII", "DCCLIX", "DCCLX", "DCCLXI", "DCCLXII", "DCCLXIII", "DCCLXIV", "DCCLXV", "DCCLXVI", "DCCLXVII", "DCCLXVIII", "DCCLXIX", "DCCLXX", "DCCLXXI", "DCCLXXII", "DCCLXXIII", "DCCLXXIV", "DCCLXXV", "DCCLXXVI", "DCCLXXVII", "DCCLXXVIII", "DCCLXXIX", "DCCLXXX", "DCCLXXXI", "DCCLXXXII", "DCCLXXXIII", "DCCLXXXIV", "DCCLXXXV", "DCCLXXXVI", "DCCLXXXVII", "DCCLXXXVIII", "DCCLXXXIX", "DCCXC", "DCCXCI", "DCCXCII", "DCCXCIII", "DCCXCIV", "DCCXCV", "DCCXCVI", "DCCXCVII", "DCCXCVIII", "DCCXCIX", "DCCC", "DCCCI", "DCCCII", "DCCCIII", "DCCCIV", "DCCCV", "DCCCVI", "DCCCVII", "DCCCVIII", "DCCCIX", "DCCCX", "DCCCXI", "DCCCXII", "DCCCXIII", "DCCCXIV", "DCCCXV", "DCCCXVI", "DCCCXVII", "DCCCXVIII", "DCCCXIX", "DCCCXX", "DCCCXXI", "DCCCXXII", "DCCCXXIII", "DCCCXXIV", "DCCCXXV", "DCCCXXVI", "DCCCXXVII", "DCCCXXVIII", "DCCCXXIX", "DCCCXXX", "DCCCXXXI", "DCCCXXXII", "DCCCXXXIII", "DCCCXXXIV", "DCCCXXXV", "DCCCXXXVI", "DCCCXXXVII", "DCCCXXXVIII", "DCCCXXXIX", "DCCCXL", "DCCCXLI", "DCCCXLII", "DCCCXLIII", "DCCCXLIV", "DCCCXLV", "DCCCXLVI", "DCCCXLVII", "DCCCXLVIII", "DCCCXLIX", "DCCCL", "DCCCLI", "DCCCLII", "DCCCLIII", "DCCCLIV", "DCCCLV", "DCCCLVI", "DCCCLVII", "DCCCLVIII", "DCCCLIX", "DCCCLX", "DCCCLXI", "DCCCLXII", "DCCCLXIII", "DCCCLXIV", "DCCCLXV", "DCCCLXVI", "DCCCLXVII", "DCCCLXVIII", "DCCCLXIX", "DCCCLXX", "DCCCLXXI", "DCCCLXXII", "DCCCLXXIII", "DCCCLXXIV", "DCCCLXXV", "DCCCLXXVI", "DCCCLXXVII", "DCCCLXXVIII", "DCCCLXXIX", "DCCCLXXX", "DCCCLXXXI", "DCCCLXXXII", "DCCCLXXXIII", "DCCCLXXXIV", "DCCCLXXXV", "DCCCLXXXVI", "DCCCLXXXVII", "DCCCLXXXVIII", "DCCCLXXXIX", "DCCCXC", "DCCCXCI", "DCCCXCII", "DCCCXCIII", "DCCCXCIV", "DCCCXCV", "DCCCXCVI", "DCCCXCVII", "DCCCXCVIII", "DCCCXCIX", "CM", "CMI", "CMII", "CMIII", "CMIV", "CMV", "CMVI", "CMVII", "CMVIII", "CMIX", "CMX", "CMXI", "CMXII", "CMXIII", "CMXIV", "CMXV", "CMXVI", "CMXVII", "CMXVIII", "CMXIX", "CMXX", "CMXXI", "CMXXII", "CMXXIII", "CMXXIV", "CMXXV", "CMXXVI", "CMXXVII", "CMXXVIII", "CMXXIX", "CMXXX", "CMXXXI", "CMXXXII", "CMXXXIII", "CMXXXIV", "CMXXXV", "CMXXXVI", "CMXXXVII", "CMXXXVIII", "CMXXXIX", "CMXL", "CMXLI", "CMXLII", "CMXLIII", "CMXLIV", "CMXLV", "CMXLVI", "CMXLVII", "CMXLVIII", "CMXLIX", "CML", "CMLI", "CMLII", "CMLIII", "CMLIV", "CMLV", "CMLVI", "CMLVII", "CMLVIII", "CMLIX", "CMLX", "CMLXI", "CMLXII", "CMLXIII", "CMLXIV", "CMLXV", "CMLXVI", "CMLXVII", "CMLXVIII", "CMLXIX", "CMLXX", "CMLXXI", "CMLXXII", "CMLXXIII", "CMLXXIV", "CMLXXV", "CMLXXVI", "CMLXXVII", "CMLXXVIII", "CMLXXIX", "CMLXXX", "CMLXXXI", "CMLXXXII", "CMLXXXIII", "CMLXXXIV", "CMLXXXV", "CMLXXXVI", "CMLXXXVII", "CMLXXXVIII", "CMLXXXIX", "CMXC", "CMXCI", "CMXCII", "CMXCIII", "CMXCIV", "CMXCV", "CMXCVI", "CMXCVII", "CMXCVIII", "CMXCIX", "M")

customStopWords <- c("ann", "annus", "aer", "aes", "aera", "num._rom.", "xnum._rom.", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom.")

lat_stopwords_romnum <- c("i", "ii", "iii", "iiii", "iv", "v", "vii", "viii", "ix", "x", "xi", "xii", "xiii", "xiv", "xv", "xvi", "xvii", "xviii", "xix", "xx", "xxi", "xxii", "xxiii", "xxiv", "xxv", "xxvi", "xxvii", "xxviii", "xxix", "xxx", "xxxi", "xxxii", "xxxiii", "xxxiv", "xxxv", "xxxvi", "xxxvii", "xxxviii", "xxxix", "xl", "xli", "xlii", "xliii", "xliv", "xlv", "xlvi", "xlvii", "xlviii", "xlix", "l", "li", "lii", "liii", "liv", "lv", "lvi", "lvii", "lviii", "lix", "lx", "lxi", "lxii", "lxiii", "lxiv", "lxv", "lxvi", "lxvii", "lxviii", "lxix", "lxx", "lxxi", "lxxii", "lxxiii", "lxxiv", "lxxv", "lxxvi", "lxxvii", "lxxviii", "lxxix", "lxxx", "lxxxi", "lxxxii", "lxxxiii", "lxxxiv", "lxxxv", "lxxxvi", "lxxxvii", "lxxxviii", "lxxxix", "xc", "xci", "xcii", "xciii", "xciv", "xcv", "xcvi", "xcvii", "xcviii", "xcix", "c")
lat_stop_perseus <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "unus", "ut", "quoque")

#save(lat_stop_perseus,file="lat_stop_perseus.Rda")

#load("lat_stop_perseus.Rda")

#MyStopwords <- c(lat_stop_perseus, customStopWords, lat_stopwords_romnum)

MyStopwords <- c(lat_stop_perseus, lat_stopwords_romnum)

#historia$texts <- removeWords(historia$texts, c(lat_stopwords, customStopWords))

historia$texts <- removeWords(historia$texts, MyStopwords)


# UDPipe annotation
#udmodel_latin <- udpipe_download_model(language = "latin_ittb")
#udmodel_latin <- udpipe_load_model(ud_model$file_model)
udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.4-190531.udpipe")

x <- udpipe_annotate(udmodel_latin, x = historia$texts, doc_id = historia$book, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)

save(x,file="historia_annotated_dataset.Rda")
load("historia_annotated_dataset.Rda")

## Get a data.frame with 1 row per doc_id/lemma or specific POS tag

#dtf <- document_term_frequencies(x[, c("doc_id", "lemma")])

dtf <- subset(x, upos %in% c("ADJ", "ADV", "PROPN", "VERB", "NOUN"))

dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")


## Create a document/term/matrix for building a topic model
dtm <- document_term_matrix(x = dtf)
## Remove words which do not occur that much
dtm <- dtm_remove_lowfreq(dtm, minfreq = 2)
head(dtm_colsums(dtm))


# +xstincum
## Remove nouns which you really do not like (mostly too common nouns)
#dtm <- dtm_remove_terms(dtm, terms = c("ann", "adipio", "annus", "aer", "aes", "aera", "suus", "filius", "multus", "num._rom.", "xnum._rom.", "xstincum", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom."))

dtm <- dtm_remove_terms(dtm, terms = c("ann", "adipio", "annus", "aer", "aes", "aera", "num._rom.", "xnum._rom.", "xstincum", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom.", "cdlxvus", "cdxcnum._rom.", "cdxcus", "cdxix", "cdxlnum._rom.", "cdxlvium", "cdxlvus", "cdxx", "cdxxcvus", "cdxxxnum._rom.", "clxxnum._rom.", "cxiium", "cxx", "dclix", "dcxliix", "dcxlis", "dcxxnum._rom.", "dcxxxix", "dlxnum._rom.", "dlxxxnum._rom.", "dlxxxvus", "dxnum._rom.", "dxxvus"))

## Or keep of these nouns the top 50 based on mean term-frequency-inverse document frequency
#dtm <- dtm_remove_tfidf(dtm, top = 50)

## Convert dtm to a list of text
dtm.to.list <- apply(dtm, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})

## convert list of text to a Corpus

myCorpus <- VCorpus(VectorSource(dtm.to.list))
inspect(myCorpus)

# Created term-document matrix

tdm <- TermDocumentMatrix(myCorpus)

td_matrix <- as.matrix(tdm)


###################################################


# Calculate a weighted term-document matrix according to the chosen local and/or global weighting scheme

tdm.tfidf <- lw_tf(td_matrix) * gw_idf(td_matrix) # weighting

#tdm.tfidf <- lw_bintf(td_matrix) * gw_idf(td_matrix) # weighting


# Calculate the latent semantic space for the give document-term matrix and create lsaSpace:
# Created LSA space

lsaSpace <- lsa::lsa(tdm.tfidf, dims=dimcalc_share()) # create LSA space

lsaMatrix <- as.textmatrix(lsaSpace)

############
# Begining
############
############
####
####

# Example from: Mastering Text Mining with R 
# https://github.com/pmtempone/tec_semantica/blob/627f79c01389a39ba07621c90e695336268e424c/tec_semantica_R/ls.R
#Compute distance between documents and scale the multidimentional semantic space (MDS) onto two dimensions

#dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix

dist.mat.lsa <- dist(t(lsaMatrix))

dist.mat.lsa # check distance mantrix

# Plot the distance matrix:

fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2) # Classical (Metric) Multidimensional Scaling

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=historia$book)) + 
  geom_text(data=points,aes(x=x, y=y-2, label=row.names(historia)))



#3D plot

library(scatterplot3d)

fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=3)

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

colors <- rep(c("blue", "green", "red", "purple", "orange" ))

s3d <- scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color=colors, pch=20, angle = 65, box = FALSE,
                     main=" ", xlab="x", ylab="y", zlab="z", type="h")
legend("top", legend = c("1 Prologus", "2 Historia Gothorum", "3 Recapitulatio", "4 Historia Wandalorum", "5 Historia Suevorum"),
       col =  c("blue", "green", "red", "purple", "orange"), pch = 16, bty = "n", bg = "transparent",
       inset = 0.2, xpd = TRUE)

s3d$points3d(seq(0,0,0), seq(0,0,0), seq(0,0,0), col="red", type="h", pch=8)

####
####
############
############
###
###

#COSINE similarity
# 
# compute cosine distance matrix

lsaMatrix <- as.textmatrix(lsaSpace)

mat.lsa.cosine <- cosine(lsaMatrix) #Cosine similarity matrix

#mat.lsa.cosine <- cosine(as.textmatrix(lsaSpace)) #Cosine similarity matrix

mat.lsa.cosine

#colnames(mat.lsa.cosine) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

rownames(mat.lsa.cosine) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

round((mat.lsa.cosine), 2) # round the results to a couple of decimals

mat.lsa.cosine

# Plot cosine similarity matrix
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

library(corrplot)
corrplot(mat.lsa.cosine)

col <- colorRampPalette(c("red", "white", "lightblue")) 

corrplot(mat.lsa.cosine, method = "number")

corrplot(mat.lsa.cosine, method="color", addCoef.col = "black", col = col(10), cl.pos = "b", tl.srt = 30, tl.col = "black")

corrplot(mat.lsa.cosine, method = "circle", addCoef.col = "black")

#corrplot(mat.lsa.cosine, method = "number", order = "hclust", hclust.method = "complete")





# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
# https://github.com/kassambara/ggcorrplot
library(ggcorrplot)
ggcorrplot(mat.lsa.cosine, lab = TRUE)



## ÝÊÑÏÅÐÈÌÅÍÒ


mat.lsa.pearson <- cor(lsaMatrix, method="pearson")

mat.lsa.pearson

#colnames(mat.lsa.pearson) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

rownames(mat.lsa.pearson) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

round((mat.lsa.pearson), 2) # round the results to a couple of decimals

corrplot(mat.lsa.pearson, method="color", addCoef.col = "black", col = col(10), cl.pos = "b", tl.srt = 30, tl.col = "black")

ggcorrplot(mat.lsa.pearson, lab = TRUE)




#spearman

mat.lsa.spearman <- cor(lsaMatrix, method="spearman")

mat.lsa.spearman

#colnames(mat.lsa.spearman) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

rownames(mat.lsa.spearman) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

round((mat.lsa.spearman), 2) # round the results to a couple of decimals

corrplot(mat.lsa.spearman, method="color", addCoef.col = "black", col = col(10), cl.pos = "b", tl.srt = 30, tl.col = "black")

ggcorrplot(mat.lsa.spearman, lab = TRUE)




#kendall

mat.lsa.kendall <- cor(lsaMatrix, method="spearman")

mat.lsa.kendall

#colnames(mat.lsa.kendall) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

rownames(mat.lsa.kendall) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

round((mat.lsa.kendall), 2) # round the results to a couple of decimals

corrplot(mat.lsa.kendall, method="color", addCoef.col = "black", col = col(10), cl.pos = "b", tl.srt = 30, tl.col = "black")

ggcorrplot(mat.lsa.kendall, lab = TRUE)



############################
## END
############################
# https://github.com/katyalrajat/corpus_mining/blob/9b805cd229b2f5260bfa1007765b0aa6c992fc8d/code.R

lsaMatrix <- as.textmatrix(lsaSpace)

#Calculate similarity of documents in LSA space
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#Similarity matrix
cs.lsa <- as.matrix(cosineSim(t(lsaMatrix)))
write.csv(cs.lsa,"cs_lsa.csv")

library(corrplot)
corrplot(cs.lsa)
corrplot(cs.lsa, method = "square")
corrplot(cs.lsa, method = "number")


############################
## END 2
############################




library(LSAfun)

neighbors("gens", n=36, tvectors = lsaMatrix)

plot_neighbors("gens", n=6, tvectors = lsaMatrix, method = "MDS", dims = 3, col = c("black","blue","red"))



