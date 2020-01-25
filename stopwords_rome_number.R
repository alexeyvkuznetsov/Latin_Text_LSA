###
# Ancient Latin stopwords
# items count: 4011
# https://github.com/aurelberra/stopwords
###


setwd("D:/GitHub/Latin_Text_LSA/")


rome_number_1000_01 <- c("i", "ii", "iii", "iiii", "iv", "v", "vii", "viii", "ix", "x", "xi", "xii", "xiii", "xiv", "xv", "xvi", "xvii", "xviii", "xix", "xx", "xxi", "xxii", "xxiii", "xxiv", "xxv", "xxvi", "xxvii", "xxviii", "xxix", "xxx", "xxxi", "xxxii", "xxxiii", "xxxiv", "xxxv", "xxxvi", "xxxvii", "xxxviii", "xxxix", "xl", "xli", "xlii", "xliii", "xliv", "xlv", "xlvi", "xlvii", "xlviii", "xlix", "l", "li", "lii", "liii", "liv", "lv", "lvi", "lvii", "lviii", "lix", "lx", "lxi", "lxii", "lxiii", "lxiv", "lxv", "lxvi", "lxvii", "lxviii", "lxix", "lxx", "lxxi", "lxxii", "lxxiii", "lxxiv", "lxxv", "lxxvi", "lxxvii", "lxxviii", "lxxix", "lxxx", "lxxxi", "lxxxii", "lxxxiii", "lxxxiv", "lxxxv", "lxxxvi", "lxxxvii", "lxxxviii", "lxxxix", "xc", "xci", "xcii", "xciii", "xciv", "xcv", "xcvi", "xcvii", "xcviii", "xcix", "c")

rome_number_1000_02 <- c("ci", "cii", "ciii", "civ", "cv", "cvi", "cvii", "cviii", "cix", "cx", "cxi", "cxii", "cxiii", "cxiv", "cxv", "cxvi", "cxvii", "cxviii", "cxix", "cxx", "cxxi", "cxxii", "cxxiii", "cxxiv", "cxxv", "cxxvi", "cxxvii", "cxxviii", "cxxix", "cxxx", "cxxxi", "cxxxii", "cxxxiii", "cxxxiv", "cxxxv", "cxxxvi", "cxxxvii", "cxxxviii", "cxxxix", "cxl", "cxli", "cxlii", "cxliii", "cxliv", "cxlv", "cxlvi", "cxlvii", "cxlviii", "cxlix", "cl", "cli", "clii", "cliii", "cliv", "clv", "clvi", "clvii", "clviii", "clix", "clx", "clxi", "clxii", "clxiii", "clxiv", "clxv", "clxvi", "clxvii", "clxviii", "clxix", "clxx", "clxxi", "clxxii", "clxxiii", "clxxiv", "clxxv", "clxxvi", "clxxvii", "clxxviii", "clxxix", "clxxx", "clxxxi", "clxxxii", "clxxxiii", "clxxxiv", "clxxxv", "clxxxvi", "clxxxvii", "clxxxviii", "clxxxix", "cxc", "cxci", "cxcii", "cxciii", "cxciv", "cxcv", "cxcvi", "cxcvii", "cxcviii", "cxcix", "cc")

rome_number_1000_03 <- c("cci", "ccii", "cciii", "cciv", "ccv", "ccvi", "ccvii", "ccviii", "ccix", "ccx", "ccxi", "ccxii", "ccxiii", "ccxiv", "ccxv", "ccxvi", "ccxvii", "ccxviii", "ccxix", "ccxx", "ccxxi", "ccxxii", "ccxxiii", "ccxxiv", "ccxxv", "ccxxvi", "ccxxvii", "ccxxviii", "ccxxix", "ccxxx", "ccxxxi", "ccxxxii", "ccxxxiii", "ccxxxiv", "ccxxxv", "ccxxxvi", "ccxxxvii", "ccxxxviii", "ccxxxix", "ccxl", "ccxli", "ccxlii", "ccxliii", "ccxliv", "ccxlv", "ccxlvi", "ccxlvii", "ccxlviii", "ccxlix", "ccl", "ccli", "cclii", "ccliii", "ccliv", "cclv", "cclvi", "cclvii", "cclviii", "cclix", "cclx", "cclxi", "cclxii", "cclxiii", "cclxiv", "cclxv", "cclxvi", "cclxvii", "cclxviii", "cclxix", "cclxx", "cclxxi", "cclxxii", "cclxxiii", "cclxxiv", "cclxxv", "cclxxvi", "cclxxvii", "cclxxviii", "cclxxix", "cclxxx", "cclxxxi", "cclxxxii", "cclxxxiii", "cclxxxiv", "cclxxxv", "cclxxxvi", "cclxxxvii", "cclxxxviii", "cclxxxix", "ccxc", "ccxci", "ccxcii", "ccxciii", "ccxciv", "ccxcv", "ccxcvi", "ccxcvii", "ccxcviii", "ccxcix", "ccc")
  
rome_number_1000_04 <- c("ccci", "cccii", "ccciii", "ccciv", "cccv", "cccvi", "cccvii", "cccviii", "cccix", "cccx", "cccxi", "cccxii", "cccxiii", "cccxiv", "cccxv", "cccxvi", "cccxvii", "cccxviii", "cccxix", "cccxx", "cccxxi", "cccxxii", "cccxxiii", "cccxxiv", "cccxxv", "cccxxvi", "cccxxvii", "cccxxviii", "cccxxix", "cccxxx", "cccxxxi", "cccxxxii", "cccxxxiii", "cccxxxiv", "cccxxxv", "cccxxxvi", "cccxxxvii", "cccxxxviii", "cccxxxix", "cccxl", "cccxli", "cccxlii", "cccxliii", "cccxliv", "cccxlv", "cccxlvi", "cccxlvii", "cccxlviii", "cccxlix", "cccl", "cccli", "ccclii", "cccliii", "cccliv", "ccclv", "ccclvi", "ccclvii", "ccclviii", "ccclix", "ccclx", "ccclxi", "ccclxii", "ccclxiii", "ccclxiv", "ccclxv", "ccclxvi", "ccclxvii", "ccclxviii", "ccclxix", "ccclxx", "ccclxxi", "ccclxxii", "ccclxxiii", "ccclxxiv", "ccclxxv", "ccclxxvi", "ccclxxvii", "ccclxxviii", "ccclxxix", "ccclxxx", "ccclxxxi", "ccclxxxii", "ccclxxxiii", "ccclxxxiv", "ccclxxxv", "ccclxxxvi", "ccclxxxvii", "ccclxxxviii", "ccclxxxix", "cccxc", "cccxci", "cccxcii", "cccxciii", "cccxciv", "cccxcv", "cccxcvi", "cccxcvii", "cccxcviii", "cccxcix", "cd")

rome_number_1000_05 <- c("cdi", "cdii", "cdiii", "cdiv", "cdv", "cdvi", "cdvii", "cdviii", "cdix", "cdx", "cdxi", "cdxii", "cdxiii", "cdxiv", "cdxv", "cdxvi", "cdxvii", "cdxviii", "cdxix", "cdxx", "cdxxi", "cdxxii", "cdxxiii", "cdxxiv", "cdxxv", "cdxxvi", "cdxxvii", "cdxxviii", "cdxxix", "cdxxx", "cdxxxi", "cdxxxii", "cdxxxiii", "cdxxxiv", "cdxxxv", "cdxxxvi", "cdxxxvii", "cdxxxviii", "cdxxxix", "cdxl", "cdxli", "cdxlii", "cdxliii", "cdxliv", "cdxlv", "cdxlvi", "cdxlvii", "cdxlviii", "cdxlix", "cdl", "cdli", "cdlii", "cdliii", "cdliv", "cdlv", "cdlvi", "cdlvii", "cdlviii", "cdlix", "cdlx", "cdlxi", "cdlxii", "cdlxiii", "cdlxiv", "cdlxv", "cdlxvi", "cdlxvii", "cdlxviii", "cdlxix", "cdlxx", "cdlxxi", "cdlxxii", "cdlxxiii", "cdlxxiv", "cdlxxv", "cdlxxvi", "cdlxxvii", "cdlxxviii", "cdlxxix", "cdlxxx", "cdlxxxi", "cdlxxxii", "cdlxxxiii", "cdlxxxiv", "cdlxxxv", "cdlxxxvi", "cdlxxxvii", "cdlxxxviii", "cdlxxxix", "cdxc", "cdxci", "cdxcii", "cdxciii", "cdxciv", "cdxcv", "cdxcvi", "cdxcvii", "cdxcviii", "cdxcix", "d")

rome_number_1000_06 <- c("di", "dii", "diii", "div", "dv", "dvi", "dvii", "dviii", "dix", "dx", "dxi", "dxii", "dxiii", "dxiv", "dxv", "dxvi", "dxvii", "dxviii", "dxix", "dxx", "dxxi", "dxxii", "dxxiii", "dxxiv", "dxxv", "dxxvi", "dxxvii", "dxxviii", "dxxix", "dxxx", "dxxxi", "dxxxii", "dxxxiii", "dxxxiv", "dxxxv", "dxxxvi", "dxxxvii", "dxxxviii", "dxxxix", "dxl", "dxli", "dxlii", "dxliii", "dxliv", "dxlv", "dxlvi", "dxlvii", "dxlviii", "dxlix", "dl", "dli", "dlii", "dliii", "dliv", "dlv", "dlvi", "dlvii", "dlviii", "dlix", "dlx", "dlxi", "dlxii", "dlxiii", "dlxiv", "dlxv", "dlxvi", "dlxvii", "dlxviii", "dlxix", "dlxx", "dlxxi", "dlxxii", "dlxxiii", "dlxxiv", "dlxxv", "dlxxvi", "dlxxvii", "dlxxviii", "dlxxix", "dlxxx", "dlxxxi", "dlxxxii", "dlxxxiii", "dlxxxiv", "dlxxxv", "dlxxxvi", "dlxxxvii", "dlxxxviii", "dlxxxix", "dxc", "dxci", "dxcii", "dxciii", "dxciv", "dxcv", "dxcvi", "dxcvii", "dxcviii", "dxcix", "dc")

rome_number_1000_07 <- c("dci", "dcii", "dciii", "dciv", "dcv", "dcvi", "dcvii", "dcviii", "dcix", "dcx", "dcxi", "dcxii", "dcxiii", "dcxiv", "dcxv", "dcxvi", "dcxvii", "dcxviii", "dcxix", "dcxx", "dcxxi", "dcxxii", "dcxxiii", "dcxxiv", "dcxxv", "dcxxvi", "dcxxvii", "dcxxviii", "dcxxix", "dcxxx", "dcxxxi", "dcxxxii", "dcxxxiii", "dcxxxiv", "dcxxxv", "dcxxxvi", "dcxxxvii", "dcxxxviii", "dcxxxix", "dcxl", "dcxli", "dcxlii", "dcxliii", "dcxliv", "dcxlv", "dcxlvi", "dcxlvii", "dcxlviii", "dcxlix", "dcl", "dcli", "dclii", "dcliii", "dcliv", "dclv", "dclvi", "dclvii", "dclviii", "dclix", "dclx", "dclxi", "dclxii", "dclxiii", "dclxiv", "dclxv", "dclxvi", "dclxvii", "dclxviii", "dclxix", "dclxx", "dclxxi", "dclxxii", "dclxxiii", "dclxxiv", "dclxxv", "dclxxvi", "dclxxvii", "dclxxviii", "dclxxix", "dclxxx", "dclxxxi", "dclxxxii", "dclxxxiii", "dclxxxiv", "dclxxxv", "dclxxxvi", "dclxxxvii", "dclxxxviii", "dclxxxix", "dcxc", "dcxci", "dcxcii", "dcxciii", "dcxciv", "dcxcv", "dcxcvi", "dcxcvii", "dcxcviii", "dcxcix", "dcc")

rome_number_1000_08 <- c("dcci", "dccii", "dcciii", "dcciv", "dccv", "dccvi", "dccvii", "dccviii", "dccix", "dccx", "dccxi", "dccxii", "dccxiii", "dccxiv", "dccxv", "dccxvi", "dccxvii", "dccxviii", "dccxix", "dccxx", "dccxxi", "dccxxii", "dccxxiii", "dccxxiv", "dccxxv", "dccxxvi", "dccxxvii", "dccxxviii", "dccxxix", "dccxxx", "dccxxxi", "dccxxxii", "dccxxxiii", "dccxxxiv", "dccxxxv", "dccxxxvi", "dccxxxvii", "dccxxxviii", "dccxxxix", "dccxl", "dccxli", "dccxlii", "dccxliii", "dccxliv", "dccxlv", "dccxlvi", "dccxlvii", "dccxlviii", "dccxlix", "dccl", "dccli", "dcclii", "dccliii", "dccliv", "dcclv", "dcclvi", "dcclvii", "dcclviii", "dcclix", "dcclx", "dcclxi", "dcclxii", "dcclxiii", "dcclxiv", "dcclxv", "dcclxvi", "dcclxvii", "dcclxviii", "dcclxix", "dcclxx", "dcclxxi", "dcclxxii", "dcclxxiii", "dcclxxiv", "dcclxxv", "dcclxxvi", "dcclxxvii", "dcclxxviii", "dcclxxix", "dcclxxx", "dcclxxxi", "dcclxxxii", "dcclxxxiii", "dcclxxxiv", "dcclxxxv", "dcclxxxvi", "dcclxxxvii", "dcclxxxviii", "dcclxxxix", "dccxc", "dccxci", "dccxcii", "dccxciii", "dccxciv", "dccxcv", "dccxcvi", "dccxcvii", "dccxcviii", "dccxcix", "dccc")

rome_number_1000_09 <- c("dccci", "dcccii", "dccciii", "dccciv", "dcccv", "dcccvi", "dcccvii", "dcccviii", "dcccix", "dcccx", "dcccxi", "dcccxii", "dcccxiii", "dcccxiv", "dcccxv", "dcccxvi", "dcccxvii", "dcccxviii", "dcccxix", "dcccxx", "dcccxxi", "dcccxxii", "dcccxxiii", "dcccxxiv", "dcccxxv", "dcccxxvi", "dcccxxvii", "dcccxxviii", "dcccxxix", "dcccxxx", "dcccxxxi", "dcccxxxii", "dcccxxxiii", "dcccxxxiv", "dcccxxxv", "dcccxxxvi", "dcccxxxvii", "dcccxxxviii", "dcccxxxix", "dcccxl", "dcccxli", "dcccxlii", "dcccxliii", "dcccxliv", "dcccxlv", "dcccxlvi", "dcccxlvii", "dcccxlviii", "dcccxlix", "dcccl", "dcccli", "dccclii", "dcccliii", "dcccliv", "dccclv", "dccclvi", "dccclvii", "dccclviii", "dccclix", "dccclx", "dccclxi", "dccclxii", "dccclxiii", "dccclxiv", "dccclxv", "dccclxvi", "dccclxvii", "dccclxviii", "dccclxix", "dccclxx", "dccclxxi", "dccclxxii", "dccclxxiii", "dccclxxiv", "dccclxxv", "dccclxxvi", "dccclxxvii", "dccclxxviii", "dccclxxix", "dccclxxx", "dccclxxxi", "dccclxxxii", "dccclxxxiii", "dccclxxxiv", "dccclxxxv", "dccclxxxvi", "dccclxxxvii", "dccclxxxviii", "dccclxxxix", "dcccxc", "dcccxci", "dcccxcii", "dcccxciii", "dcccxciv", "dcccxcv", "dcccxcvi", "dcccxcvii", "dcccxcviii", "dcccxcix", "cm")

rome_number_1000_10 <- c("cmi", "cmii", "cmiii", "cmiv", "cmv", "cmvi", "cmvii", "cmviii", "cmix", "cmx", "cmxi", "cmxii", "cmxiii", "cmxiv", "cmxv", "cmxvi", "cmxvii", "cmxviii", "cmxix", "cmxx", "cmxxi", "cmxxii", "cmxxiii", "cmxxiv", "cmxxv", "cmxxvi", "cmxxvii", "cmxxviii", "cmxxix", "cmxxx", "cmxxxi", "cmxxxii", "cmxxxiii", "cmxxxiv", "cmxxxv", "cmxxxvi", "cmxxxvii", "cmxxxviii", "cmxxxix", "cmxl", "cmxli", "cmxlii", "cmxliii", "cmxliv", "cmxlv", "cmxlvi", "cmxlvii", "cmxlviii", "cmxlix", "cml", "cmli", "cmlii", "cmliii", "cmliv", "cmlv", "cmlvi", "cmlvii", "cmlviii", "cmlix", "cmlx", "cmlxi", "cmlxii", "cmlxiii", "cmlxiv", "cmlxv", "cmlxvi", "cmlxvii", "cmlxviii", "cmlxix", "cmlxx", "cmlxxi", "cmlxxii", "cmlxxiii", "cmlxxiv", "cmlxxv", "cmlxxvi", "cmlxxvii", "cmlxxviii", "cmlxxix", "cmlxxx", "cmlxxxi", "cmlxxxii", "cmlxxxiii", "cmlxxxiv", "cmlxxxv", "cmlxxxvi", "cmlxxxvii", "cmlxxxviii", "cmlxxxix", "cmxc", "cmxci", "cmxcii", "cmxciii", "cmxciv", "cmxcv", "cmxcvi", "cmxcvii", "cmxcviii", "cmxcix", "m")


rome_number_1000 <- c(rome_number_1000_01, rome_number_1000_02, rome_number_1000_03, rome_number_1000_04, rome_number_1000_05, rome_number_1000_06, rome_number_1000_07, rome_number_1000_08, rome_number_1000_09, rome_number_1000_10)


save(rome_number_1000,file="rome_number_1000.Rda")

load("rome_number_1000.Rda")

class(rome_number_1000)



# Perseus Digital Library list
## http://www.perseus.tufts.edu/hopper/stopwords 2017-10-09

lat_stop_perseus <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "unus", "ut")

save(lat_stop_perseus,file="lat_stop_perseus.Rda")

# Digital Classicist list
## http://wiki.digitalclassicist.org/Stopwords_for_Greek_and_Latin
## originally from Perseus list
## but differs from online Perseus list ("unus" and "ut" were removed or lost)

## 95-word stoplist augmented AB 2017-10-07
lat_stop_digiclass <- c("a", "ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "jam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "siue", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "vel", "uero", "vero")

# Classical Language Toolkit Latin
## CLTK currently uses the Perseus list
## https://github.com/cltk/cltk/blob/master/cltk/stop/latin/stops.py

## But Patrick Burns is working towards better Latin stopwords for the CLTK
## I refer here to the NLP-based lists quoted
## in https://github.com/diyclassics/stopwords (slides)
## and loosely call them `cltk` lists 2017-10-07

## 100-word stoplist by mean probability

lat_stop_cltk_mean <- c("et", "in", "est", "non", "ad", "ut", "cum", "quod", "qui", "sed", "si", "de", "quae", "quam", "per", "ex", "nec", "sunt", "esse", "se", "hoc", "enim", "ab", "aut", "autem", "etiam", "quid", "te", "atque", "uel", "eius", "me", "quo", "sit", "iam", "quia", "ne", "haec", "mihi", "tamen", "ac", "tibi", "nam", "sic", "ita", "id", "pro", "eo", "nunc", "uero", "neque", "inter", "quem", "erat", "ille", "ergo", "ipse", "eum", "quibus", "quoque", "sibi", "ego", "quidem", "nisi", "qua", "omnia", "hic", "post", "fuit", "tu", "nihil", "ea", "illa", "his", "omnes", "nos", "esset", "modo", "dum", "sine", "quis", "ubi", "sicut", "ante", "sub", "tam", "secundum", "deus", "potest", "dei", "nobis", "quos", "igitur", "ei", "omnibus", "res", "cui", "sua", "apud", "eorum")

## 100-word stoplist by variance probability

lat_stop_cltk_var <- c("et", "in", "est", "non", "quod", "ad", "ut", "cum", "qui", "de", "si", "sed", "quae", "per", "ex", "quam", "esse", "nec", "te", "sunt", "autem", "me", "enim", "se", "dig", "hoc", "aut", "ab", "bibit", "quid", "uel", "atque", "mihi", "eius", "quaestio", "pro", "etiam", "tibi", "quia", "sit", "iam", "secundum", "quo", "ac", "ne", "ergo", "od", "nihil", "tu", "haec", "sic", "id", "nam", "ego", "neque", "tamen", "eum", "deus", "nunc", "dei", "ita", "eo", "uero", "sicut", "uos", "hic", "erat", "nouus", "fuit", "nos", "ille", "inter", "dum", "quem", "quoque", "quidem", "esset", "bellum", "ipse", "sibi", "nummus", "anno", "quibus", "post", "his", "omnia", "ea", "super", "qua", "sub", "illa", "dominus", "deo", "rex", "nisi", "totus", "dixit", "dicitur", "ed", "ante")

## 100-word stoplist by entropy

lat_stop_cltk_ent <- c("et", "in", "est", "non", "ad", "ut", "cum", "quod", "qui", "sed", "si", "de", "quae", "quam", "per", "ex", "nec", "sunt", "esse", "se", "hoc", "ab", "enim", "aut", "autem", "etiam", "quid", "quo", "atque", "eius", "te", "uel", "sit", "me", "iam", "ne", "haec", "quia", "tamen", "nam", "ac", "mihi", "ita", "sic", "tibi", "id", "pro", "eo", "inter", "nunc", "quem", "ipse", "uero", "neque", "quibus", "ille", "erat", "eum", "sibi", "qua", "nisi", "quoque", "ergo", "quidem", "omnia", "post", "hic", "fuit", "ego", "ea", "nihil", "omnes", "his", "illa", "modo", "tu", "esset", "sine", "nos", "dum", "ubi", "ante", "quis", "tam", "sub", "sicut", "quos", "omnibus", "potest", "nobis", "sua", "cui", "igitur", "res", "ei", "tantum", "cuius", "apud", "contra", "magis")

## 100-word aggregate stoplist by Borda sort

lat_stop_cltk_borda <- c("et", "in", "est", "non", "ad", "ut", "quod", "cum", "qui", "si", "sed", "de", "quae", "quam", "per", "ex", "nec", "esse", "sunt", "se", "hoc", "enim", "autem", "ab", "aut", "te", "quid", "uel", "etiam", "atque", "me", "eius", "quo", "sit", "quia", "iam", "ne", "ac", "mihi", "haec", "tamen", "tibi", "pro", "nam", "id", "ita", "sic", "eo", "neque", "uero", "eum", "nunc", "inter", "ergo", "erat", "quem", "ipse", "ego", "quibus", "nihil", "ille", "quoque", "quidem", "sibi", "dig", "nisi", "qua", "post", "ea", "tu", "hic", "fuit", "omnia", "his", "esset", "nos", "sicut", "illa", "omnes", "sine", "secundum", "bibit", "modo", "dum", "quis", "quaestio", "ubi", "deus", "od", "ante", "dei", "potest", "tam", "sub", "ei", "uos", "nouus", "quos", "nobis", "bellum")

# List from "Stopwords ISO" Latin
## https://github.com/stopwords-iso/stopwords-la

lat_stop_iso <- c("a","ab","ac","ad","at","atque","aut","autem","cum","de","dum","e","erant","erat","est","et","etiam","ex","haec","hic","hoc","in","ita","me","nec","neque","non","per","qua","quae","quam","qui","quibus","quidem","quo","quod","re","rebus","rem","res","sed","si","sic","sunt","tamen","tandem","te","ut","vel")