# stop words from http://www.nettiapina.fi/finnish-stopword-list/
# with some manual additions

# tuomo.a.nieminen@gmail.com
# 16.2.2016

fin_stopwords <- read.table("../data/fin_stopwords.txt")
fi_stop <- as.character(fin_stopwords$V1)
save(file="../data/fi_stopwords.Rda",fi_stop)
