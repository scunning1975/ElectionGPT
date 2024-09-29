
cd "/Users/sunmingrun/Documents/GitHub/ElectionGPT/NLP"


// 1. import csv 
import delimited "/Users/sunmingrun/Documents/GitHub/ElectionGPT/NLP/avgvotes_sentiment.csv", varnames(1) clear

// 2. destring 
drop if date=="NA"
destring averagevotes avg_sentiment no_news, replace

// 3. date
gen date_stata = date(date, "YMD")
format date_stata %td

drop if party=="Republican"

// Regression 
/* Instructions: 
Electoral College votes ~ Fox dummy, msnbc dummy, bbc dummy, sentiment score, and then Fox interacted with sentiment and so on 
*/

// generate dummy
tabulate voice, gen(v)

rename v1 Anonymous
rename v2 BBC
rename v3 Fox
rename v4 MSNBC

encode voice, gen(voice_en)

reg averagevotes Fox MSNBC BBC avg_sentiment,r


reg averagevotes Fox MSNBC BBC avg_sentiment Fox##c.avg_sentiment MSNBC##c.avg_sentiment BBC##c.avg_sentiment, r

reg averagevotes voice_en##c.avg_sentiment, r


reg averagevotes Fox##c.avg_sentiment MSNBC##c.avg_sentiment BBC##c.avg_sentiment, r

