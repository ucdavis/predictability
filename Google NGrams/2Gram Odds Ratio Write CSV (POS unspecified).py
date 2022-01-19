#important preface, if the instructions on the ZS library page don't work, try the following:
#download the wheel from https://www.lfd.uci.edu/~gohlke/pythonlibs/#zs for whichever version of windows you have
#then use pip install "file location of the wheel without the quotes"

###POS Unspecified was used for the staub replication

###See https://vorpus.org/papers/draft/zs-paper.pdf for the full paper on the ZS file format

if __name__ == '__main__':

    from zs import ZS
    import math
    import csv

    z = ZS("D:\PhD Stuff\Linguistics Stuff\Google NGRAMS\google-books-eng-us-all-20120701-2gram.zs") #bigram corpus
    z_1gram = ZS("D:\PhD Stuff\Linguistics Stuff\Google NGRAMS\google-books-eng-us-all-20120701-1gram.zs") #onegram corpus

    onegram_memory = {}
    corpus_size = 0
    for i, item in enumerate (z_1gram.search()):

        one_gram_string = item.decode('utf-8') #decode string
        one_gram_output = one_gram_string.split('\t') #split by tab
        one_gram_match_count = one_gram_output[2] #match count
        one_gram_volume_count = one_gram_output[3] #volume count
        word = one_gram_output[0]
        word = word.lower()
        year = one_gram_output[1]
                    

        if i % 10000000 == 0: #for debugging purposes
            print(one_gram_string)
            print(word)

        if len(word.split('_')) == 2: #length of rows with part of speech tagging are different than length of rows without, this line accounts for that and splits by POS.
            word, POS = word.split('_')

        else:
            word = word

        if word.isalpha(): #ignore non-words
            corpus_size = corpus_size + int(one_gram_match_count) #keep track of corpus size

    def two_gram_odds_ratio(word1, word2): #function for getting the odds ratio for any two words
        
        if __name__ == '__main__':
        
            word1_capital = word1.capitalize() #search query doesn't let us ignore case, so we have to account for each combination of capital and lowercase in English
            word2_capital = word2.capitalize()
            word1 = word1.encode("utf-8")
            word2 = word2.encode("utf-8")
            word1_capital = word1_capital.encode("utf-8")
            word2_capital = word2_capital.encode("utf-8")
            two_gram_one = 0
            two_gram_two = 0
            for record in z.search(start=word1 + b" " + word2 + b"\t1980", stop=word1 + b" " + word2 + b"\t2009"): #search for each 2gram between 1980 and 2009
                navy_ship = record.decode("utf-8")
                n_gram = navy_ship.split("\t") #separate the string so we can focus on what we care about 
                two_gram_one = two_gram_one + int(n_gram[2]) #go through each record and add up all the match counts and volume counts
                two_gram_two = two_gram_two + int(n_gram[3]) 
            for record in z.search(start=word1_capital + b" " + word2_capital + b"\t1980", stop=word1_capital + b" " + word2_capital + b"\t2009"): #accounting for capitalization
                navy_ship = record.decode("utf-8")
                n_gram = navy_ship.split("\t")   
                two_gram_one = two_gram_one + int(n_gram[2]) 
                two_gram_two = two_gram_two + int(n_gram[3])
            for record in z.search(start=word1_capital + b" " + word2 + b"\t1980", stop=word1_capital + b" " + word2 + b"\t2009"): #accounting for capitalization
                navy_ship = record.decode("utf-8")
                n_gram = navy_ship.split("\t")   
                two_gram_one = two_gram_one + int(n_gram[2])
                two_gram_two = two_gram_two + int(n_gram[3])
            for record in z.search(start=word1 + b" " + word2_capital + b"\t1980", stop=word1 + b" " + word2_capital + b"\t2009"): #accounting for capitalization
                navy_ship = record.decode("utf-8")
                n_gram = navy_ship.split("\t")   
                two_gram_one = two_gram_one + int(n_gram[2])
                two_gram_two = two_gram_two + int(n_gram[3])
        
            one_gram_one = 0
            one_gram_two = 0
            for record in z_1gram.search(prefix=word1): #number of times word1 appears total
                navy = record.decode("utf-8")
                one_gram = navy.split("\t")
                if int(one_gram[1]) >= 1980:
                    one_gram_one = one_gram_one +int(one_gram[2]) #loop through and add up all the match counts for each word
                    one_gram_two = one_gram_two +int(one_gram[3]) #volume count
            for record in z_1gram.search(prefix=word1_capital):  #need to account for capitalization for this as well
                navy = record.decode("utf-8")
                one_gram = navy.split("\t")
                if int(one_gram[1]) >= 1980:
                    one_gram_one = one_gram_one +int(one_gram[2]) #match count
                    one_gram_two = one_gram_two +int(one_gram[3]) #volume count

            n2_count_one = 0
            n2_count_two = 0
            for record in z_1gram.search(prefix=word2): #number of times word1 appears total
                ship = record.decode("utf-8")
                n2_gram = ship.split("\t")
                if int(n2_gram[1]) >= 1980:
                    n2_count_one = n2_count_one +int(n2_gram[2]) #match count
                    n2_count_two = n2_count_two +int(n2_gram[3]) #volume count
            for record in z_1gram.search(prefix=word2_capital):  #need to account for capitalization for this as well
                ship = record.decode("utf-8")
                n2_gram = ship.split("\t")
                if int(n2_gram[1]) >= 1980:
                    n2_count_one = n2_count_one +int(n2_gram[2]) #match count
                    n2_count_two = n2_count_two +int(n2_gram[3]) #volume count

            
            #count((count(gun rights)/count(gun)) / count(gun) - count(gun rights) / count(gun) --> this is the odds ratio
        
            if one_gram_one != 0: #to avoid divide by zero errors
                P_value = (two_gram_one / one_gram_one)
                oddsratio_col1 = (two_gram_one / one_gram_one) / ((one_gram_one - two_gram_one) / one_gram_one) # P / (1-P)
                oddsratio_col2 = (two_gram_two / one_gram_two) / ((one_gram_two - two_gram_two) / one_gram_two)
                deltaP_col1 = (two_gram_one / one_gram_one) - ((n2_count_one - two_gram_one) / (corpus_size - one_gram_one))
                deltaP_col2 = (two_gram_two / one_gram_two) - ((n2_count_two - n2_count_two) / (corpus_size - one_gram_two))
                oddsratio_simplified1 = (two_gram_one / one_gram_one)
                oddsratio_simplified2 = (two_gram_two / one_gram_two)
            
        #this allows us to get an idea of how predictive word1 is of word 2
        #in other words, what are the odds of word2 given word1?
        
        
            if two_gram_one != 0 and one_gram_one != 0: #this prevents an error due to trying to divide by 0
                return(word1.decode('utf-8'), word2.decode('utf-8'), two_gram_one, two_gram_two, one_gram_one, one_gram_two, P_value, oddsratio_col1, oddsratio_col2, deltaP_col1, deltaP_col2, oddsratio_simplified1, oddsratio_simplified2, n2_count_one, n2_count_two, corpus_size)
            else: 
                return(word1.decode('utf-8'), word2.decode('utf-8'), two_gram_one, two_gram_two, one_gram_one, one_gram_two, "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")

    if __name__ == '__main__':
        with open('D:\PhD Stuff\Linguistics Stuff\Google NGRAMS\outputforngrams.csv', newline='') as csvfile_input: #Read CSV with pairs of words, output match count, volume count, P value, odds ratio, etc
            with open('D:\PhD Stuff\Linguistics Stuff\Google NGRAMS\words for oddsratio calculations exp4.csv', 'w') as csvfile_output:
                my_two_grams_reader = csv.reader(csvfile_input, delimiter=',')
                my_two_grams_writer = csv.writer(csvfile_output, delimiter=',', lineterminator='\n')
                my_two_grams_writer.writerow(["N1", "N2", "Match Count of 2gram", "Volume Count of 2gram", "Match Count of N1", "Volume Count of N1", "P Value", "Odds Ratio of Match count", "Odds Ratio of Volume Count", "Delta P of Match Count", "Delta P of Volume Count", "Odds ratio simplified (match count)", "Odds ratio simplified (volume count)", "N2 Match Count", "N2 Volume Count", "Corpus Size"])
                for row in my_two_grams_reader:
                    odds_ratio = two_gram_odds_ratio(row[0], row[1])
                    my_two_grams_writer.writerow(odds_ratio)
                    #header should be:
                    #word1, word2, match count of 2gram, volume count of 2gram, match count of N1, volume count of N1, P value, odds ratio of match count, odds ratio of volume count, log odds ratio match count, log odds ratio volume count
            

