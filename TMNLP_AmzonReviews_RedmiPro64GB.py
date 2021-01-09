# -*- coding: utf-8 -*-
"""
Created on Tue Aug 18 10:25:55 2020

@author: RAVI
"""



import requests   # Importing requests to extract content from a url
from bs4 import BeautifulSoup as bs # Beautifulsoup is for web scrapping...used to scrap specific content 
import re  #regular expressions

#import nltk

from nltk.corpus import stopwords

import matplotlib.pyplot as plt

pip install wordcloud

from wordcloud import WordCloud

# creating empty reviews list 
redmi_reviews=[]
#forest = ["the","king","of","jungle"]

for i in range(1,31):
  ip=[]  
  url="https://www.amazon.in/Redmi-Pro-Blue-64GB-Storage/product-reviews/B07DJHR5DY/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber="+str(i)
  response = requests.get(url)
  soup = bs(response.content,"html.parser")# creating soup object to iterate over the extracted content 
  reviews = soup.findAll("span",attrs={"class","a-size-base review-text review-text-content"})# Extracting the content under specific tags  
  for i in range(len(reviews)):
    ip.append(reviews[i].text)  
  redmi_reviews=redmi_reviews+ip  # adding the reviews of one page to empty list which in future contains all the reviews

# writng reviews in a text file 
with open("RedmiProBlue64GB.txt","w",encoding='utf8') as output:
    output.write(str(redmi_reviews))
    
import os
os.getcwd()    

  red_rev_string = redmi_reviews
    
# Joinining all the reviews into single paragraph 
red_rev_string = " ".join(redmi_reviews)



# Removing unwanted symbols incase if exists
red_rev_string = re.sub("[^A-Za-z" "]+"," ",red_rev_string).lower()
red_rev_string = re.sub("[0-9" "]+"," ",red_rev_string)



# words that contained in iphone 7 reviews
red_reviews_words = red_rev_string.split(" ")


import nltk
nltk.download('stopwords')

stop_words = stopwords.words('english')

import os
os.getcwd()    

with open("C:/RAVI/Data science/Assignments/Text Mining-Natural Language Processing(NLP)/stop.txt","r") as sw:
    stopwords = sw.read()

stopwords = stopwords.split("\n")


temp = ["this","is","awsome","Data","Science"]
[i for i in temp if i not in "is"]



red_reviews_words = [w for w in red_reviews_words if not w in stopwords]



# Joinining all the reviews into single paragraph 
red_rev_string = " ".join(red_reviews_words)

# WordCloud can be performed on the string inputs. That is the reason we have combined 
# entire reviews into single paragraph
# Simple word cloud


wordcloud_red = WordCloud(
                      background_color='black',
                      width=1800,
                      height=1400
                     ).generate(red_rev_string)

plt.imshow(wordcloud_red)

# positive words # Choose the path for +ve words stored in system
with open("C:/RAVI/Data science/Assignments/Text Mining-Natural Language Processing(NLP)/positive-words.txt","r") as pos:
  poswords = pos.read().split("\n")
  
#poswords = poswords[36:]




# negative words  Choose path for -ve words stored in system
with open("C:/RAVI/Data science/Assignments/Text Mining-Natural Language Processing(NLP)/negative-words.txt","r") as neg:
  negwords = neg.read().split("\n")

negwords = negwords[37:]

# negative word cloud
# Choosing the only words which are present in negwords
red_neg_in_neg = " ".join ([w for w in red_reviews_words if w in negwords])

wordcloud_neg_in_neg = WordCloud(
                      background_color='black',
                      width=1800,
                      height=1400
                     ).generate(red_neg_in_neg)

plt.imshow(wordcloud_neg_in_neg)

# Positive word cloud
# Choosing the only words which are present in positive words
red_pos_in_pos = " ".join ([w for w in red_reviews_words if w in poswords])
wordcloud_pos_in_pos = WordCloud(
                      background_color='white',
                      width=1800,
                      height=1400
                     ).generate(red_pos_in_pos)

plt.imshow(wordcloud_pos_in_pos)

