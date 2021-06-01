from flair.models import TextClassifier
import numpy as np
from flair.data import Sentence
from segtok.segmenter import split_single
import pandas as pd
import re
import os


os.chdir("Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper")


################################## Functions #################################
classifier = TextClassifier.load('en-sentiment')

def clean(raw):
    """ Remove hyperlinks and markup """
    result = re.sub("<[a][^>]*>(.+?)</[a]>", 'Link.', raw)
    result = re.sub('&gt;', "", result)
    result = re.sub('&#x27;', "'", result)
    result = re.sub('&quot;', '"', result)
    result = re.sub('&#x2F;', ' ', result)
    result = re.sub('<p>', ' ', result)
    result = re.sub('</i>', '', result)
    result = re.sub('&#62;', '', result)
    result = re.sub('<i>', ' ', result)
    result = re.sub("\n", '', result)
    return result

def make_sentences(text):
    """ Break apart text into a list of sentences """
    sentences = [sent for sent in split_single(text)]
    return sentences

def predict(sentence):
    """ Predict the sentiment of a sentence """
    if sentence == "":
        return 0
    text = Sentence(sentence)
    # stacked_embeddings.embed(text)
    classifier.predict(text)
    value = text.labels[0].to_dict()['value'] 
    if value == 'POSITIVE':
        result = text.to_dict()['labels'][0]['confidence']
    else:
        result = -(text.to_dict()['labels'][0]['confidence'])
    return round(result, 3)



def get_scores(sentences):
    """ Call predict on every sentence of a text """
    results = []
    
    for i in range(0, len(sentences)): 
        results.append(predict(sentences[i]))
    
    return results

def get_sum(scores):
    
    result = round(sum(scores), 3)
    return result



##############################################################################

df = pd.read_csv('Data/wsb_reddit_complete_clean.csv')
df = df.dropna()
df = df.reset_index(drop=True)

df.Text = df.Text.apply(clean)
df['sentences'] = df.Text.apply(make_sentences)

df['scores'] = df['sentences'].apply(get_scores)
df['scores_sum'] = df.scores.apply(get_sum)

df.to_csv(index="False")

df.to_csv('/Users/danielbulat/Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper/Data/reddit_sentiment.csv',
          index = False)












