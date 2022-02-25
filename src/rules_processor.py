from durable.lang import *
import pandas as pd
import inflect

p = inflect.engine()

import random

# Utility rule functions
def sometimes(t, p=0.5, prefix=''):
    """Sometimes return a string passed to the function."""
    if random.random()>=p:
        return f'{prefix}{t}'
    return ''

def occasionally(t):
    """Sometimes return a string passed to the function."""
    return sometimes(t, p=0.2)

def rarely(t):
    """Rarely return a string passed to the function."""
    return sometimes(t, p=0.05)

def pickone_equally(l, prefix='', suffix=''):
    """Return an item from a list,
       selected at random with equal probability."""
    t = random.choice(l)
    if t:
        return f'{prefix}{t}{suffix}'
    return suffix

def pickfirst_prob(l, p=0.5):
    """Select the first item in a list with the specified probability,
       else select an item, with equal probability, from the rest of the list."""
    if len(l)>1 and random.random() >= p:
        return random.choice(l[1:])
    return l[0]
  

#  Rule / df processors
import json

def rulesbyrow(row, ruleset):
    row = json.loads(json.dumps(row.to_dict()))
    post(ruleset,row)

def df_json(df):
    """Convert rows in a pandas dataframe to a JSON string.
       Cast the JSON string back to a list of dicts 
       that are palatable to the rules engine. 
    """
    return json.loads(df.to_json(orient='records'))
  
def factsbyrow(row, ruleset):
    row = json.loads(json.dumps(row.to_dict()))
    assert_fact(ruleset,row)
    
#
