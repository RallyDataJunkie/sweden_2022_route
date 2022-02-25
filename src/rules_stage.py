## Rules

from durable.lang import *
from durable.lang import _main_host

txts = []

if _main_host is not None:
    _main_host._ruleset_directory.clear()

from numpy import inf
from pandas import isnull


with ruleset('rule_multi_stage'):
  
    #Display something about the crew in first place
    @when_all(m.overall_stage_pos==1)
    def whos_in_first(c):
        """Generate a sentence to report on the first placed vehicle."""
        #We can add additional state, accessible from other rules
        #In this case, record the Crew and Brand for the first placed crew
        c.s.first_code = c.m.code
        c.s.prev_code = c.m.code
        
        abs_int_overall_pos = abs(int(c.m.overall_position_delta))
        
        if c.m.gained_lead and not isnull(c.m.overall_position_delta) and c.m.overall_position_delta:
            lead_typ = f" picked up {p.number_to_words(int(c.m.overall_position_delta))} {p.plural('place',abs_int_overall_pos)} to " if c.m.overall_position_delta else ''
            lead_typ = lead_typ + pickone_equally(['gain','take'])+" the overall lead" + sometimes("of the rally", prefix=' ')
        elif c.m.gained_lead:
            lead_typ = " took the overall lead"+ sometimes("of the rally", prefix=' ')
        elif c.m.retained_lead:
            lead_typ = pickone_equally([f" retained{sometimes('the', prefix=' ')} overall lead", " kept hold of the overall lead"]) + sometimes("of the rally", prefix=' ')
        elif not isnull(c.m.overall_position_delta) and c.m.overall_position_delta:
            overall_pos = p.number_to_words(p.ordinal(int(c.m.overall_pos)))
            lead_typ = f' moved up {p.number_to_words(int(c.m.overall_position_delta))} {p.plural("place",abs_int_overall_pos)} to {overall_pos} overall' if c.m.overall_position_delta > 0 else f' dropped {p.number_to_words(int(abs(c.m.overall_position_delta)))} {p.plural("place",abs_int_overall_pos)} to {overall_pos} overall'
        else:
            lead_typ=''
            
        #Python f-strings make it easy to generate text sentences that include data elements
        txt=f'{c.m.code} {pickone_equally(["took the stage", "recorded the stage win", "took the stage win"])} and {lead_typ} {pickone_equally(["with", "in", "recording", "taking", "marking", "making"])} a time of {c.m.TimeInS}s.'
        txt=txt.replace(' ,',',').replace(',,',',')
        txts.append(txt)
        stage_txts[c.m.code] = txt
        
    #We can be a bit more creative in the other results
    @when_all((m.overall_stage_pos>1) & (m.diff<=60))
    def whos_where(c):
        """Generate a sentence to describe the position of each other placed vehicle."""
        
        abs_int_overall_pos = abs(int(c.m.overall_position_delta))
        
        #Use the inflect package to natural language textify position numbers...
        nth = p.number_to_words(p.ordinal(int(c.m.overall_stage_pos)))
        #prevnth = p.number_to_words(p.ordinal(int(c.m.overall_stage_pos)-1))
        
        #Use various probabalistic text generators to make a comment for each other result
        first_opts = [c.s.first_code, 'the stage winner', f'stage winner {c.s.first_code}']
        first_text = "{}s behind {}".format(str(round(c.m.gap,1)), pickone_equally(first_opts))
        #if c.m.Brand==c.s.first_brand:
        #    first_opts.append(f'the first placed {c.m.Brand}')
        
        desc_opts = [f'with a time of {c.m.TimeInS}s',
                             '{}{}'.format(first_text,
                                           sometimes(f"in a time of {c.m.TimeInS}s", prefix=" "))]
        if c.m.diff<0.1:
          t3 = f' {pickone_equally(["recording the same time as", "with the same time as", "matching the time of"])}  {c.s.prev_code}'
        elif c.m.diff<1:
          t3 = pickone_equally([f' {pickone_equally(["and just", ", just", ", only"])} {round(c.m.diff,1)}s {pickone_equally(["behind", "further behind", "further back behind"])} {c.s.prev_code}',
                                f', just {round(c.m.diff,1)}s off {c.s.prev_code}'])
        else:
          desc_opts.append(f'{round(c.m.diff,1)}s {pickone_equally(["behind", "further back behind"])} {c.s.prev_code}')
          t3=''
        
        t = pickone_equally(desc_opts,
                           prefix=', ')
        
        if c.m.gained_lead and not isnull(c.m.overall_position_delta) and c.m.overall_position_delta:
            lead_typ = f" and picked up {p.number_to_words(int(c.m.overall_position_delta))} {p.plural('place',abs_int_overall_pos)}"+ pickone_equally([f" to gain{sometimes('the', prefix=' ')} overall lead ", f" took{sometimes('the', prefix=' ')} overall lead"]) + sometimes("of the rally", prefix=' ')
        elif c.m.gained_lead:
            lead_typ = " and took{sometimes('the', prefix=' ')} overall lead"+ sometimes("of the rally", prefix=' ')
        elif c.m.retained_lead:
            lead_typ = pickone_equally([f" and retained{sometimes('the', prefix=' ')} overall lead", " kept hold of the overall lead"]) + sometimes("of the rally", prefix=' ')
        elif c.m.lost_lead:
            lead_typ = f" and dropped {p.number_to_words(int(abs(c.m.overall_position_delta)))} {p.plural('place',abs_int_overall_pos)}" +f" to lose the overall lead, falling back to {p.number_to_words(p.ordinal(int(c.m.overall_pos)))} overall"
        elif not isnull(c.m.overall_position_delta) and c.m.overall_position_delta:
            overall_pos = p.number_to_words(p.ordinal(int(c.m.overall_pos)))
            lead_typ = f' and moved up {p.number_to_words(int(c.m.overall_position_delta))} {p.plural("place",abs_int_overall_pos)} to {overall_pos} overall' if c.m.overall_position_delta > 0 else f' dropped {p.number_to_words(int(abs(c.m.overall_position_delta)))} {p.plural("place",abs_int_overall_pos)} to {overall_pos} overall'
        else:
            lead_typ=''
          
        stage_pos = f'{pickone_equally(["was in", "took"])} {nth}{pickfirst_prob([""," place"," position"])} on stage'
        #And add even more variation possibilities into the returned generated sentence
        txt=f'{c.m.code} {stage_pos}{t} {lead_typ}{t3}.'
        txt=txt.replace(' ,',',').replace(',,',',')
        
        if not isnull(c.m.control):
          txt = f"{txt} {c.m.code} also picked up a {c.m.penaltyDurationS}s penalty at {c.m.control} / {c.location} {c.m.type} ({c.m.reason})."
        
        txts.append(txt)
        stage_txts[c.m.code] = txt
        c.s.prev_code = c.m.code

    @when_all((m.overall_stage_pos>1) & (m.diff>60) & (m.diff<9999))
    def whos_where_bigdiff(c):
        """Generate a sentence to describe the position of each other placed vehicle."""
        
        abs_int_overall_pos = abs(int(c.m.overall_position_delta))
        
        #Use the inflect package to natural language textify position numbers...
        nth = p.number_to_words(p.ordinal(int(c.m.overall_stage_pos)))
        #Use various probabalistic text generators to make a comment for each other result
        first_opts = [c.s.first_code, 'the stage winner', f'stage winner {c.s.first_code}']
        first_text = "{}s behind {}".format(str(round(c.m.gap,1)), pickone_equally(first_opts))
        #if c.m.Brand==c.s.first_brand:
        #    first_opts.append(f'the first placed {c.m.Brand}')
        t = pickone_equally([f'with a time of {c.m.TimeInS}s',
                             '{}{}'.format(first_text,
                                           sometimes(f"in a time of {c.m.TimeInS}s", prefix=" "))
                              ],
                           prefix=', ')
        t2 = f", {pickone_equally(['well back on', 'significantly further behind' ])} {c.s.prev_code}, who was {round(c.m.diff,1)}s {pickone_equally(['faster','quicker'])}"
        
        if c.m.gained_lead and not isnull(c.m.overall_position_delta) and c.m.overall_position_delta:
            lead_typ = f" and picked up {p.number_to_words(int(c.m.overall_position_delta))} {p.plural('place',abs_int_overall_pos)}"+ pickone_equally([f" to gain{sometimes('the', prefix=' ')} overall lead ", f" took{sometimes('the', prefix=' ')} overall lead"]) + sometimes("of the rally", prefix=' ')
        elif c.m.gained_lead:
            lead_typ = " and took{sometimes('the', prefix=' ')} overall lead"+ sometimes("of the rally", prefix=' ')
        elif c.m.retained_lead: 
            lead_typ = pickone_equally([f" and retained{sometimes('the', prefix=' ')} overall lead", " kept hold of the overall lead"]) + sometimes("of the rally", prefix=' ')
        elif c.m.lost_lead:
            lead_typ =  f" lost the overall lead, dropping {p.number_to_words(int(abs(c.m.overall_position_delta)))} {p.plural('place',abs_int_overall_pos)} and falling back to {p.number_to_words(p.ordinal(int(c.m.overall_pos)))} overall"
        elif not isnull(c.m.overall_position_delta) and c.m.overall_position_delta:
            overall_pos = p.number_to_words(p.ordinal(int(c.m.overall_pos)))
            lead_typ = f' and moved up {p.number_to_words(int(c.m.overall_position_delta))} {p.plural("place",abs_int_overall_pos)} to {overall_pos} overall' if c.m.overall_position_delta > 0 else f' dropped {p.number_to_words(int(abs(c.m.overall_position_delta)))} {p.plural("place",abs_int_overall_pos)} to {overall_pos} overall'
        else:
            lead_typ=''
        
        stage_pos = f'{pickone_equally(["was in", "took"])} {nth}{pickfirst_prob([""," place"," position"])} on stage'
        
        #And add even more variation possibilities into the returned generated sentence
        txt=f'{c.m.code} {stage_pos} {lead_typ}{t}{t2}.'
        txt=txt.replace(' ,',',').replace(',,',',')
        
        if not isnull(c.m.control):
          txt = f"{txt} {c.m.code} also picked up a {c.m.penaltyDurationS}s penalty at {c.m.control} / {c.location} {c.m.type} ({c.m.reason})."
        
        txts.append(txt)
        stage_txts[c.m.code] = txt
        c.s.prev_code = c.m.code
