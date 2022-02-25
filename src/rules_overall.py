## Rules

from durable.lang import *
from durable.lang import _main_host

# Install from pypi
import unidecode
# eg for TÄN umlaut
#unidecode.unidecode(c.m.code)
from pandas import isnull

txts = []
overall_txts = {}

if _main_host is not None:
    _main_host._ruleset_directory.clear()

with ruleset('rule_multi_overall'):

    #Display something about the crew in first place
    @when_all(m.overall_pos==1)
    def whos_in_first(c):
        """Generate a sentence to report on the first placed vehicle."""
        #We can add additional state, accessible from other rules
        #In this case, record the Crew and Brand for the first placed crew
        c.s.first_code = c.m.code
        c.s.prev_code = c.m.code
        
        stage_pos = p.number_to_words(p.ordinal(int(c.m.stage_position)))
        stage_win = ', with the stage win,' if c.m.stage_win else f', {stage_pos} on stage,'
        
        if c.m.gained_lead:
            lead_typ = pickone_equally([f"gained{sometimes('the', prefix=' ')} overall lead", f"took{sometimes('the', prefix=' ')} overall lead"]) + sometimes("of the rally", prefix=' ')
        elif c.m.retained_lead:
            lead_typ = pickone_equally([f"retained{sometimes('the', prefix=' ')} overall lead", "kept hold of the overall lead"]) + sometimes("of the rally", prefix=' ')
        else:
            lead_typ = f"was " + pickone_equally(["in first", "leading the rally", "at the head of the field", 
                        "in overall first", "in overall first position"])

        #Python f-strings make it easy to generate text sentences that include data elements
        txt=f'- {c.m.code}{stage_win} {lead_typ}'
        txt=txt.replace(' ,', ',').replace(',,', ',')
        txts.append(txt) # with a time of {c.m.stageTime}.')
        overall_txts[c.m.code] = txt
        #txts = txts+[f'At the end of stage {c.m.stage}:']+subtxts
        
    #We can be a bit more creative in the other results
    @when_all(m.overall_pos > 1)
    def whos_where(c):
        """Generate a sentence to describe the position of each other placed vehicle."""

        #Use the inflect package to natural language textify position numbers...
        nth = p.number_to_words(p.ordinal(int(c.m.overall_pos)))
        #Use various probabalistic text generators to make a comment for each other result
        first_opts = [c.s.first_code, 'the overall leader']

        abs_int_overall_pos = abs(int(c.m.overall_position_delta))
                
        stage_pos = p.number_to_words(p.ordinal(int(c.m.stage_position)))
        stage_win = ', with the stage win,' if c.m.stage_win else f' {sometimes("finishing in ")}{stage_pos} on stage,'
        
        pos_change=' losing the overall lead and' if c.m.lost_lead else ''

        if not isnull(c.m.overall_position_delta) and c.m.overall_position_delta:
            overall_pos = p.number_to_words(p.ordinal(int(c.m.overall_pos)))
            pos_change = pos_change+f' gaining {p.number_to_words(int(c.m.overall_position_delta))} {p.plural("place",abs_int_overall_pos)} to move up to {overall_pos} overall' if c.m.overall_position_delta > 0 else f' losing {p.number_to_words(abs_int_overall_pos)} {p.plural("place",abs_int_overall_pos)} to drop down to {overall_pos} overall'
        else:
            pos_change=f'{nth}{sometimes(" position")} overall,'
            
        #if c.m.Brand==c.s.first_brand:
        #    first_opts.append(f'the first placed {c.m.Brand}')
        #t = pickone_equally([f'with a time of {c.m.totalTime}'
        #                     #"{} behind {}".format(str(c.m.diffFirstS), pickone_equally(first_opts))
        #                     ],
        #                   prefix=', ')
        t2 = f' and {c.m.overall_gap}s {pickone_equally(["behind "+c.s.first_code, "off the lead", "off the overall lead pace"])}' if c.s.first_code!=c.s.prev_code else ''
        #And add even more variation possibilities into the returned generated sentence
        txt=f'- {c.m.code}{stage_win} {pos_change}, {round(c.m.overall_diff,1)}s behind {c.s.prev_code}{t2}'
        txt=txt.replace(' ,',',').replace(',,',',')
        txts.append(txt)
        overall_txts[c.m.code] = txt

        c.s.prev_code = c.m.code
