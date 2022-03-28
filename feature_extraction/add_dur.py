#!/usr/bin/env python3

import pandas as pd
from glob import glob
import csv
import os
import numpy as np
import sys

# Get fricative durations

csv_path = '/Users/carlawiksebarrow/Desktop/frics/analysis_vux/praat_results/static/'
durs = {}

for csv_path in glob(os.path.join(csv_path, '*.txt')):
    print(csv_path)
    with open(csv_path, encoding='utf-8') as csv_f:
        csv_read = csv.reader(csv_f, delimiter='\t')

        for l in csv_read:
            l[0] = l[0].replace('_ch1', '')
            if l[8] == '0':
                durs['-'.join([l[0], l[2]])] = l[4]
#print([k for k in durs.keys() if 'käpp' in k])

#sys.exit()

#durs = pd.DataFrame(durs, columns=['id', 'dur'])
#durs = durs.set_index('id')

with open('/Users/carlawiksebarrow/Desktop/frics/analysis_vux/mel_all_files44100.csv') as fin,\
     open('/Users/carlawiksebarrow/Desktop/frics/analysis_vux/mel_all_files44100_durs.csv', 'w') as fout:

    csv_read = csv.reader(fin, delimiter=',')
    csv_write = csv.writer(fout)
    header = next(csv_read)
    header.append('dur')
    csv_write.writerow(header)
    for l in csv_read:
        id = '-'.join([l[0], l[3]])
        l.append(durs[id])
        csv_write.writerow(l)
    

#spect = pd.read_csv('/Users/carlawiksebarrow/Desktop/frics/analysis_vux/mel_all_files.csv')
#print(set(spect['trajectory']))
#print(spect['speaker'] + '-' + spect['word'] +'-' + spect['trajectory'].astype(int))
