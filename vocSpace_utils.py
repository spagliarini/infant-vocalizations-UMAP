#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 07 14:42:32 2021

@author: silviapagliarini
"""
import os
import numpy as np
import pandas as pd
import csv
from pydub import AudioSegment
import scipy.io.wavfile as wav

def opensmile_executable(data, baby_id, classes, args):
    """
    Generate a text file executable on shell to compute multiple times opensmile features.
    If option labels_creation == True, it also generates a csv file containing number of the sound and label.

    INPUT
    - path to directory
    - type of dataset (can be a single directory, or a dataset keywords): see args.baby_id

    OUTPUT
    A text file for each directory with the command lines to compute MFCC for each extracted sound in the directory.
    """
    f = open(args.data_dir + '/' + 'executable_opensmile_' + baby_id + '.txt', 'w+')

    i = 0
    while i < len(data):
        #name = './build/progsrc/smilextract/SMILExtract -C config/mfcc/MFCC12_0_D_A.conf -I /Users/silviapagliarini/Documents/Datasets/InitialDatasets/singleVoc/single_vocalizations/'
        #name = './build/progsrc/smilextract/SMILExtract -C config/mfcc/MFCC12_0_D_A.conf -I /Users/silviapagliarini/Documents/Datasets/completeDataset/'
        name = './build/progsrc/smilextract/SMILExtract -C config/mfcc/MFCC12_0_D_A.conf -I /Users/silviapagliarini/Documents/Datasets/subsetSilence/'
        #name = './build/progsrc/smilextract/SMILExtract -C config/mfcc/MFCC12_0_D_A.conf -I /Users/silviapagliarini/Documents/Datasets/HumanLabels/exp1'
        #name = './build/progsrc/smilextract/SMILExtract -C config/mfcc/MFCC12_0_D_A.conf -I /Users/silviapagliarini/Documents/BabbleNN/interspeech_Wave'

        if baby_id == 'AnneModel':
            f.write(name + '/' + os.path.basename(data[i]) + ' -csvoutput ' + os.path.basename(data[i])[0:-3] + 'mfcc.csv')
            f.write('\n')

        else:
            #output_dir = '/Users/silviapagliarini/Documents/opensmile/HumanData_analysis/humanVSlena/human'
            #output_dir = '/Users/silviapagliarini/Documents/opensmile/HumanData_analysis/completeDataset'
            output_dir = '/Users/silviapagliarini/Documents/opensmile/HumanData_analysis/subsetSilence'
            os.makedirs(output_dir + '/' + baby_id, exist_ok=True)
            for c in range(0,len(classes)):
                os.makedirs(output_dir + '/' + baby_id + '/' + classes[c], exist_ok=True)
            f.write(name + baby_id[0:4] + '/' + baby_id + '_segments/' + os.path.basename(data[i]) + ' -csvoutput ' + output_dir + '/' + baby_id + '/' + os.path.basename(data[i])[0:-3] + 'mfcc.csv')
            #f.write(name + '/' + baby_id + '_segments/' + os.path.basename(data[i]) + ' -csvoutput ' + output_dir + '/' + baby_id + '/' + os.path.basename(data[i])[0:-3] + 'mfcc.csv')
            f.write('\n')
        i = i + 1
    f.close()

    if args.labels_creation == True:
        # writing the data rows
        labels = []
        i = 0
        while i < len(data):
            j = 0
            while j < len(classes):
                if os.path.basename(data[i]).find(classes[j]) != -1:
                    labels.append(classes[j])
                j = j + 1
            i = i + 1

        with open(args.data_dir + '/' + 'LENAlabels_' + baby_id + '.csv', 'w') as csvfile:
            # creating a csv writer object
            csvwriter = csv.writer(csvfile)

            # writing the fields
            csvwriter.writerow(['ID', 'Label'])

            i = 0
            while i < len(data):
                csvwriter.writerow([str(i), labels[i]])
                i = i + 1

    print('Done')

def list(args):
    """
    Create a list of all the babies in the dataset in order to simplify the following steps of the analysis.

    INPUT
    - path to directory (subdirectories should be the single family directories).

    OUTPUT
    - .csv file with name of the baby and age of the baby in days.
    """
    listDir = glob2.glob(args.data_dir + '/0*')
    with open(args.data_dir + '/baby_list_basic.csv', 'w') as csvfile:
        # creating a csv writer object
        csvwriter = csv.writer(csvfile)

        # writing the fields
        csvwriter.writerow(['ID', 'AGE'])

        i = 0
        while i<len(listDir):
            name = os.path.basename(listDir[i])
            age = int(name[6])*365 + int(name[8]) * 30 + int(name[10])
            csvwriter.writerow([name, age])
            i = i + 1

    print('Done')

def merge_labels(babies, args):
    """
    Create a LENA-like .csv with the human corrections included. When a label has been identified as wrong, it is substitute with the
    noise lable NOF.

    INPUT
    - path to directory
    - list of babies

    OUTPUT
    .csv file containing cleaned labels.
    """
    for i in range(0,len(babies)):
        print(babies[i])
        lena = pd.read_csv(args.data_dir + '/' + babies[i] + '_segments.csv')
        human = pd.read_csv(args.data_dir + '/' + babies[i] + '_scrubbed_CHNrelabel_lplf_1.csv')
        time_stamp_lena_start = lena["startsec"]
        time_stamp_lena_end = lena["endsec"]
        prominence = human["targetChildProminence"]
        lena_labels = lena["segtype"]

        CHNSP_pos = np.where(lena_labels == 'CHNSP')[0]
        CHNNSP_pos = np.where(lena_labels == 'CHNNSP')[0]
        pos = np.append(CHNSP_pos, CHNNSP_pos)
        pos = sorted(pos)

        for j in range(0, len(pos)):
            if i < 2:
                if prominence[j] > 2:
                    lena_labels[pos[j]] = 'NOF'
            else:
                if prominence[j] == False:
                    lena_labels[pos[j]] = 'NOF'

        with open(args.data_dir + '/new_' + babies[i] + '_segments.csv', 'w') as csvfile:
            # creating a csv writer object
            csvwriter = csv.writer(csvfile)

            # writing the fields
            csvwriter.writerow(['segtype', 'startsec', 'endsec'])

            i = 0
            while i < len(time_stamp_lena_start):
                csvwriter.writerow([lena_labels[i], time_stamp_lena_start[i], time_stamp_lena_end[i]])
                i = i + 1

    print('Done')

def len_wav(data, args):
    """
    Compute the duration of all the vocalizations and give the average, max, min as an output.
    """
    duration = np.zeros((np.size(data),))
    for i in range(0, len(data)):
        #read the syllable
        or_rec = AudioSegment.from_wav(data[i])
        duration[i] = np.round(1000 * np.size(or_rec.get_array_of_samples()) / args.sr)

    mean_dur = np.mean(duration)
    max_dur = np.max(duration)
    min_dur = np.min(duration)

    print("Mean: ", mean_dur)
    print("Min: ", min_dur)
    print("Max: ", max_dur)
    print('Done')

if __name__ == '__main__':
    import argparse
    import glob2
    import sys

    parser = argparse.ArgumentParser()
    parser.add_argument('--option', type=str, choices=['merge', 'list', 'executeOS', 'duration'])
    parser.add_argument('--data_dir', type=str)
    parser.add_argument('--output_dir', type=str)
    parser.add_argument('--baby_id', type = str)
    parser.add_argument('--labels_creation', type = bool, default=False)

    uniform_duration_args = parser.add_argument_group('Uniform')
    uniform_duration_args.add_argument('--sd', type=int,
                                       help='Expected sound duration in milliseconds', default = 1000)
    uniform_duration_args.add_argument('--sr', type=int, help='Expected sampling rate',
                                       default=16000)

    args = parser.parse_args()

    if args.output_dir != None:
        if not os.path.isdir(args.data_dir + '/' + args.output_dir):
            os.makedirs(args.data_dir + '/' + args.output_dir)

    if args.option == 'executeOS':
        # Labels (change only if needed)
        # classes = ['B', 'S', 'N', 'MS', 'ME', 'M', 'OAS', 'SLEEP']
        classes = ['MAN', 'FAN', 'CHNSP', 'CHNNSP']
        #classes = ['CHNNSP']

        if args.baby_id == 'initial':
            # List of babies
            summary = pd.read_csv(args.data_dir + '/' + 'baby_list.csv')
            summary = pd.DataFrame.to_numpy(summary)
            babies = summary[:,0]
            # Load data
            if len(babies) == 0:
                baby_id = args.baby_id
                dataset = sorted(glob2.glob(args.data_dir + '/' + baby_id + '/' + '*.wav'))

                # Labels (change only if needed)
                #classes = ['B', 'S', 'N', 'MS', 'ME', 'M', 'OAS', 'SLEEP']
                #classes = ['FAN', 'CHNSP']

                opensmile_executable(dataset, baby_id, classes, args)

            else:
                for i in range(0, len(babies)):
                    dataset = sorted(glob2.glob(args.data_dir + '/' + babies[i] + '_segments' + '/' + '*.wav'))
                    print(dataset)
                    input()

                    opensmile_executable(dataset, babies[i], classes, args)

        elif args.baby_id == 'complete':
            # List of babies
            babies_dir = glob2.glob(args.data_dir + '/0*')
            for i in range(0, len(babies_dir)):
                babies_wav = glob2.glob(babies_dir[i] + '/*.wav')
                babies = []
                for j in range(0, len(babies_wav)):
                    babies = os.path.basename(babies_wav[j][0:-4])
                    dataset = sorted(glob2.glob(babies_dir[i] + '/' + babies + '_segments' + '/' + '*.wav'))

                    opensmile_executable(dataset, babies, classes, args)

        elif args.baby_id == 'subset':
            # List of babies
            summary = pd.read_csv(args.data_dir + '/' + 'baby_list.csv')
            summary = pd.DataFrame.to_numpy(summary)
            babies = summary[:, 0]
            for j in range(0, len(babies)):
                dataset = sorted(glob2.glob(args.data_dir + '/' + babies[j][0:4] + '/' + babies[j] + '_segments' + '/' + '*.wav'))
                opensmile_executable(dataset, babies[j], classes, args)

        else:
            dataset = sorted(glob2.glob(args.data_dir + '/' + args.baby_id + '_segments/' + '*.wav'))
            opensmile_executable(dataset, args.baby_id, classes, args)

    if args.option == 'list':
        list(args)

    if args.option == 'merge':
        babies_csv = pd.read_csv(args.data_dir + '/baby_list.csv')
        babies = babies_csv["name"]
        merge_labels(babies, args)

    if args.option == 'duration':
        # List of babies
        summary = pd.read_csv(args.data_dir + '/' + 'baby_list_basicONLYallAGES.csv')
        summary = pd.DataFrame.to_numpy(summary)
        babies = summary[:, 0]

        dataset = []
        for j in range(0, len(babies)):
            dataset_aux = sorted(
                glob2.glob(args.wav_dir + '/' + babies[j][0:4] + '/' + babies[j] + '_segments' + '/' + '*.wav'))
            dataset.append(dataset_aux)

        len_wav(dataset[0], args)

    ### Example: python3 BabyExperience.py --data_dir /Users/labadmin/Documents/Silvia/HumanData --option list