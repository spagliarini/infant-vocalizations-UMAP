#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: silviapagliarini
"""
import os
import numpy as np
import xlsxwriter
import pandas as pd
import csv

def modal(babies, judges_list_name, args):
    """
    For a given recording, compute the modal value across listeners.

    Output:
    For each baby, a table containing:
    - the vocalization list (start and end times)
    - lena label for the vocalization
    - the prominences of all the judges for each vocalization
    - the modal value for each vocalization
    - the label chosen depending on the modal value as the "average label"
    - Lena-like file containing all the labels (re-labeled infants and others)
    """
    for b in range(0, len(babies)):
        print(babies[b])
        # Prepare how many vocalizations in the recording
        n_test_table = pd.read_csv(args.data_dir + '/' + babies[b] + '_scrubbed_CHNrelabel_' + judges_list_name[1] + '_1.csv')
        n_test = len(n_test_table["startSeconds"])
        n_test_start = n_test_table["startSeconds"]
        n_test_end = n_test_table["endSeconds"]

        # Lena labels
        lena = pd.read_csv(args.data_dir + '/' + babies[b] + '_segments.csv')
        lena_labels = lena["segtype"]
        lena_startsec = lena["startsec"]
        lena_endsec = lena["endsec"]
        CHNSP_pos = np.where(lena_labels == 'CHNSP')[0]
        CHNNSP_pos = np.where(lena_labels == 'CHNNSP')[0]
        pos = np.append(CHNSP_pos, CHNNSP_pos)
        pos = sorted(pos)

        # Prominence assigned by the listeners
        prominence = np.zeros((len(judges_list_name), n_test))
        for j in range(0, len(judges_list_name)):
            human_table = pd.read_csv(args.data_dir + '/' + babies[b] + '_scrubbed_CHNrelabel_' + judges_list_name[j] + '_1.csv')
            human = pd.DataFrame.to_numpy(human_table)
            prominence_value = human[:, 2]
            prominence[j, :] = prominence_value

        # Modal value across listeners and average label
        modal_value = []
        avg_label = []
        for v in range(0, n_test):
            prominence_value_count = []
            for i in range(1, 6):
                prominence_value_count.append(len(np.where(prominence[:, v] == i)[0]))
            prominence_value_count = np.asarray(prominence_value_count)
            max_index_aux = np.where(prominence_value_count == prominence_value_count.max())[0]
            if len(max_index_aux)>1:
                max_index = np.max(max_index_aux)
            else:
                max_index = max_index_aux[0]
            modal_value.append(max_index)

            if max_index == 0:
                avg_label.append(lena_labels[pos[v]])
            else:
                avg_label.append('NOF')

        # Creation of the table
        workbook = xlsxwriter.Workbook(args.data_dir + '/' + babies[b] + '_modal_value.xlsx')
        worksheet = workbook.add_worksheet()

        # Define judges names on "x axis"
        row = 0
        worksheet.write(row, 0, 'voc_number')
        worksheet.write(row, 1, 'time_start')
        worksheet.write(row, 2, 'time_etart')
        worksheet.write(row, 3, 'lena')

        column = 4
        # Iterating through content list
        for item in judges_list_name:
            # write operation perform
            worksheet.write(row, column, item)
            # incrementing the value of row by one
            # with each iterations.
            column += 1
        worksheet.write(row, column, 'modal_value')
        column += 1
        worksheet.write(row, column, 'avg_label')

        row = 1
        # Define the list of vocalizations, time stamps and lena labels
        for item in range(0, n_test):
            worksheet.write(row, 0, item)
            worksheet.write(row, 1, n_test_start[item])
            worksheet.write(row, 2, n_test_end[item])
            worksheet.write(row, 3, lena_labels[pos[item]])
            row += 1

        #TODO: add all the prominence of the listeners to the table (white for now)

        row = 1
        column = 4 + len(judges_list_name)
        for item in modal_value:
            worksheet.write(row, column, item)
            row += 1

        row = 1
        column += 1
        for item in avg_label:
            worksheet.write(row, column, item)
            row += 1

        workbook.close()

        # Lena-like file containing all the labels (re-labeled infants and others)
        new_labels = lena_labels
        for i in range(0, len(pos)):
            new_labels[pos[i]] = avg_label[i]

        with open(args.data_dir + '/new_' + babies[b] + '_segments.csv', 'w') as csvfile:
            # creating a csv writer object
            csvwriter = csv.writer(csvfile)

            # writing the fields
            csvwriter.writerow(['segtype', 'startsec', 'endsec'])

            i = 0
            while i < len(lena_startsec):
                csvwriter.writerow([new_labels[i], lena_startsec[i], lena_endsec[i]])
                i = i + 1

    print('Done')

if __name__ == '__main__':
    import argparse
    import glob2
    import sys

    parser = argparse.ArgumentParser()
    parser.add_argument('--option', type=str, choices=['mod'])
    parser.add_argument('--data_dir', type=str)
    parser.add_argument('--output_dir', type=str)
    parser.add_argument('--baby_id', type = str)
    parser.add_argument('--judge', type=str)

    args = parser.parse_args()

    if args.output_dir != None:
        if not os.path.isdir(args.data_dir + '/' + args.output_dir):
            os.makedirs(args.data_dir + '/' + args.output_dir)

    if args.option == 'mod':
        # Read baby list
        babies_table = pd.read_csv(args.data_dir + '/' + 'babies_list.csv')
        babies = babies_table["name"]
        age = babies_table["age"]

        # Read judge list
        # judge_list_table = pd.read_csv(args.data_dir + '/' + args.baby_id + '_judges_list.csv')
        judge_list_table = pd.read_csv(args.data_dir + '/' + 'common_judges_list.csv')
        judges_list_code = judge_list_table["judge_code"]
        judges_list_name = judge_list_table["judge_name"]

        modal(babies, judges_list_name, args)
