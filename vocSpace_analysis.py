#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 07 14:42:32 2021

@author: silviapagliarini
"""
import os
import random
import numpy as np
from matplotlib.lines import Line2D
import matplotlib.pyplot as plt
from sklearn.manifold import TSNE
from sklearn.decomposition import PCA
import umap
import umap.plot
import pandas as pd
import csv
import matplotlib
import librosa

csfont = {'fontname':'Times New Roman'}

# entropy
def entropy(hist):
    from math import log
    p = hist.flatten()
    p = p/np.sum(p)
    H = 0
    for h in range(len(p)):
        if p[h] != 0:
            H = H + p[h] * log(p[h], 2)

    return -H

def multidim(classes, baby, age, args):
    # Define colormap
    classes_colors = ['darkgreen', 'darkblue', 'red', 'gold']
    classes_cmap = matplotlib.colors.LinearSegmentedColormap.from_list("manual", classes_colors)
    plt.register_cmap("manual", classes_cmap)

    labels = []
    colors = []
    sum_mfcc_list = []
    basename = []
    babyname = []
    timename = []
    for c in range(0, len(classes)):
        data = glob2.glob(args.data_dir + '/' + baby + '*/' + classes[c] + '/' + '*.mfcc.csv')
        data = np.asarray(data)
        print(len(data))

        if args.portion == True:
            # select random subset
            randomlist = []
            for i in range(0, int(args.portion_size)):
                n = random.randint(0, len(data) - 1)
                randomlist.append(n)
            randomlist = np.asarray(randomlist)
            data = data[randomlist]

        how_many = len(data)

        i = 0
        while i < how_many:
            babyname.append(baby)
            basename_aux = os.path.basename(data[i])
            timename_aux = basename_aux[22:22 + 5]
            if timename_aux[-1] == '_':
                timename_aux = timename_aux[0:-1]
            elif timename_aux[-2] == '_':
                timename_aux = timename_aux[0:-2]
            elif timename_aux[-3] == '_':
                timename_aux = timename_aux[0:-3]

            if len(timename_aux) == 1:
                timename.append('0000' + timename_aux)
            elif len(timename_aux) == 2:
                timename.append('000' + timename_aux)
            elif len(timename_aux) == 3:
                timename.append('00' + timename_aux)
            elif len(timename_aux) == 4:
                timename.append('0' + timename_aux)
            else:
                timename.append(timename_aux)

            basename.append(basename_aux[0:-9])

            labels.append(classes[c])
            colors.append(classes_colors[c])

            # Load mfcc table
            mfcc_table = pd.read_csv(data[i], sep=';')
            mfcc_table = mfcc_table.to_numpy()

            sum_mfcc = 0
            k = 0
            while k < np.shape(mfcc_table)[0]:
                sum_mfcc = sum_mfcc + mfcc_table[k][2::]
                k = k + 1

            sum_mfcc_list.append(sum_mfcc)

            i = i + 1

    labels = np.asarray(labels)

    legend_elements = []
    for l in range(0, len(classes)):
        legend_elements.append(
            Line2D([], [], marker='o', color=classes_colors[l], markersize=10, label=classes[l]))

    # UMAP
    mapper_sum = umap.UMAP(random_state=args.seed, spread=args.spread, n_neighbors=args.n_neigh, min_dist=args.min_d,
                           n_components=args.n_comp).fit(np.array(sum_mfcc_list))

    umap.plot.points(mapper_sum, np.asarray(labels), color_key_cmap="manual")
    plt.savefig(
        args.data_dir + '/' + baby + '_LENAlabels_' + '_opensmile_day_UMAP_mfcc_sum_' + str(
            args.n_neigh) + '_' + str(int(args.portion_size)) + '.pdf')
    plt.close('all')

    # Create summary
    with open(args.data_dir + '/' + baby + '_summary_LENAlabels_' + '_opensmile_day_UMAP_mfcc.csv', 'w') as csvfile:
        # creating a csv writer object
        csvwriter = csv.writer(csvfile)

        # writing the fields
        csvwriter.writerow(['umap ID', 'filename', 'baby_ID', 'time ID', 'label', 'labelID', 'umap sum x', 'umap sum y', 'age'])

        i = 0
        while i < len(basename):
            csvwriter.writerow([i, basename[i], babyname[i], timename[i], labels[i], np.where(np.asarray(classes)==labels[i])[0][0], mapper_sum.embedding_[i][0], mapper_sum.embedding_[i][1], age])
            i = i + 1

    print('Done')


def multidim_all(classes, babies, age, args):
    # Define colormap
    classes_colors = ['darkgreen', 'darkblue', 'red', 'gold']
    classes_cmap = matplotlib.colors.LinearSegmentedColormap.from_list("manual", classes_colors)
    plt.register_cmap("manual", classes_cmap)

    # New list to avoid duplicate names (to join recordings)
    new_babies = []
    new_age = []
    for b in range(0, len(babies)):
        print(babies[b])
        if babies[b][-1] == 'a':
            new_babies.append(babies[b][0:-1])
            new_age.append(age[b])
        elif babies[b][-1] == 'b' or babies[b][-1] == 'c' or babies[b][-1] == 'd':
            pass
        else:
            new_babies.append(babies[b])
            new_age.append(age[b])

    with open(args.data_dir + '/' + 'new_baby_list.csv', 'w') as csvfile:
        # creating a csv writer object
        csvwriter = csv.writer(csvfile)

        # writing the fields
        csvwriter.writerow(['ID', 'AGE'])

        i = 0
        while i < len(new_babies):
            csvwriter.writerow([new_babies[i], new_age[i]])
            i = i + 1

    labels = []
    colors = []
    sum_mfcc_list = []
    average_mfcc_list = []
    basename = []
    babyname = []
    timename = []
    agegroup = []
    for b in range(0, len(new_babies)):
        for c in range(0, len(classes)):
            data = glob2.glob(args.data_dir + '/' + new_babies[b] + '*/' + classes[c] + '/' + '*.mfcc.csv')
            data = np.asarray(data)
            print(len(data))

            if args.portion == True:
                # select random subset
                randomlist = []
                for i in range(0, int(args.portion_size)):
                    n = random.randint(0, len(data)-1)
                    randomlist.append(n)
                randomlist = np.asarray(randomlist)
                data = data[randomlist]

            how_many = len(data)

            i = 0
            while i < how_many:
                babyname.append(new_babies[b])
                basename_aux = os.path.basename(data[i])
                timename_aux = basename_aux[22:22 + 5]
                if timename_aux[-1] == '_':
                    timename_aux = timename_aux[0:-1]
                elif timename_aux[-2] == '_':
                    timename_aux = timename_aux[0:-2]
                elif timename_aux[-3] == '_':
                    timename_aux = timename_aux[0:-3]

                if len(timename_aux) == 1:
                    timename.append('0000' + timename_aux)
                elif len(timename_aux) == 2:
                    timename.append('000' + timename_aux)
                elif len(timename_aux) == 3:
                    timename.append('00' + timename_aux)
                elif len(timename_aux) == 4:
                    timename.append('0' + timename_aux)
                else:
                    timename.append(timename_aux)

                basename.append(basename_aux[0:-9])

                if age[b] < 180:
                    agegroup.append('3mo')
                elif (age[b] >= 180 and age[b] < 250):
                    agegroup.append('6mo')
                elif (age[b] >= 250 and age[b] < 500):
                    agegroup.append('9mo')
                else:
                    agegroup.append('18mo')

                labels.append(classes[c])
                colors.append(classes_colors[c])

                # Load mfcc table
                mfcc_table = pd.read_csv(data[i], sep=';')
                mfcc_table = mfcc_table.to_numpy()

                sum_mfcc = 0
                k = 0
                while k < np.shape(mfcc_table)[0]:
                    sum_mfcc = sum_mfcc + mfcc_table[k][2::]
                    k = k + 1

                sum_mfcc_list.append(sum_mfcc)
                average_mfcc_list.append(sum_mfcc / len(sum_mfcc))

                i = i + 1

    element_mean = np.zeros((np.shape(sum_mfcc_list)[1],))
    element_std = np.zeros((np.shape(sum_mfcc_list)[1],))
    for j in range(0,np.shape(sum_mfcc_list)[1]):
        element_mean[j] = np.mean(np.asarray(sum_mfcc_list)[:, j])
        element_std[j] = np.std(np.asarray(sum_mfcc_list)[:, j])

    # Each sum minus the mean sum over each mfcc, velocity, acceleration
    sum_mfcc_list_aux = (np.asarray(sum_mfcc_list) - element_mean)
    # Each new sum divided by the std over each mfcc, velocity, acceleration
    new_sum_mfcc = np.asarray(sum_mfcc_list_aux)/element_std

    labels = np.asarray(labels)
    agegroup = np.asarray(agegroup)

    legend_elements = []
    for l in range(0, len(classes)):
        legend_elements.append(
            Line2D([], [], marker='o', color=classes_colors[l], markersize=10, label=classes[l]))

    # UMAP
    mapper_sum = umap.UMAP(random_state=args.seed, spread=args.spread, n_neighbors=args.n_neigh, min_dist=args.min_d,
                           n_components=args.n_comp).fit(np.array(new_sum_mfcc))

    umap.plot.points(mapper_sum, np.asarray(labels), color_key_cmap="manual") #, background='black')
    plt.savefig(
        args.data_dir + '/' + 'ALLbabies_LENAlabels_' + '_opensmile_day_UMAP_mfcc_sum_' + str(
            args.n_neigh) + '_' + str(int(args.portion_size)) + '.png')
    plt.close('all')

    mapper_avg = umap.UMAP(random_state=args.seed, spread=args.spread, n_neighbors=args.n_neigh, min_dist=args.min_d,
                       n_components=args.n_comp).fit(np.array(average_mfcc_list))
    umap.plot.points(mapper_avg, np.asarray(labels), color_key_cmap='manual') #, background='black')
    plt.savefig(
        args.data_dir + '/' + 'ALLbabies_LENAlabels_' + '_opensmile_day_UMAP_mfcc_avg_' + str(
            args.n_neigh) + '_' + str(int(args.portion_size)) + '.png')
    plt.close('all')

    # Create summary
    with open(args.data_dir + '/' + 'ALL_BABYADULT_summary_LENAlabels_' + '_opensmile_day_UMAP_mfcc.csv', 'w') as csvfile:
        # creating a csv writer object
        csvwriter = csv.writer(csvfile)

        # writing the fields
        csvwriter.writerow(['umap ID', 'filename', 'baby_ID', 'time ID', 'label', 'labelID', 'umap sum x', 'umap sum y', 'umap avg x', 'umap avg y', 'age'])

        i = 0
        while i < len(basename):
            csvwriter.writerow([i, basename[i], babyname[i], timename[i], labels[i], np.where(np.asarray(classes)==labels[i])[0][0], mapper_sum.embedding_[i][0], mapper_sum.embedding_[i][1], mapper_avg.embedding_[i][0], mapper_avg.embedding_[i][1], agegroup[i]])
            i = i + 1

    # t-SNE
    tsne_result = TSNE(n_components=2, learning_rate='auto', init='random').fit_transform(np.array(new_sum_mfcc))

    fig, ax = plt.subplots(figsize=(10, 10))
    ax.scatter(tsne_result[:, 0], tsne_result[:, 1], c=colors, s=0.1, alpha=0.8)
    ax.set_xlabel('t-SNE 1')
    ax.set_ylabel('y-SNE 2')
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.legend(handles=legend_elements)
    plt.savefig(args.data_dir + '/' + 'ALLbabies_LENAlabels_' + '_opensmile_day_TSNE_mfcc_sum_' + str(
        args.n_neigh) + '_' + str(int(args.portion_size)) + '.png')

    # PCA
    pca = PCA(n_components=4)
    pca_result = pca.fit_transform(np.array(new_sum_mfcc))

    fig, ax = plt.subplots(figsize=(10, 10))
    ax.scatter(pca_result[:,0], pca_result[:,1], c=colors, s=0.1, alpha=0.8)
    ax.set_xlabel('PCA 1')
    ax.set_ylabel('PCA 2')
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.legend(handles=legend_elements)
    plt.savefig(args.data_dir + '/' + 'ALLbabies_LENAlabels_' + '_opensmile_day_PCA_mfcc_sum_' + str(
            args.n_neigh) + '_' + str(int(args.portion_size)) + '.png')

    # Load data and create a single csv for each baby (to work better)
    summary = pd.read_csv(args.data_dir + '/' + 'ALL_BABYADULT_summary_LENAlabels_' + '_opensmile_day_UMAP_mfcc.csv')
    summary = pd.DataFrame.to_numpy(summary)

    for b in range(0, len(new_babies)):
        basename_aux = summary[:,1][np.where(summary[:,2]==new_babies[b])]
        timename_aux = summary[:,3][np.where(summary[:,2]==new_babies[b])]
        labels_aux = summary[:,4][np.where(summary[:,2]==new_babies[b])]
        labelsID_aux = summary[:,5][np.where(summary[:,2]==new_babies[b])]
        umapXsum = summary[:,6][np.where(summary[:,2]==new_babies[b])]
        umapYsum = summary[:,7][np.where(summary[:,2]==new_babies[b])]
        umapXavg = summary[:,8][np.where(summary[:,2]==new_babies[b])]
        umapYavg = summary[:,9][np.where(summary[:,2]==new_babies[b])]
        with open(args.data_dir + '/' + 'ALL_BABYADULTsummary_' + new_babies[
            b] + '_summary_LENAlabels_' + '_opensmile_day_UMAP_mfcc.csv',
                  'w') as csvfile:
            # creating a csv writer object
            csvwriter = csv.writer(csvfile)

            # writing the fields
            csvwriter.writerow(
                ['umap ID', 'filename', 'baby_ID', 'time ID', 'label', 'labelID', 'umap sum x', 'umap sum y',
                 'umap avg x', 'umap avg y', 'PCA 1', 'PCA 2', 'tSNE 1', 'tSNE 2'])

            i = 0
            while i < len(basename_aux):
                csvwriter.writerow([i, basename_aux[i], basename_aux[i][0:11], timename_aux[i], labels_aux[i],
                                   labelsID_aux[i], umapXsum[i], umapYsum[i], umapXavg[i], umapYavg[i], pca_result[i,0], pca_result[i,1],
                                    tsne_result[i,0], tsne_result[i,1]])
                i = i + 1

    plt.close('all')
    print('Done')

def my_plot(classes, args):
    # Define colormap
    #classes_colors = ['darkblue', 'red', 'gold']
    classes_colors = ['darkgreen', 'darkblue', 'red', 'gold']
    classes_cmap = matplotlib.colors.LinearSegmentedColormap.from_list("manual", classes_colors)
    plt.register_cmap("manual", classes_cmap)

    age_classes = ['3mo', '6mo', '9mo', '18mo', 'FAN-MAN']
    age_classes_colors = ['red', 'gold', 'navy', 'darkgreen', 'lightgray']
    age_classes_cmap = matplotlib.colors.LinearSegmentedColormap.from_list("age_manual", age_classes_colors)
    plt.register_cmap("age_manual", age_classes_cmap)

    legend_elements = []
    for l in range(0, len(classes)):
        legend_elements.append(
            Line2D([], [], marker='o', color=classes_colors[l], markersize=10, label=classes[l]))

    legend_elements_age = []
    for l in range(0, len(age_classes)):
        legend_elements_age.append(
            Line2D([], [], marker='o', color=age_classes_colors[l], markersize=10, label=age_classes[l]))

    # Load data
    summary = pd.read_csv(args.data_dir + '/' + 'ALL_BABYADULT_summary_LENAlabels_' + '_opensmile_day_UMAP_mfcc.csv')
    # Sort data frame
    summary = summary.sort_values("time ID", ignore_index=True)
    # Check order
    #print(summary["time ID"])
    #input()

    umapX = summary["umap sum x"]
    umapY = summary["umap sum y"]

    umapXavg = summary["umap avg x"]
    umapYavg = summary["umap avg y"]

    colors = []
    i = 0
    while i < len(umapX):
        j = 0
        while j < len(classes):
            if summary["label"][i] == classes[j]:
                colors.append(classes_colors[j])
            j = j + 1
        i = i + 1

    colors_age = ['gray' for i in range(0, len(umapX))]
    for i in range(0, len(umapX)):
        if summary["label"][i] == 'CHNSP':
            for k in range(0, len(age_classes) -1):
                if summary["age"][i] == age_classes[k]:
                    colors_age[i] = age_classes_colors[k]

    fig, ax = plt.subplots(figsize=(10, 10))
    ax.scatter(umapX, umapY, c=colors, cmap='manual', s=0.1, alpha=0.8)
    ax.set_xlim(np.min(umapX)-0.5, np.max(umapX)+0.5)
    ax.set_ylim(np.min(umapY)-0.5, np.max(umapY)+0.5)
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.xticks(fontsize=20)
    plt.yticks(fontsize=20)
    plt.legend(handles=legend_elements, fontsize=20)
    plt.savefig(args.data_dir + '/' + "_UMAPmanual.png")

    fig, ax = plt.subplots(figsize=(10, 10))
    ax.scatter(umapXavg, umapYavg, c=colors, cmap='manual', s=0.1, alpha=0.8)
    ax.set_xlim(np.min(umapXavg)-0.5, np.max(umapXavg)+0.5)
    ax.set_ylim(np.min(umapYavg)-0.5, np.max(umapYavg)+0.5)
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.xticks(fontsize=20)
    plt.yticks(fontsize=20)
    plt.legend(handles=legend_elements, fontsize=20)
    plt.savefig(args.data_dir + '/' + "_UMAPmanualAVG.png")

    fig, ax = plt.subplots(figsize=(10, 10))
    ax.scatter(umapX, umapY, c=colors_age, cmap='age_manual', s=0.1, alpha=0.8)
    ax.set_xlim(np.min(umapX)-0.5, np.max(umapX)+0.5)
    ax.set_ylim(np.min(umapY)-0.5, np.max(umapY)+0.5)
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.xticks(fontsize=20)
    plt.yticks(fontsize=20)
    plt.legend(handles=legend_elements_age, fontsize=20)
    plt.savefig(args.data_dir + '/' + "_UMAPmanual_age.png")

    # List of elements in the tail
    pos = np.where(umapX>5.3)[0]
    tail_X = np.asarray(umapX[pos])
    tail_Y = np.asarray(umapY[pos])
    umapID = np.asarray(summary["umap ID"][pos])
    basename = np.asarray(summary["filename"][pos])
    babyname = np.asarray(summary["baby_ID"][pos])
    timename = np.asarray(summary["time ID"][pos])
    labels = np.asarray(summary["label"][pos])
    labelsID = np.asarray(summary["labelID"][pos])
    agegroup = np.asarray(summary["age"][pos])

    # Create summary
    with open(args.data_dir + '/' + 'TailSounds_summary_LENAlabels_' + '_opensmile_day_UMAP_mfcc.csv',
              'w') as csvfile:
        # creating a csv writer object
        csvwriter = csv.writer(csvfile)

        # writing the fields
        csvwriter.writerow(
            ['umap ID', 'filename', 'baby_ID', 'time ID', 'label', 'labelID', 'umap sum x', 'umap sum y', 'age'])

        i = 0
        while i < len(basename):
            csvwriter.writerow(
                [umapID[i], basename[i], babyname[i], timename[i], labels[i], labelsID[i],
                 tail_X[i], tail_Y[i], agegroup[i]])
            i = i + 1

    print('Done')

def stat(classes, baby_id, args):
    # Load data
    if args.all == True:
        summary = pd.read_csv(
            args.data_dir + '/' + 'ALL_BABYADULTsummary_' + baby_id + '_summary_LENAlabels__opensmile_day_UMAP_mfcc.csv')
    else:
        summary = pd.read_csv(args.data_dir + '/' + 'BABYADULTsummary_' + baby_id +'_LENAlabels__opensmile_day_UMAP_mfcc.csv')
    # Sort data frame
    summary = summary.sort_values("time ID", ignore_index=True)
    # Check order
    #print(summary["time ID"])
    #input()

    summary = pd.DataFrame.to_numpy(summary)
    if args.all == True:
        time = summary[:, 2]
        labels = summary[:, 4]
        labelsID = summary[:, 5]
        umapX = summary[:, 6]
        umapY = summary[:, 7]
        umapXavg = summary[:, 8]
        umapYavg = summary[:, 9]
        pcaX = summary[:, 10]
        pcaY = summary[:, 11]
        tsneX = summary[:, 12]
        tsneY = summary[:, 13]
    else:
        time = summary[:,1]
        labels = summary[:,3]
        labelsID = summary[:,4]
        umapX = summary[:,5]
        umapY = summary[:,6]

    # All in an array
    all_umap = np.zeros((len(umapX),2))
    all_umapAVG = np.zeros((len(umapXavg),2))
    all_pca = np.zeros((len(pcaX),2))
    all_tsne = np.zeros((len(tsneX),2))
    all_umap[:,0] = umapX
    all_umap[:,1] = umapY
    all_umapAVG[:, 0] = umapXavg
    all_umapAVG[:, 1] = umapYavg
    all_pca[:,0] = pcaX
    all_pca[:,1] = pcaY
    all_tsne[:,0] = tsneX
    all_tsne[:,1] = tsneY

    # Group coordinates
    how_many = []
    all_umapX = []
    all_umapY = []
    all_umapXY = []
    all_umapXavg = []
    all_umapYavg = []
    all_umapXYavg = []
    all_pcaX = []
    all_pcaY = []
    all_pcaXY = []
    all_tsneX = []
    all_tsneY = []
    all_tsneXY = []
    all_filename = []
    ratio = []
    hist2d_UMAP = []
    hist2d_tSNE = []
    for i in range(0,len(classes)):
        all_filename.append(time[np.where(labels == classes[i])[0]])
        # UMAP
        aux_X = umapX[np.where(labels==classes[i])[0]]
        aux_Y = umapY[np.where(labels==classes[i])[0]]
        how_many.append(len(aux_X))
        all_umapX.append(aux_X)
        all_umapY.append(aux_Y)
        all_XY_aux = np.zeros((len(aux_X),2))
        for j in range(0, len(aux_X)):
            all_XY_aux[j,0] = aux_X[j]
            all_XY_aux[j,1] = aux_Y[j]
        ratio.append(len(aux_X)/len(umapX))
        all_umapXY.append(all_XY_aux)

        aux_Xavg = umapXavg[np.where(labels == classes[i])[0]]
        aux_Yavg = umapYavg[np.where(labels == classes[i])[0]]
        all_umapXavg.append(aux_Xavg)
        all_umapYavg.append(aux_Yavg)
        all_XYavg_aux = np.zeros((len(aux_Xavg), 2))
        for j in range(0, len(aux_Xavg)):
            all_XYavg_aux[j,0] = aux_Xavg[j]
            all_XYavg_aux[j,1] = aux_Yavg[j]
        all_umapXYavg.append(all_XYavg_aux)

        hist_aux, xedges, yedges = np.histogram2d(aux_X, aux_Y, bins=10)
        hist2d_UMAP.append(hist_aux)

        # PCA
        aux_X = pcaX[np.where(labels==classes[i])[0]]
        aux_Y = pcaY[np.where(labels==classes[i])[0]]
        all_pcaX.append(aux_X)
        all_pcaY.append(aux_Y)
        all_XY_aux = np.zeros((len(aux_X),2))
        for j in range(0, len(aux_X)):
            all_XY_aux[j,0] = aux_X[j]
            all_XY_aux[j,1] = aux_Y[j]
        all_pcaXY.append(all_XY_aux)

        # tSNE
        aux_X = tsneX[np.where(labels==classes[i])[0]]
        aux_Y = tsneY[np.where(labels==classes[i])[0]]
        all_tsneX.append(aux_X)
        all_tsneY.append(aux_Y)
        all_XY_aux = np.zeros((len(aux_X),2))
        for j in range(0, len(aux_X)):
            all_XY_aux[j,0] = aux_X[j]
            all_XY_aux[j,1] = aux_Y[j]
        all_tsneXY.append(all_XY_aux)

        hist_aux, xedges, yedges = np.histogram2d(aux_X, aux_Y, bins=10)
        hist2d_tSNE.append(hist_aux)

    hist2d_UMAP = np.asarray(hist2d_UMAP)
    hist2d_tSNE = np.asarray(hist2d_tSNE)
    how_many = np.asarray(how_many)

    # centroids and entropy analysis
    centroids = []
    centroids_min_dist = np.zeros((len(classes), len(classes)))
    centroids_mean_dist = np.zeros((len(classes), len(classes)))
    centroidsAVG = []
    centroids_min_distAVG = np.zeros((len(classes), len(classes)))
    centroids_mean_distAVG = np.zeros((len(classes), len(classes)))
    centroids_PCA = []
    centroids_min_dist_PCA = np.zeros((len(classes), len(classes)))
    centroids_mean_dist_PCA = np.zeros((len(classes), len(classes)))
    centroids_tSNE = []
    centroids_min_dist_tSNE = np.zeros((len(classes), len(classes)))
    centroids_mean_dist_tSNE = np.zeros((len(classes), len(classes)))
    entropy_UMAP = []
    entropy_tSNE = []
    for i in range(0, len(classes)):
        # centroid
        centroid_UMAP = (sum(all_umapX[i]) / len(all_umapX[i]), sum(all_umapY[i]) / len(all_umapY[i]))
        centroids.append(centroid_UMAP)

        centroid_UMAPavg = (sum(all_umapXavg[i]) / len(all_umapXavg[i]), sum(all_umapYavg[i]) / len(all_umapYavg[i]))
        centroidsAVG.append(centroid_UMAPavg)

        centroid_PCA = (sum(all_pcaX[i]) / len(all_pcaX[i]), sum(all_pcaY[i]) / len(all_pcaY[i]))
        centroids_PCA.append(centroid_PCA)

        centroid_tSNE = (sum(all_tsneX[i]) / len(all_tsneX[i]), sum(all_tsneY[i]) / len(all_tsneY[i]))
        centroids_tSNE.append(centroid_tSNE)

        # distance from the centroid
        for k in range(0,len(classes)):
            dist_UMAP = []
            dist_UMAPavg = []
            dist_PCA = []
            dist_tSNE = []
            for j in range(0, len(all_umapX[k])):
                aux = np.array([all_umapX[k][j], all_umapY[k][j]])
                dist_aux_UMAP = np.linalg.norm(aux - centroid_UMAP)
                if dist_aux_UMAP != 0:
                    dist_UMAP.append(dist_aux_UMAP)

                aux = np.array([all_umapXavg[k][j], all_umapYavg[k][j]])
                dist_aux_UMAPavg = np.linalg.norm(aux - centroid_UMAPavg)
                if dist_aux_UMAPavg != 0:
                    dist_UMAPavg.append(dist_aux_UMAPavg)

                aux = np.array([all_pcaX[k][j], all_pcaY[k][j]])
                dist_aux_PCA = np.linalg.norm(aux - centroid_PCA)
                if dist_aux_PCA != 0:
                    dist_PCA.append(dist_aux_PCA)

                aux = np.array([all_pcaX[k][j], all_pcaY[k][j]])
                dist_aux_tSNE = np.linalg.norm(aux - centroid_tSNE)
                if dist_aux_tSNE != 0:
                    dist_tSNE.append(dist_aux_tSNE)

            centroids_min_dist[i, k] = np.min(dist_UMAP)
            centroids_mean_dist[i, k] = np.mean(dist_UMAP)
            centroids_min_distAVG[i, k] = np.min(dist_UMAPavg)
            centroids_mean_distAVG[i, k] = np.mean(dist_UMAPavg)
            centroids_min_dist_PCA[i, k] = np.min(dist_PCA)
            centroids_mean_dist_PCA[i, k] = np.mean(dist_PCA)
            centroids_min_dist_tSNE[i, k] = np.min(dist_tSNE)
            centroids_mean_dist_tSNE[i, k] = np.mean(dist_tSNE)

        # entropy
        # UMAP
        H = entropy(hist2d_UMAP[i])
        entropy_UMAP.append(H)
        # tSNE
        H = entropy(hist2d_tSNE[i])
        entropy_tSNE.append(H)

    # Across classes centroids distance
    centroids = np.asarray(centroids)
    centroidsAVG = np.asarray(centroidsAVG)
    centroids_PCA = np.asarray(centroids_PCA)
    centroids_tSNE = np.asarray(centroids_tSNE)
    dist_centroids = np.zeros((len(classes), len(classes)))
    dist_centroidsAVG = np.zeros((len(classes), len(classes)))
    for i in range(0, len(classes)):
        for j in range(0, len(classes)):
            dist_centroids[i,j] = np.linalg.norm(centroids[i]-centroids[j])
            dist_centroidsAVG[i,j] = np.linalg.norm(centroidsAVG[i]-centroidsAVG[j])

    dist_centroids_PCA = np.zeros((len(classes), len(classes)))
    for i in range(0, len(classes)):
        for j in range(0, len(classes)):
            dist_centroids_PCA[i,j] = np.linalg.norm(centroids_PCA[i]-centroids_PCA[j])

    dist_centroids_tSNE = np.zeros((len(classes), len(classes)))
    for i in range(0, len(classes)):
        for j in range(0, len(classes)):
            dist_centroids_tSNE[i,j] = np.linalg.norm(centroids_tSNE[i]-centroids_tSNE[j])

    # Save a dictionary with the quantities
    dataset_summary = {'how_many': how_many, 'entropy_UMAP': entropy_UMAP, 'entropy_tSNE': entropy_tSNE,
                       'Dist_centr_UMAP': dist_centroids, 'centroids': centroids, 'centroid_mean_dis': centroids_mean_dist,
                       'Dist_centr_UMAPavg': dist_centroidsAVG, 'centroidsAVG': centroidsAVG,
                       'centroid_mean_disAVG': centroids_mean_distAVG,
                       'Dist_centr_PCA': dist_centroids_PCA, 'centroids_PCA': centroids_PCA, 'centroid_mean_dis_PCA': centroids_mean_dist_PCA,
                       'Dist_centr_tSNE': dist_centroids_tSNE, 'centroids_tSNE': centroids_tSNE, 'centroid_mean_dis_tSNE': centroids_mean_dist_tSNE,
                       'Ratio': ratio}
    np.save(args.data_dir + '/' + baby_id + '_stat_summary.npy', dataset_summary)

    print('Done')

def plot_stat_complete(classes, args):
    from matplotlib.lines import Line2D
    import pyreadr as readR

    # List of babies
    summary_table = pd.read_csv(args.data_dir + '/' + 'new_baby_list.csv')
    summary = pd.DataFrame.to_numpy(summary_table)
    babies = summary[:, 0]
    age = summary[:, 1]

    # List of colors
    colors = []
    for i in range(0, len(babies)):
        colors.append('k')

    # class location
    pos_CHNSP = -1
    pos_CHNNSP = -1
    pos_MAN = -1
    pos_FAN = -1
    for c in range(0, len(classes)):
        if classes[c] == 'CHNSP':
            pos_CHNSP = c
        elif classes[c] == 'FAN':
            pos_FAN = c
        elif classes[c] == 'MAN':
            pos_MAN = c
        elif classes[c] == 'CHNNSP':
            pos_CHNNSP = c

    # From stat summary
    entropy_UMAP_CHNSP = np.zeros((len(babies),))
    entropy_tSNE_CHNSP = np.zeros((len(babies),))
    entropy_UMAP_CHNNSP = np.zeros((len(babies),))
    entropy_tSNE_CHNNSP = np.zeros((len(babies),))
    ratio = []
    how_many_all = []
    how_many_classes = np.zeros((len(babies), len(classes)))
    how_many_CHNSP = []
    dist_CHNSP_FAN_UMAP = []
    dist_CHNSP_FAN_UMAPavg = []
    dist_CHNSP_FAN_PCA = []
    dist_CHNSP_FAN_tSNE = []
    dist_CHNNSP_FAN_UMAP = []
    dist_CHNNSP_FAN_PCA = []
    dist_CHNNSP_FAN_tSNE = []
    dist_CHNSP_MAN_UMAP = []
    dist_CHNSP_MAN_PCA = []
    dist_CHNSP_MAN_tSNE = []
    dist_CHNNSP_MAN_UMAP = []
    dist_CHNNSP_MAN_PCA = []
    dist_CHNNSP_MAN_tSNE = []
    dist_CHNNSP_CHNSP_UMAP = []
    baby_ID = []
    agegroup = []
    centroid_CHNSP_self_UMAP = []
    centroid_CHNSP_self_UMAPavg = []
    centroid_CHNSP_self_PCA = []
    centroid_CHNSP_self_tSNE = []
    centroid_CHNNSP_self_UMAP = []
    centroid_CHNNSP_self_PCA = []
    centroid_CHNNSP_self_tSNE = []
    for i in range(0, len(babies)):
        print(babies[i])

        baby_ID.append(babies[i][1:4])
        if age[i] < 180:
            agegroup.append('3mo')
        elif (age[i]>=180 and age[i]<250):
            agegroup.append('6mo')
        elif (age[i]>=250 and age[i]<500):
            agegroup.append('9mo')
        else:
            agegroup.append('18mo')

        dataset_summary = np.load(args.data_dir + '/' + babies[i] + '_stat_summary.npy', allow_pickle=True)
        dataset_summary = dataset_summary.item()
        ratio.append(dataset_summary['Ratio'])
        how_many_all.append(np.sum(dataset_summary['how_many']))
        how_many_classes[i,:] = dataset_summary['how_many']
        how_many_CHNSP.append(dataset_summary['how_many'][pos_CHNSP])
        # Centroids distance from CHNSP and FAN
        if pos_FAN != -1 and pos_CHNSP != -1:
            dist_CHNSP_FAN_UMAP.append(dataset_summary['Dist_centr_UMAP'][pos_CHNSP, pos_FAN])
            dist_CHNSP_FAN_UMAPavg.append(dataset_summary['Dist_centr_UMAPavg'][pos_CHNSP, pos_FAN])
            dist_CHNSP_FAN_PCA.append(dataset_summary['Dist_centr_PCA'][pos_CHNSP, pos_FAN])
            dist_CHNSP_FAN_tSNE.append(dataset_summary['Dist_centr_tSNE'][pos_CHNSP, pos_FAN])
        # Centroids distance from CHNNSP and FAN
        if pos_FAN != -1 and pos_CHNNSP != -1:
            dist_CHNNSP_FAN_UMAP.append(dataset_summary['Dist_centr_UMAP'][pos_CHNNSP, pos_FAN])
            dist_CHNNSP_FAN_PCA.append(dataset_summary['Dist_centr_PCA'][pos_CHNNSP, pos_FAN])
            dist_CHNNSP_FAN_tSNE.append(dataset_summary['Dist_centr_tSNE'][pos_CHNNSP, pos_FAN])
        # Centroids distance from CHNSP and MAN
        if pos_MAN != -1 and pos_CHNSP != -1:
            dist_CHNSP_MAN_UMAP.append(dataset_summary['Dist_centr_UMAP'][pos_CHNSP, pos_MAN])
            dist_CHNSP_MAN_PCA.append(dataset_summary['Dist_centr_PCA'][pos_CHNSP, pos_MAN])
            dist_CHNSP_MAN_tSNE.append(dataset_summary['Dist_centr_tSNE'][pos_CHNSP, pos_MAN])
        # Centroids distance from CHNNSP and FAN
        if pos_MAN != -1 and pos_CHNNSP != -1:
            dist_CHNNSP_MAN_UMAP.append(dataset_summary['Dist_centr_UMAP'][pos_CHNNSP, pos_FAN])
            dist_CHNNSP_MAN_PCA.append(dataset_summary['Dist_centr_PCA'][pos_CHNNSP, pos_FAN])
            dist_CHNNSP_MAN_tSNE.append(dataset_summary['Dist_centr_tSNE'][pos_CHNNSP, pos_FAN])
        # Distance between CHNSP vocalizations and CHNSP centroid + entropy of CHNSP vocalizations
        if pos_CHNSP != -1:
            centroid_CHNSP_self_UMAP.append(dataset_summary['centroid_mean_dis'][pos_CHNSP, pos_CHNSP])
            centroid_CHNSP_self_UMAPavg.append(dataset_summary['centroid_mean_disAVG'][pos_CHNSP, pos_CHNSP])
            centroid_CHNSP_self_PCA.append(dataset_summary['centroid_mean_dis_PCA'][pos_CHNSP, pos_CHNSP])
            centroid_CHNSP_self_tSNE.append(dataset_summary['centroid_mean_dis_tSNE'][pos_CHNSP, pos_CHNSP])
            entropy_UMAP_CHNSP[i] = dataset_summary['entropy_UMAP'][pos_CHNSP]
            entropy_tSNE_CHNSP[i] = dataset_summary['entropy_tSNE'][pos_CHNSP]
        # Distance between CHNNSP vocalizations and CHNNSP centroid
        if pos_CHNNSP != -1:
            centroid_CHNNSP_self_UMAP.append(dataset_summary['centroid_mean_dis'][pos_CHNNSP, pos_CHNNSP])
            centroid_CHNNSP_self_PCA.append(dataset_summary['centroid_mean_dis_PCA'][pos_CHNNSP, pos_CHNNSP])
            centroid_CHNNSP_self_tSNE.append(dataset_summary['centroid_mean_dis_tSNE'][pos_CHNNSP, pos_CHNNSP])
            entropy_UMAP_CHNNSP[i] = dataset_summary['entropy_UMAP'][pos_CHNNSP]
            entropy_tSNE_CHNNSP[i] = dataset_summary['entropy_tSNE'][pos_CHNNSP]
        # Centroids distance from CHNNSP and FAN
        if pos_CHNSP != -1 and pos_CHNNSP != -1:
            dist_CHNNSP_CHNSP_UMAP.append(dataset_summary['Dist_centr_UMAP'][pos_CHNNSP, pos_CHNSP])

    how_many_all = np.asarray(how_many_all)
    how_many_CHNSP = np.asarray(how_many_CHNSP)
    entropy_UMAP_CHNSP = np.asarray(entropy_UMAP_CHNSP)
    entropy_tSNE_CHNSP = np.asarray(entropy_tSNE_CHNSP)
    entropy_UMAP_CHNNSP = np.asarray(entropy_UMAP_CHNNSP)
    entropy_tSNE_CHNNSP = np.asarray(entropy_tSNE_CHNNSP)
    baby_ID = np.asarray(baby_ID)
    agegroup = np.asarray(agegroup)
    dist_CHNSP_FAN_UMAP = np.asarray(dist_CHNSP_FAN_UMAP)
    dist_CHNSP_FAN_UMAPavg= np.asarray(dist_CHNSP_FAN_UMAPavg)
    dist_CHNSP_MAN_UMAP = np.asarray(dist_CHNSP_MAN_UMAP)
    dist_CHNNSP_CHNSP_UMAP = np.asarray(dist_CHNNSP_CHNSP_UMAP)
    centroid_CHNSP_self_UMAP = np.asarray(centroid_CHNSP_self_UMAP)
    centroid_CHNSP_self_UMAPavg = np.asarray(centroid_CHNSP_self_UMAPavg)
    dist_CHNNSP_FAN_UMAP = np.asarray(dist_CHNNSP_FAN_UMAP)
    dist_CHNNSP_MAN_UMAP = np.asarray(dist_CHNNSP_MAN_UMAP)
    centroid_CHNNSP_self_UMAP = np.asarray(centroid_CHNNSP_self_UMAP)

    # Mean, median, std of the same age.
    ages = ['3mo', '6mo', '9mo', '18mo']
    mean_age = np.zeros((len(ages),2))
    median_age = np.zeros((len(ages),2))
    std_age = np.zeros((len(ages),2))
    j = 0
    while j<len(ages):
        mean_age[j,0] = np.mean(dist_CHNSP_FAN_UMAP[np.where(agegroup==ages[j])[0]])
        mean_age[j,1] = np.mean(centroid_CHNSP_self_UMAP[np.where(agegroup==ages[j])[0]])
        median_age[j,0] = np.median(dist_CHNSP_FAN_UMAP[np.where(agegroup==ages[j])[0]])
        median_age[j,1] = np.median(centroid_CHNSP_self_UMAP[np.where(agegroup==ages[j])[0]])
        std_age[j,0] = np.std(dist_CHNSP_FAN_UMAP[np.where(agegroup==ages[j])[0]])
        std_age[j,1] = np.std(centroid_CHNSP_self_UMAP[np.where(agegroup==ages[j])[0]])
        j = j+1

    summary_table['NUMALLVOC'] = how_many_all
    summary_table['NUMCHNSPVOC'] = how_many_CHNSP
    summary_table['CHNSPentropyUMAP'] = entropy_UMAP_CHNSP
    summary_table['CHNNSPentropyUMAP'] = entropy_UMAP_CHNNSP
    summary_table['CHNSPentropytSNE'] = entropy_tSNE_CHNSP
    summary_table['CHNNSPentropytSNE'] = entropy_tSNE_CHNNSP
    summary_table['CHILDID'] = baby_ID
    summary_table['AGEGROUP'] = agegroup
    summary_table['CENTROID_CHNSP_FAN_UMAP'] = dist_CHNSP_FAN_UMAP
    summary_table['CENTROID_CHNSP_FAN_UMAPavg'] = dist_CHNSP_FAN_UMAPavg
    summary_table['CENTROID_CHNSP_MAN_UMAP'] = dist_CHNSP_MAN_UMAP
    summary_table['CENTROID_CHNNSP_FAN_UMAP'] = dist_CHNNSP_FAN_UMAP
    summary_table['CENTROID_CHNNSP_MAN_UMAP'] = dist_CHNNSP_MAN_UMAP
    summary_table['CENTROID_CHNNSP_CHNSP_UMAP'] = dist_CHNNSP_CHNSP_UMAP
    summary_table['CENTROID_CHNSP_FAN_PCA'] = dist_CHNSP_FAN_PCA
    summary_table['CENTROID_CHNSP_FAN_tSNE'] = dist_CHNSP_FAN_tSNE
    summary_table['CENTROIDdist_CHNSPself_UMAP'] = centroid_CHNSP_self_UMAP
    summary_table['CENTROIDdist_CHNSPself_UMAPavg'] = centroid_CHNSP_self_UMAPavg
    summary_table['CENTROIDdist_CHNNSPself_UMAP'] = centroid_CHNNSP_self_UMAP
    summary_table['CENTROIDdist_CHNSPself_tSNE'] = centroid_CHNSP_self_tSNE
    summary_table['CENTROIDdist_CHNSPself_tSNE'] = centroid_CHNSP_self_tSNE
    summary_table.to_csv(args.data_dir + '/' + 'baby_list.csv')

    how_many_age = np.zeros((4, len(classes)))
    ages = ['3mo', '6mo', '9mo', '18mo']
    for i in range(0,len(ages)):
        how_many_age[i,:] = np.sum(how_many_classes[np.where(agegroup==ages[i])], axis=0)

    print('How many per class')
    print('Min')
    print(np.min(how_many_classes, axis=0))
    print('Sum')
    print(np.sum(how_many_classes, axis=0))
    print('Max')
    print(np.max(how_many_classes, axis=0))
    print('Mean')
    print(np.mean(how_many_classes, axis=0))
    input()

    print('How many per age')
    print(how_many_age)
    input()

    # Figures
    fig, ax = plt.subplots()
    for i in range(0, len(babies)):
        plt.plot(age[i], ratio[i][0], color=colors[i], marker='*')
    plt.savefig(args.data_dir + '/' + 'ratioCHNSP.pdf')

    fig, ax = plt.subplots()
    for i in range(0, len(babies)):
        plt.plot(age[i], ratio[i][1], color=colors[i], marker='*')
    plt.savefig(args.data_dir + '/' + 'ratioFAN.pdf')

    # Read fit from R and plot
    if pos_CHNSP != -1:
        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNSPcentroidSELF.Rdata')
        UMAP_fit_CHNSPselfCENTROID = aux['pred']
        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNSPcentroidSELFavg.Rdata')
        UMAP_fit_CHNSPselfCENTROIDavg = aux['pred']
        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNSPentropy.Rdata')
        UMAP_CHNSPentropy = aux['pred']
        aux = readR.read_r(args.data_dir + '/' + 'tSNE_CHNSPentropy.Rdata')
        tSNE_CHNSPentropy = aux['pred']

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], centroid_CHNSP_self_UMAP[i], color=colors[i], marker='*')
            ax.set_xlabel('Age (in days)', fontsize=15)
            ax.set_ylabel('Mean distance from the centroid', fontsize=15)
            # plt.legend(handles=legend_elements, ncol=2)
        plt.plot(sorted(age), UMAP_fit_CHNSPselfCENTROID, 'k', lw=0.5)
        plt.savefig(args.data_dir + '/' + 'MEAN_points_dist_CHNSP_self_UMAP.pdf')

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], centroid_CHNSP_self_UMAPavg[i], color=colors[i], marker='*')
            ax.set_xlabel('Age (in days)', fontsize=15)
            ax.set_ylabel('Mean distance from the centroid', fontsize=15)
            # plt.legend(handles=legend_elements, ncol=2)
        plt.plot(sorted(age), UMAP_fit_CHNSPselfCENTROIDavg, 'k', lw=0.5)
        plt.savefig(args.data_dir + '/' + 'MEAN_points_dist_CHNSP_self_UMAPavg.pdf')

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], entropy_UMAP_CHNSP[i], color=colors[i], marker='*')
        ax.set_xlabel('Age (in days)', **csfont, fontsize=18)
        ax.set_ylabel('Entropy', **csfont, fontsize=18)
        plt.plot(sorted(age), UMAP_CHNSPentropy, 'k', lw=0.5)
        # plt.legend(handles=legend_elements, ncol=2)
        plt.savefig(args.data_dir + '/' + 'entropy_CHNSP_UMAP.pdf')

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], entropy_tSNE_CHNSP[i], color=colors[i], marker='*')
        ax.set_xlabel('Age (in days)', **csfont, fontsize=18)
        ax.set_ylabel('Entropy', **csfont, fontsize=18)
        plt.plot(sorted(age), tSNE_CHNSPentropy, 'k', lw=0.5)
        # plt.legend(handles=legend_elements, ncol=2)
        plt.savefig(args.data_dir + '/' + 'entropy_CHNSP_tSNE.pdf')

        plt.close('all')

    if pos_CHNNSP != -1:
        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNNSPcentroidSELF.Rdata')
        UMAP_fit_CHNNSPselfCENTROID = aux['pred']
        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNNSPentropy.Rdata')
        UMAP_CHNNSPentropy = aux['pred']
        aux = readR.read_r(args.data_dir + '/' + 'tSNE_CHNNSPentropy.Rdata')
        tSNE_CHNNSPentropy = aux['pred']

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], centroid_CHNNSP_self_UMAP[i], color=colors[i], marker='*')
            ax.set_xlabel('Age (in days)', fontsize=15)
            ax.set_ylabel('Mean distance from the centroid', fontsize=15)
            # plt.legend(handles=legend_elements, ncol=2)
        plt.plot(sorted(age), UMAP_fit_CHNNSPselfCENTROID, 'k', lw=0.5)
        plt.savefig(args.data_dir + '/' + 'MEAN_points_dist_CHNNSP_self_UMAP.pdf')

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], entropy_UMAP_CHNNSP[i], color=colors[i], marker='*')
        ax.set_xlabel('Age (in days)', **csfont, fontsize=18)
        ax.set_ylabel('Entropy', **csfont, fontsize=18)
        plt.plot(sorted(age), UMAP_CHNNSPentropy, 'k', lw=0.5)
        # plt.legend(handles=legend_elements, ncol=2)
        plt.savefig(args.data_dir + '/' + 'entropy_CHNNSP_UMAP.pdf')

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], entropy_tSNE_CHNNSP[i], color=colors[i], marker='*')
        ax.set_xlabel('Age (in days)', **csfont, fontsize=18)
        ax.set_ylabel('Entropy', **csfont, fontsize=18)
        plt.plot(sorted(age), tSNE_CHNNSPentropy, 'k', lw=0.5)
        # plt.legend(handles=legend_elements, ncol=2)
        plt.savefig(args.data_dir + '/' + 'entropy_CHNNSP_tSNE.pdf')

    if pos_FAN != -1 and pos_CHNSP != -1:
        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNSP_FAN_centroid.Rdata')
        UMAP_fit_CHNSP_FAN_CENTROID = aux['pred']

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], dist_CHNSP_FAN_UMAP[i], color=colors[i], marker='*')
            ax.set_xlabel('Age (in days)', fontsize=15)
            ax.set_ylabel('Distance between centroids', fontsize=15)
            # plt.legend(handles=legend_elements, ncol=2)
        plt.plot(sorted(age), UMAP_fit_CHNSP_FAN_CENTROID, 'k', lw=0.5)
        plt.savefig(args.data_dir + '/' + 'dist_CHNSP_FAN_UMAP.pdf')

        plt.close('all')

        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNSP_FAN_centroidAVG.Rdata')
        UMAP_fit_CHNSP_FAN_CENTROIDavg = aux['pred']

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], dist_CHNSP_FAN_UMAPavg[i], color=colors[i], marker='*')
            ax.set_xlabel('Age (in days)', fontsize=15)
            ax.set_ylabel('Distance between centroids', fontsize=15)
            # plt.legend(handles=legend_elements, ncol=2)
        plt.plot(sorted(age), UMAP_fit_CHNSP_FAN_CENTROIDavg, 'k', lw=0.5)
        plt.savefig(args.data_dir + '/' + 'dist_CHNSP_FAN_UMAPavg.pdf')

    if pos_MAN != -1 and pos_CHNSP != -1:
        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNSP_MAN_centroid.Rdata')
        UMAP_fit_CHNSP_MAN_CENTROID = aux['pred']

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], dist_CHNSP_MAN_UMAP[i], color=colors[i], marker='*')
            ax.set_xlabel('Age (in days)', fontsize=15)
            ax.set_ylabel('Distance between centroids', fontsize=15)
            # plt.legend(handles=legend_elements, ncol=2)
        plt.plot(sorted(age), UMAP_fit_CHNSP_MAN_CENTROID, 'k', lw=0.5)
        plt.savefig(args.data_dir + '/' + 'dist_CHNSP_MAN_UMAP.pdf')

    if pos_FAN != -1 and pos_CHNNSP != -1:
        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNNSP_FAN_centroid.Rdata')
        UMAP_fit_CHNNSP_FAN_CENTROID = aux['pred']

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], dist_CHNNSP_FAN_UMAP[i], color=colors[i], marker='*')
            ax.set_xlabel('Age (in days)', fontsize=15)
            ax.set_ylabel('Distance between centroids', fontsize=15)
            # plt.legend(handles=legend_elements, ncol=2)
        plt.plot(sorted(age), UMAP_fit_CHNNSP_FAN_CENTROID, 'k', lw=0.5)
        plt.savefig(args.data_dir + '/' + 'dist_CHNNSP_FAN_UMAP.pdf')

    if pos_MAN != -1 and pos_CHNNSP != -1:
        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNNSP_FAN_centroid.Rdata')
        UMAP_fit_CHNNSP_MAN_CENTROID = aux['pred']

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], dist_CHNNSP_MAN_UMAP[i], color=colors[i], marker='*')
            ax.set_xlabel('Age (in days)', fontsize=15)
            ax.set_ylabel('Distance between centroids', fontsize=15)
            # plt.legend(handles=legend_elements, ncol=2)
        plt.plot(sorted(age), UMAP_fit_CHNNSP_MAN_CENTROID, 'k', lw=0.5)
        plt.savefig(args.data_dir + '/' + 'dist_CHNNSP_MAN_UMAP.pdf')

    if pos_CHNSP != -1 and pos_CHNNSP != -1:
        aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNNSP_CHNSP_centroid.Rdata')
        UMAP_fit_CHNNSP_CHNSP_CENTROID = aux['pred']

        fig, ax = plt.subplots()
        for i in range(0, len(babies)):
            plt.plot(age[i], dist_CHNNSP_CHNSP_UMAP[i], color=colors[i], marker='*')
            ax.set_xlabel('Age (in days)', fontsize=15)
            ax.set_ylabel('Distance between centroids', fontsize=15)
            # plt.legend(handles=legend_elements, ncol=2)
        plt.plot(sorted(age), UMAP_fit_CHNNSP_CHNSP_CENTROID, 'k', lw=0.5)
        plt.savefig(args.data_dir + '/' + 'dist_CHNNSP_CHNSP_UMAP.pdf')

    plt.close('all')

    print('Done')

def comparison(args):
    # Read data
    dataset_summary_L = pd.read_csv(args.data_dir + '/lena/' + 'baby_list.csv')
    dataset_summary_H = pd.read_csv(args.data_dir + '/human/' + 'baby_list.csv')

    lena_dist_CHNSP_FAN_UMAP = dataset_summary_L['CENTROID_CHSNP_FAN_UMAP']
    lena_centroid_CHNSP_self_UMAP = dataset_summary_L['CENTROIDdist_CHSNPself_UMAP']

    human_dist_CHNSP_FAN_UMAP = dataset_summary_H['CENTROID_CHSNP_FAN_UMAP']
    human_centroid_CHNSP_self_UMAP = dataset_summary_H['CENTROIDdist_CHSNPself_UMAP']

    fig, ax = plt.subplots()
    plt.scatter(human_dist_CHNSP_FAN_UMAP, lena_dist_CHNSP_FAN_UMAP, color='k', marker='*')
    lims = [np.min([ax.get_xlim(), ax.get_ylim()]),  np.max([ax.get_xlim(), ax.get_ylim()])]  # min/max of both axes
    ax.plot(lims, lims, 'k--', alpha=0.75, zorder=0)
    ax.set_xlabel('Manual labels', **csfont, fontsize=18)
    ax.set_ylabel('Lena labels', **csfont, fontsize=18)
    plt.savefig(args.data_dir + '/' + 'comparison_distCHNSPself.pdf')

    # Correlation
    corr = np.corrcoef(lena_centroid_CHNSP_self_UMAP, human_centroid_CHNSP_self_UMAP)
    print(corr)

    print('Done')

def single(classes, baby, args):
    # Define colormap
    classes_colors = ['darkgreen', 'darkblue', 'red', 'gold']
    classes_cmap = matplotlib.colors.LinearSegmentedColormap.from_list("manual", classes_colors)
    plt.register_cmap("manual", classes_cmap)

    labels = []
    colors = []
    basename = []
    sum_mfcc_list = []

    for c in range(0, len(classes)):
        data = glob2.glob(args.data_dir + '/' + baby + '*/' + classes[c] + '/' + '*.mfcc.csv')
        data = np.asarray(data)

        if args.portion == True:
            # select random subset
            randomlist = []
            for i in range(0, int(args.portion_size)):
                n = random.randint(0, len(data) - 1)
                randomlist.append(n)
            randomlist = np.asarray(randomlist)
            data = data[randomlist]

        how_many = len(data)

        i=0
        while i < how_many:
            labels.append(classes[c])
            colors.append(classes_colors[c])

            # Load mfcc table
            mfcc_table = pd.read_csv(data[i], sep=';')
            mfcc_table = mfcc_table.to_numpy()

            if i == 0:
                # plot spectrogram
                name = os.path.basename(data[i])
                path_spectro = '/Users/silviapagliarini/Documents/Datasets/HumanLabels/interspeech/' + name[0:11] + '_segments/' + name[0:-9] + '.wav'

                read_aux, sr = librosa.load(path_spectro, mono=True, sr=16000)

                X = librosa.stft(read_aux, n_fft=args.N, hop_length=args.H, win_length=args.N, window='hann',
                                 pad_mode='constant', center=True)
                T_coef = np.arange(X.shape[1]) * args.H / sr * 1000
                spectrogram_aux = np.log(1 + 100 * np.abs(X ** 2))
                K = args.N // 2
                F_coef = np.arange(K + 1) * sr / args.N

                plt.figure(figsize=(15, 3))
                extent = [T_coef[0], T_coef[-1], F_coef[0], F_coef[-1]]
                plt.imshow(spectrogram_aux, aspect='auto', cmap='inferno', origin='lower', extent=extent)
                plt.xlabel('Time (s)')
                plt.ylabel('Frequency (Hz)')
                plt.tight_layout()
                plt.savefig(args.data_dir + '/' + name[0:11] + '_test_spectrogram' + '.pdf')

                # plot MFCC
                mfcc = np.zeros((13, np.shape(mfcc_table)[0]))
                velocity  = np.zeros((13, np.shape(mfcc_table)[0]))
                acceleration = np.zeros((13, np.shape(mfcc_table)[0]))
                for k in range(0, np.shape(mfcc_table)[0]):
                    mfcc[:, k] = mfcc_table[k][2:15]
                    velocity[:, k] = mfcc_table[k][15:28]
                    acceleration[:, k] = mfcc_table[k][28:41]

                plt.figure(figsize=(15, 3))
                plt.imshow(np.array(mfcc), aspect='auto', cmap='inferno', origin='lower')
                plt.xlabel('Time (steps)')
                plt.ylabel('Frequency (Hz)')
                plt.tight_layout()
                plt.savefig(args.data_dir + '/' + name[0:11] + '_test_mfcc' + '.pdf')

                # plot 1st and 2nd derivatives
                plt.figure(figsize=(15, 3))
                plt.imshow(np.array(velocity), aspect='auto', cmap='inferno', origin='lower')
                plt.xlabel('Time (steps)')
                plt.ylabel('Frequency (Hz)')
                plt.tight_layout()
                plt.savefig(args.data_dir + '/' + name[0:11] + '_test_velocity' + '.pdf')

                plt.figure(figsize=(15, 3))
                plt.imshow(np.array(acceleration), aspect='auto', cmap='inferno', origin='lower')
                plt.xlabel('Time (steps)')
                plt.ylabel('Frequency (Hz)')
                plt.tight_layout()
                plt.savefig(args.data_dir + '/' + name[0:11] + '_test_acceleration' + '.pdf')

            sum_mfcc = 0
            k = 0
            while k < np.shape(mfcc_table)[0]:
                sum_mfcc = sum_mfcc + mfcc_table[k][2::]
                k = k + 1

            sum_mfcc_list.append(sum_mfcc)

            basename.append(os.path.basename(data[i]))

            i = i + 1


    element_mean = np.zeros((np.shape(sum_mfcc_list)[1],))
    element_std = np.zeros((np.shape(sum_mfcc_list)[1],))
    for j in range(0,np.shape(sum_mfcc_list)[1]):
        element_mean[j] = np.mean(np.asarray(sum_mfcc_list)[:, j])
        element_std[j] = np.std(np.asarray(sum_mfcc_list)[:, j])

    # Each sum minus the mean sum over each mfcc, velocity, acceleration
    sum_mfcc_list_aux = (np.asarray(sum_mfcc_list) - element_mean)
    # Each new sum divided by the std over each mfcc, velocity, acceleration
    new_sum_mfcc = np.asarray(sum_mfcc_list_aux)/element_std

    # Plot sum
    i = 0
    plt.figure()
    plt.plot(sum_mfcc_list[i])
    plt.tight_layout()
    plt.savefig(args.data_dir + '/' + baby + '_test_sum' + '.pdf')

    plt.figure()
    plt.plot(new_sum_mfcc[i])
    plt.tight_layout()
    plt.savefig(args.data_dir + '/' + baby + '_test_sum_zscore' + '.pdf')

    labels = np.asarray(labels)

    legend_elements = []
    for l in range(0, len(classes)):
        legend_elements.append(
            Line2D([], [], marker='o', color=classes_colors[l], markersize=10, label=classes[l]))

    # UMAP
    mapper_sum = umap.UMAP(random_state=args.seed, spread=args.spread, n_neighbors=args.n_neigh, min_dist=args.min_d,
                           n_components=args.n_comp).fit(np.array(sum_mfcc_list))

    umap.plot.points(mapper_sum, np.asarray(labels), color_key_cmap="manual")  # , background='black')
    plt.savefig(
        args.data_dir + '/' + 'baby_' + baby + '_opensmile_day_UMAP_mfcc_sum_' + str(
            args.n_neigh) + '_' + str(int(args.portion_size)) + '.pdf')
    plt.close('all')

    mapper_sum = umap.UMAP(random_state=args.seed, spread=args.spread, n_neighbors=args.n_neigh, min_dist=args.min_d,
                           n_components=args.n_comp).fit(np.array(new_sum_mfcc))

    umap.plot.points(mapper_sum, np.asarray(labels), color_key_cmap="manual")  # , background='black')
    plt.savefig(
        args.data_dir + '/' + 'baby_' + baby + '_opensmile_day_UMAP_mfcc_sum_zscore' + str(
            args.n_neigh) + '_' + str(int(args.portion_size)) + '.pdf')
    plt.close('all')

    umapX = []
    umapY = []
    for i in range(0, len(labels)):
        umapX.append(mapper_sum.embedding_[i][0])
        umapY.append(mapper_sum.embedding_[i][1])

    color_points = []
    umapX_points = []
    umapY_points = []
    for i in [0, 15, 1000, 500, 45, 34, 98, 56, 70, 150, 178, 182, 200, 678, 2050]:
        color_points.append('k')
        umapX_points.append(mapper_sum.embedding_[i][0])
        umapY_points.append(mapper_sum.embedding_[i][1])
        print(basename[i])
        print(umapX[i])
        print(umapY[i])

    n = range(1,len(umapX_points)+1)
    fig, ax = plt.subplots(figsize=(10, 10))
    ax.scatter(umapX, umapY, c=colors, cmap='manual', s=0.5)
    ax.scatter(umapX_points, umapY_points, c=color_points)

    for i, txt in enumerate(n):
        ax.annotate(txt, (umapX_points[i], umapY_points[i]),size=15)

    ax.set_xlim(np.min(umapX)-0.5, np.max(umapX)+0.5)
    ax.set_ylim(np.min(umapY)-0.5, np.max(umapY)+0.5)
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.xticks(fontsize=20)
    plt.yticks(fontsize=20)
    plt.legend(handles=legend_elements, fontsize=20)
    plt.savefig(
        args.data_dir + '/' + 'manual_baby_' + baby + '_opensmile_day_UMAP_mfcc_sumzscore_' + str(
            args.n_neigh) + '_' + str(int(args.portion_size)) + '.pdf')

    print('Done')

if __name__ == '__main__':
    import argparse
    import glob2
    import sys

    parser = argparse.ArgumentParser()
    parser.add_argument('--option', type=str, choices=['multidim', 'multidim_all', 'plot', 'stat', 'plot_stat_complete', 'comparison', 'single'])
    parser.add_argument('--data_dir', type=str)
    parser.add_argument('--output_dir', type=str)
    parser.add_argument('--baby_id', type=str)
    parser.add_argument('--audio_dir', type=str)

    UMAP_parameters_args = parser.add_argument_group('UMAP_params')
    UMAP_parameters_args.add_argument('--seed', type=int, default=42)
    UMAP_parameters_args.add_argument('--n_neigh', type=int,
                                      help='How much local is the topology. The smaller the more local')
    UMAP_parameters_args.add_argument('--min_d', type=float,
                                      help='How tightly UMAP is allowed to pack points together. Higher values provide more details. Range 0.0 to 0.99',
                                      default=0.1)
    UMAP_parameters_args.add_argument('--spread', type=float,
                                      help='Additional parameter to change when min_d is changed, it has to be such that spread >= min_d',
                                      default=0.1)
    UMAP_parameters_args.add_argument('--portion', type=bool,
                                      help='To visualize only a part of the day',
                                      default=False)
    UMAP_parameters_args.add_argument('--portion_size', type=float,
                                      help='How long we want to visualize, in hours or in number of sounds per class, depending on the function',
                                      default=0)
    UMAP_parameters_args.add_argument('--step', type=int, help='How many steps in one hour. For example, if slices of 1s, then 3600. If slices of 0.5s, then 7200',
                                      default=3600)
    UMAP_parameters_args.add_argument('--labels', type=str,
                                      help='Lables on/off, and which type',
                                      default='off', choices=['off', 'lena', 'my'])
    UMAP_parameters_args.add_argument('--n_comp', type=int, help='How many components', default=2)
    UMAP_parameters_args.add_argument('--all_labels', type=list, help='All the possible labels',
                                      default=['CHF', 'CHNNSP', 'CHNSP', 'CXF', 'CXN', 'FAF', 'FAN', 'MAF', 'MAN', 'NOF', 'NON', 'OLF', 'OLN', 'SIL', 'TVF', 'TVN'])

    comparison_args = parser.add_argument_group('comparison')
    comparison_args.add_argument('--all', type = bool, help='Are we considering all the babies in the same space?', default=False)
    comparison_args.add_argument('--R', type = bool, help='True if R fit curve is defined manually. Change coeffs.', default=True)

    read_sound_args = parser.add_argument_group('read_sound')
    read_sound_args.add_argument('--sampling_rate', type=int, default=16000)
    read_sound_args.add_argument('--N', type=int, help='Nfft spectrogram librosa', default=1024)
    read_sound_args.add_argument('--H', type=int, help='Hop length spectrogram librosa', default=64)

    args = parser.parse_args()

    if args.output_dir != None:
        if not os.path.isdir(args.data_dir + '/' + args.output_dir):
            os.makedirs(args.data_dir + '/' + args.output_dir)

    if args.option == 'multidim':
        # Classes
        classes  = ['CHNNSP', 'CHNSP', 'FAN', 'MAN']

        # List of babies
        summary = pd.read_csv(args.data_dir + '/' + 'new_baby_list.csv')
        summary = pd.DataFrame.to_numpy(summary)
        babies = summary[:, 0]
        age = summary[:, 1]

        for i in range(0, len(babies)):
            multidim(classes, babies[i], age[i], args)

    if args.option == 'multidim_all':
        # Classes
        # classes = ['CHNNSP', 'CHNSP', 'FAF', 'FAN', 'MAN', 'TVF', 'TVN']
        classes = ['CHNNSP', 'CHNSP', 'FAN', 'MAN']
        # classes = ['CHNSP', 'FAN', 'MAN']
        #classes = ['CHNSP', 'FAN']
        # classes = ['CHNSP', 'CHNNSP']
        # classes = ['CHNSP', 'CHNNSP', 'FAN']
        # classes = ['CHNSP']

        # List of babies
        summary = pd.read_csv(args.data_dir + '/' + 'baby_list_basicONLYallAGES.csv')
        #summary = pd.read_csv(args.data_dir + '/' + 'babies_list.csv')
        summary = pd.DataFrame.to_numpy(summary)
        babies = summary[:, 0]
        age = summary[:, 1]

        multidim_all(classes, babies, age, args)

    if args.option == 'plot':
        #classes = ['CHNSP', 'FAN', 'MAN']
        classes = ['CHNNSP', 'CHNSP', 'FAN', 'MAN']
        #classes = ['CHNSP', 'FAN']
        #classes = ['CHNNSP', 'CHNSP']
        #classes = ['CHNSP']
        my_plot(classes, args)

    if args.option == 'stat':
        # List of babies
        summary = pd.read_csv(args.data_dir + '/' + 'new_baby_list.csv')
        summary = pd.DataFrame.to_numpy(summary)
        babies = summary[:,0]

        #classes = ['CHNSP', 'FAN', 'MAN']
        classes = ['CHNNSP', 'CHNSP', 'FAN', 'MAN']
        #classes = ['CHNSP', 'FAN']
        #classes = ['CHNNSP', 'CHNSP', 'FAN', 'MAN']
        #classes = ['CHNSP', 'CHNNSP']
        for i in range(0,len(babies)):
            print(babies[i])
            if babies[i] != '0932_000602a':
                stat(classes, babies[i], args)

    if args.option == 'plot_stat_complete':
        #classes = ['CHNSP', 'FAN', 'MAN']
        classes = ['CHNNSP', 'CHNSP', 'FAN', 'MAN']
        #classes = ['CHNSP', 'FAN']
        #classes = ['CHNSP', 'CHNNSP']
        plot_stat_complete(classes, args)

    if args.option == 'comparison':
        comparison(args)

    if args.option == 'single':
        classes = ['CHNNSP', 'CHNSP', 'FAN', 'MAN']
        baby = '0437_010603'
        #baby = '0833_010606'
        single(classes, baby, args)

    ### Example: python3 BabyExperience.py --data_dir /Users/silviapagliarini/Documents/opensmile/HumanData_analysis --option plot