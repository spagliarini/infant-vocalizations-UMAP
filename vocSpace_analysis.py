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

    labels = np.asarray(labels)
    agegroup = np.asarray(agegroup)

    legend_elements = []
    for l in range(0, len(classes)):
        legend_elements.append(
            Line2D([], [], marker='o', color=classes_colors[l], markersize=10, label=classes[l]))

    # UMAP
    mapper_sum = umap.UMAP(random_state=args.seed, spread=args.spread, n_neighbors=args.n_neigh, min_dist=args.min_d,
                           n_components=args.n_comp).fit(np.array(sum_mfcc_list))

    umap.plot.points(mapper_sum, np.asarray(labels), color_key_cmap="manual") #, background='black')
    plt.savefig(
        args.data_dir + '/' + 'ALLbabies_LENAlabels_' + '_opensmile_day_UMAP_mfcc_sum_' + str(
            args.n_neigh) + '_' + str(int(args.portion_size)) + '.pdf')
    plt.close('all')

    mapper_avg = umap.UMAP(random_state=args.seed, spread=args.spread, n_neighbors=args.n_neigh, min_dist=args.min_d,
                       n_components=args.n_comp).fit(np.array(average_mfcc_list))
    umap.plot.points(mapper_avg, np.asarray(labels), color_key_cmap='manual', background='black')
    plt.savefig(
        args.data_dir + '/' + 'ALLbabies_LENAlabels_' + '_opensmile_day_UMAP_mfcc_avg_' + str(
            args.n_neigh) + '_' + str(int(args.portion_size)) + '.pdf')
    plt.close('all')

    # t-SNE
    tsne_result = TSNE(n_components=2, learning_rate='auto', init='random').fit_transform(np.array(sum_mfcc_list))

    fig, ax = plt.subplots(figsize=(10, 10))
    ax.scatter(tsne_result[:, 0], tsne_result[:, 1], c=colors, s=0.1, alpha=0.8)
    ax.set_xlabel('t-SNE 1')
    ax.set_ylabel('y-SNE 2')
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.legend(handles=legend_elements)
    plt.savefig(args.data_dir + '/' + 'ALLbabies_LENAlabels_' + '_opensmile_day_TSNE_mfcc_sum_' + str(
        args.n_neigh) + '_' + str(int(args.portion_size)) + '.pdf')

    # PCA
    pca = PCA(n_components=4)
    pca_result = pca.fit_transform(np.array(sum_mfcc_list))

    fig, ax = plt.subplots(figsize=(10, 10))
    ax.scatter(pca_result[:,0], pca_result[:,1], c=colors, s=0.1, alpha=0.8)
    ax.set_xlabel('PCA 1')
    ax.set_ylabel('PCA 2')
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.legend(handles=legend_elements)
    plt.savefig(args.data_dir + '/' + 'ALLbabies_LENAlabels_' + '_opensmile_day_PCA_mfcc_sum_' + str(
            args.n_neigh) + '_' + str(int(args.portion_size)) + '.pdf')

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
    plt.savefig(args.data_dir + '/' + "_UMAPmanual.pdf")

    fig, ax = plt.subplots(figsize=(10, 10))
    ax.scatter(umapX, umapY, c=colors_age, cmap='age_manual', s=0.1, alpha=0.8)
    ax.set_xlim(np.min(umapX)-0.5, np.max(umapX)+0.5)
    ax.set_ylim(np.min(umapY)-0.5, np.max(umapY)+0.5)
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.xticks(fontsize=20)
    plt.yticks(fontsize=20)
    plt.legend(handles=legend_elements_age, fontsize=20)
    plt.savefig(args.data_dir + '/' + "_UMAPmanual_age.pdf")

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
    all_pca = np.zeros((len(pcaX),2))
    all_tsne = np.zeros((len(tsneX),2))
    all_umap[:,0] = umapX
    all_umap[:,1] = umapY
    all_pca[:,0] = pcaX
    all_pca[:,1] = pcaY
    all_tsne[:,0] = tsneX
    all_tsne[:,1] = tsneY

    # Group coordinates
    how_many = []
    all_umapX = []
    all_umapY = []
    all_umapXY = []
    all_pcaX = []
    all_pcaY = []
    all_pcaXY = []
    all_tsneX = []
    all_tsneY = []
    all_tsneXY = []
    all_filename = []
    ratio = []
    for i in range(0,len(classes)):
        # UMAP
        aux_X = umapX[np.where(labels==classes[i])[0]]
        aux_Y = umapY[np.where(labels==classes[i])[0]]
        how_many.append(len(aux_X))
        all_umapX.append(aux_X)
        all_umapY.append(aux_Y)
        all_XY_aux = np.zeros((len(aux_X),2))
        all_filename.append(time[np.where(labels==classes[i])[0]])
        for j in range(0, len(aux_X)):
            all_XY_aux[j,0] = aux_X[j]
            all_XY_aux[j,1] = aux_Y[j]
        ratio.append(len(aux_X)/len(umapX))
        all_umapXY.append(all_XY_aux)

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

    how_many = np.asarray(how_many)

    # centroids and entropy analysis
    centroids = []
    centroids_min_dist = np.zeros((len(classes), len(classes)))
    centroids_mean_dist = np.zeros((len(classes), len(classes)))
    centroids_PCA = []
    centroids_min_dist_PCA = np.zeros((len(classes), len(classes)))
    centroids_mean_dist_PCA = np.zeros((len(classes), len(classes)))
    centroids_tSNE = []
    centroids_min_dist_tSNE = np.zeros((len(classes), len(classes)))
    centroids_mean_dist_tSNE = np.zeros((len(classes), len(classes)))
    for i in range(0, len(classes)):
        # centroid
        centroid_UMAP = (sum(all_umapX[i]) / len(all_umapX[i]), sum(all_umapY[i]) / len(all_umapY[i]))
        centroids.append(centroid_UMAP)

        centroid_PCA = (sum(all_pcaX[i]) / len(all_pcaX[i]), sum(all_pcaY[i]) / len(all_pcaY[i]))
        centroids_PCA.append(centroid_PCA)

        centroid_tSNE = (sum(all_tsneX[i]) / len(all_tsneX[i]), sum(all_tsneY[i]) / len(all_tsneY[i]))
        centroids_tSNE.append(centroid_tSNE)

        # distance from the centroid
        for k in range(0,len(classes)):
            dist_UMAP = []
            dist_PCA = []
            dist_tSNE = []
            for j in range(0, len(all_umapX[k])):
                aux = np.array([all_umapX[k][j], all_umapY[k][j]])
                dist_aux_UMAP = np.linalg.norm(aux - centroid_UMAP)
                if dist_aux_UMAP != 0:
                    dist_UMAP.append(dist_aux_UMAP)

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
            centroids_min_dist_PCA[i, k] = np.min(dist_PCA)
            centroids_mean_dist_PCA[i, k] = np.mean(dist_PCA)
            centroids_min_dist_tSNE[i, k] = np.min(dist_tSNE)
            centroids_mean_dist_tSNE[i, k] = np.mean(dist_tSNE)

    # Across classes centroids distance
    centroids = np.asarray(centroids)
    centroids_PCA = np.asarray(centroids_PCA)
    centroids_tSNE = np.asarray(centroids_tSNE)
    dist_centroids = np.zeros((len(classes), len(classes)))
    for i in range(0, len(classes)):
        for j in range(0, len(classes)):
            dist_centroids[i,j] = np.linalg.norm(centroids[i]-centroids[j])

    dist_centroids_PCA = np.zeros((len(classes), len(classes)))
    for i in range(0, len(classes)):
        for j in range(0, len(classes)):
            dist_centroids_PCA[i,j] = np.linalg.norm(centroids_PCA[i]-centroids_PCA[j])

    dist_centroids_tSNE = np.zeros((len(classes), len(classes)))
    for i in range(0, len(classes)):
        for j in range(0, len(classes)):
            dist_centroids_tSNE[i,j] = np.linalg.norm(centroids_tSNE[i]-centroids_tSNE[j])

    # Save a dictionary with the quantities
    dataset_summary = {'how_many': how_many, 'entropy': entropy, 'cov_adultFANpre_babyCHNSP': cov_adultFANpre_babyCHNSP,
                       'cov_adultFANpost_babyCHNSP': cov_adultFANpost_babyCHNSP, 'cov_BBself': cov_babyCHNSP_self,
                       'cov_BBself_pre': cov_babyCHNSP_self_pre,
                       'corr_adultFANpre_babyCHNSP': corr_coeff_adultFANpre_babyCHNSP,
                       'corr_adultFANpost_babyCHNSP': corr_coeff_adultFANpost_babyCHNSP, 'corr_BBself': corr_coeff_baby_CHNSP_self,
                       'Dist_centr_UMAP': dist_centroids, 'centroids': centroids, 'centroid_mean_dis': centroids_mean_dist,
                       'Dist_centr_PCA': dist_centroids_PCA, 'centroids_PCA': centroids_PCA, 'centroid_mean_dis_PCA': centroids_mean_dist_PCA,
                       'Dist_centr_tSNE': dist_centroids_tSNE, 'centroids_tSNE': centroids_tSNE, 'centroid_mean_dis_tSNE': centroids_mean_dist_tSNE,
                       'Ratio': ratio, 'Wiener_entropy': wiener, 'Mean_pitch': mean_pitch, 'Min_pitch': min_pitch,
                       'Max_pitch': max_pitch, 'All_pitches': pitch, 'L2norm': L2norm}
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

    # List of colors (each color is a particular baby)
    colors = summary[:,2]
    for i in range(0, len(colors)):
        colors[i] = 'k'

    # class location
    for c in range(0, len(classes)):
        if classes[c] == 'CHNSP':
            pos_CHNSP = c
        elif classes[c] == 'FAN':
            pos_FAN = c

    # From stat summary
    ratio = []
    how_many_all = []
    how_many_classes = np.zeros((len(babies), len(classes)))
    how_many_CHNSP = []
    dist_CHNSP_FAN_UMAP = []
    dist_CHNSP_FAN_PCA = []
    dist_CHNSP_FAN_tSNE = []
    baby_ID = []
    agegroup = []
    centroid_CHNSP_self_UMAP = []
    centroid_CHNSP_self_PCA = []
    centroid_CHNSP_self_tSNE = []
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
        dist_CHNSP_FAN_UMAP.append(dataset_summary['Dist_centr_UMAP'][pos_CHNSP, pos_FAN])
        dist_CHNSP_FAN_PCA.append(dataset_summary['Dist_centr_PCA'][pos_CHNSP, pos_FAN])
        dist_CHNSP_FAN_tSNE.append(dataset_summary['Dist_centr_tSNE'][pos_CHNSP, pos_FAN])
        # Distance between CHNSP vocalizations and CHNSP centroid
        centroid_CHNSP_self_UMAP.append(dataset_summary['centroid_mean_dis'][pos_CHNSP, pos_CHNSP])
        centroid_CHNSP_self_PCA.append(dataset_summary['centroid_mean_dis_PCA'][pos_CHNSP, pos_CHNSP])
        centroid_CHNSP_self_tSNE.append(dataset_summary['centroid_mean_dis_tSNE'][pos_CHNSP, pos_CHNSP])

    how_many_all = np.asarray(how_many_all)
    how_many_CHNSP = np.asarray(how_many_CHNSP)
    baby_ID = np.asarray(baby_ID)
    agegroup = np.asarray(agegroup)
    dist_CHNSP_FAN_UMAP = np.asarray(dist_CHNSP_FAN_UMAP)
    centroid_CHNSP_self_UMAP = np.asarray(centroid_CHNSP_self_UMAP)

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
    summary_table['CHILDID'] = baby_ID
    summary_table['AGEGROUP'] = agegroup
    summary_table['CENTROID_CHSNP_FAN_UMAP'] = dist_CHNSP_FAN_UMAP
    summary_table['CENTROID_CHSNP_FAN_PCA'] = dist_CHNSP_FAN_PCA
    summary_table['CENTROID_CHSNP_FAN_tSNE'] = dist_CHNSP_FAN_tSNE
    summary_table['CENTROIDdist_CHSNPself_UMAP'] = centroid_CHNSP_self_UMAP
    summary_table['CENTROIDdist_CHSNPself_PCA'] = centroid_CHNSP_self_PCA
    summary_table['CENTROIDdist_CHSNPself_tSNE'] = centroid_CHNSP_self_tSNE
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

    # Read fit from R
    aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNSPcentroidSELF.Rdata')
    UMAP_fit_CHNSPselfCENTROID = aux['pred']
    aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNSP_FAN_centroid.Rdata')
    UMAP_fit_CHNSP_FAN_CENTROID = aux['pred']
    aux = readR.read_r(args.data_dir + '/' + 'PCA_CHNSPcentroidSELF.Rdata')
    PCA_fit_CHNSPselfCENTROID = aux['pred']
    aux = readR.read_r(args.data_dir + '/' + 'PCA_CHNSP_FAN_centroid.Rdata')
    PCA_fit_CHNSP_FAN_CENTROID = aux['pred']
    aux = readR.read_r(args.data_dir + '/' + 'tSNE_CHNSPcentroidSELF.Rdata')
    tSNE_fit_CHNSPselfCENTROID = aux['pred']
    aux = readR.read_r(args.data_dir + '/' + 'tSNE_CHNSP_FAN_centroid.Rdata')
    tSNE_fit_CHNSP_FAN_CENTROID = aux['pred']
    aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNSPselfPREMeanCOVARIANCE.Rdata')
    UMAP_CHNSPselfPREMeanCOVARIANCE = aux['pred']
    aux = readR.read_r(args.data_dir + '/' + 'UMAP_CHNSPselfMeanCOVARIANCE.Rdata')
    UMAP_CHNSPselfMeanCOVARIANCE = aux['pred']

    fig, ax = plt.subplots()
    for i in range(0, len(babies)):
        plt.plot(age[i], ratio[i][0], color=colors[i], marker='*')
    plt.savefig(args.data_dir + '/' + 'ratioCHNSP.pdf')

    fig, ax = plt.subplots()
    for i in range(0, len(babies)):
        plt.plot(age[i], ratio[i][1], color=colors[i], marker='*')
    plt.savefig(args.data_dir + '/' + 'ratioFAN.pdf')

    fig, ax = plt.subplots()
    for i in range(0, len(babies)):
        plt.plot(age[i], dist_CHNSP_FAN_UMAP[i], color=colors[i], marker='*')
        ax.set_xlabel('Age (in days)', fontsize=15)
        ax.set_ylabel('Distance between centroids', fontsize=15)
        #plt.legend(handles=legend_elements, ncol=2)
    plt.plot(sorted(age), UMAP_fit_CHNSP_FAN_CENTROID, 'k', lw=0.5)
    plt.savefig(args.data_dir + '/' + 'dist_CHNSP_FAN_UMAP.pdf')

    fig, ax = plt.subplots()
    for i in range(0, len(babies)):
        plt.plot(age[i], dist_CHNSP_FAN_PCA[i], color=colors[i], marker='*')
        ax.set_xlabel('Age (in days)', fontsize=15)
        ax.set_ylabel('Distance between centroids', fontsize=15)
        #plt.legend(handles=legend_elements, ncol=2)
    plt.plot(sorted(age), PCA_fit_CHNSP_FAN_CENTROID, 'k', lw=0.5)
    plt.savefig(args.data_dir + '/' + 'dist_CHNSP_FAN_PCA.pdf')

    fig, ax = plt.subplots()
    for i in range(0, len(babies)):
        plt.plot(age[i], dist_CHNSP_FAN_tSNE[i], color=colors[i], marker='*')
        ax.set_xlabel('Age (in days)', fontsize=15)
        ax.set_ylabel('Distance between centroids', fontsize=15)
        #plt.legend(handles=legend_elements, ncol=2)
    plt.plot(sorted(age), tSNE_fit_CHNSP_FAN_CENTROID, 'k', lw=0.5)
    plt.savefig(args.data_dir + '/' + 'dist_CHNSP_FAN_tSNE.pdf')

    plt.close('all')

    fig, ax = plt.subplots()
    for i in range(0, len(babies)):
        plt.plot(age[i], centroid_CHNSP_self_UMAP[i], color=colors[i], marker='*')
        ax.set_xlabel('Age (in days)', fontsize=15)
        ax.set_ylabel('Mean distance from the centroid', fontsize=15)
        #plt.legend(handles=legend_elements, ncol=2)
    plt.plot(sorted(age), UMAP_fit_CHNSPselfCENTROID, 'k', lw=0.5)
    plt.savefig(args.data_dir + '/' + 'MEAN_points_dist_CHNSP_self_UMAP.pdf')

    fig, ax = plt.subplots()
    for i in range(0, len(babies)):
        plt.plot(age[i], centroid_CHNSP_self_PCA[i], color=colors[i], marker='*')
        ax.set_xlabel('Age (in days)', fontsize=15)
        ax.set_ylabel('Mean distance from the centroid', fontsize=15)
        #plt.legend(handles=legend_elements, ncol=2)
    plt.plot(sorted(age), PCA_fit_CHNSPselfCENTROID, 'k', lw=0.5)
    plt.savefig(args.data_dir + '/' + 'MEAN_points_dist_CHNSP_self_PCA.pdf')

    fig, ax = plt.subplots()
    for i in range(0, len(babies)):
        plt.plot(age[i], centroid_CHNSP_self_tSNE[i], color=colors[i], marker='*')
        ax.set_xlabel('Age (in days)', fontsize=15)
        ax.set_ylabel('Mean distance from the centroid', fontsize=15)
        #plt.legend(handles=legend_elements, ncol=2)
    plt.plot(sorted(age), tSNE_fit_CHNSPselfCENTROID, 'k', lw=0.5)
    plt.savefig(args.data_dir + '/' + 'MEAN_points_dist_CHNSP_self_tSNE.pdf')

    plt.close('all')

    print('Done')

if __name__ == '__main__':
    import argparse
    import glob2
    import sys

    parser = argparse.ArgumentParser()
    parser.add_argument('--option', type=str, choices=['multidim', 'multidim_all', 'plot', 'stat', 'plot_stat_complete'])
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
        summary = pd.read_csv(args.data_dir + '/' + 'baby_list_basicONLYallAGES_18m.csv')
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

        #classes = ['CHNSP', 'FAN', 'MAN']  # ['CHNNSP', 'CHNSP', 'FAF', 'FAN', 'MAN']
        #classes = ['CHNSP', 'FAN']
        #classes = ['CHNNSP', 'CHNSP', 'FAN', 'MAN']
        classes = ['CHNSP', 'CHNNSP']
        for i in range(0,len(babies)):
            print(babies[i])
            if babies[i] != '0932_000602a':   #ICIS without '0583_000605':
                stat(classes, babies[i], args)

    if args.option == 'plot_stat_complete':
        #classes = ['CHNSP', 'FAN', 'MAN']
        #classes = ['CHNSP', 'FAN']
        classes = ['CHNSP', 'CHNNSP']
        plot_stat_complete(classes, args)

    ### Example: python3 BabyExperience.py --data_dir /Users/silviapagliarini/Documents/opensmile/HumanData_analysis --option plot