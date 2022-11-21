# infant-vocalizations-UMAP

## Pre-processing
1. Extraction of single vocalizations based on the automatic LENA system (see https://github.com/HomeBankCode/lena-its-tools?):
* label creation (using segments.pl in the mentioned repository);
* MATLAB routine based on the labels and the whole recording (using getIndividualAudioSegments.m in the mentioned repository).

References:
> J. Gilkerson and J. A. Richards, “The lena natural language study,” Boulder, CO: LENA Foundation. Retrieved March, vol. 3, p. 2009, 2008.
 
> D. Xu, U. Yapanel, S. Gray, J. Gilkerson, J. Richards, and J. Hansen, “Signal processing for young child speech language development,” in First Workshop on Child, Computer and Interaction, 2008.

2. Extraction of MFCCs coefficients, velocity and acceleration using OpenSMILE. Resources and guidelines can be found at https://audeering.github.io/opensmile/. References:
> Florian Eyben, Martin Wöllmer, Björn Schuller: "openSMILE - The Munich Versatile and Fast Open-Source Audio Feature Extractor", Proc. ACM Multimedia (MM), ACM, Florence, Italy, ISBN 978-1-60558-933-6, pp. 1459-1462, 25.-29.10.2010.

3. Utils to run previous and following steps. The python file vocSpace_utils.py contains: 
* list: a function to create a list containing all the babies included in the study.
* executeOS: a function to create an automatic bash file useful to run the opensmile pre-processing.
* merge: a function needed for the validation part to merge the human re-labeled vocalization with the original automatic labels, and use the new labels to extract the vocalizations as in point 1. 

## Vocalizations space and statistical analysis
1. Compute the low-dimensional representation (UMPA, t-SNE, PCA). The python file vocSpace_analysis.py contains:
* multidim_all: function to compute UMAP and tSNE space from the MFCC coefficients collection. It creates summary tables (general and baby-wise with the coordinates of UMAP/tSNE space)
* my_plot: manual plot of UMAP space (Figure 1 in the paper)
* single: allows to represent one single recording and highlight random or selected points in the space. 

2. Compute statistical properties of the clusters across families and across time (age). The python file vocSpace_analysis.py contains:
* stat: function to compute the statistical quantities (e.g.: centorids, distance between centroids). It creates a summary table containing the statistical properties.
* plot_stat_complete: function to plot the desired quantites (e.g. Figure 2 and 3) and extract the data in Table 1. It also adds values to the summary tables with information regarding the centroids and statistical properties. This table is needed to run the following R analysis. Note: run once before R and again after R. 

3. The two R files contain the functions to fit the data. The output can be then loaded into plot_stat_complete to obtain figures and correlation measure.

4. Validation and modal value
In the Cohen_kappa.py code one can compute the modal_value, which allows to group all the listeners new labels and define an "average label".
Then, one can run all the pre-processing + analysis as described above.
