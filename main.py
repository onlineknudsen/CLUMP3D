import cluster_config
import file_readers
import os
import preliminaries as prelim

###########################################################################
# markov chain monte carlo variables (ML)
# rescale number of iterations if desired (JS)
nMCMCTimes = 2
nMCMCTimesXSZ = 1
nMCMCTimesGL = 1
nBFTimesXSZ = 1
nSeedsTimes = 5*nMCMCTimes
###########################################################################

###########################################################################
# i'll put the 2D later (ML)
# do we want to run initial 2D fits to provide starting parameter values? (JS)
Run2DXSZ = True
Run2DGL = True
Run3D = True
Run3DExtension = False
RunPictures = True
###########################################################################

###########################################################################
# chi-square weight variables (ML)
# do we want to change weighting of different datasets in fit? (JS)
ChiSquareWeightXSZ = 1
ChiSquareWeightGL = 1
ChiSquareWeightSB1D = 1
ChiSquareWeightSB2D = 1
ChiSquareWeightT = 1
ChiSquareWeightSZ = 1
###########################################################################

###########################################################################
# what type of priors to use, and set some file extensions (JS)
SBMapString = "2D" # x-ray?
qPriorString = "flat" # matter shape priors, apparently the acceptable values are "flat";"nbody";"nbodyJS02";"flatICM";"spherical"
qICMPriorString = "none" # gas shape priors, values are "none","T"(TICM=TMat),"HE"(qICMi=qiphi),"HE+T"(TICM=TMat and qICM1=q1phi),"LS03"(approximated LS03)

# strings (ML)
GLProjStringToAddTmp1 = "example_v1.0"
XSZProjStringToAddTmp1 = "_example_v1.0" # "_v2017"
GLXSZ3DStringToAddTmp1 = "example_v1.0" # "";"_SaWLens";"_nPlanck";"_v01"

# additional sharp priors
SharpPriorsToAddString = "none" # defaults include "PUniv" "TUniv" and "none"

if qPriorString == "spherical":
    qPriorString = "sph"
else:
    qPriorString = "ell"

if XSZProjStringToAddTmp1 != "" and XSZProjStringToAddTmp1[0] != "_":
    XSZProjStringToAddTmp1 = "_" + XSZProjStringToAddTmp1

if GLXSZ3DStringToAddTmp1 != "" and GLXSZ3DStringToAddTmp1[0] != "_":
    GLXSZ3DStringToAddTmp1 = "_" + GLXSZ3DStringToAddTmp1
###########################################################################

###########################################################################
# cosmological parameters and units (JS)

# SEE preliminaries.py
# omegaM0v, hv, pull from astropy
# omegaLambda0v = 1 - omegaM0v

MUnits = 10**15 * prelim.MSunMKS / prelim.hv
rUnits = prelim.MpcMKS / prelim.hv
PUnits = prelim.G * MUnits**2 / rUnits**2   # G M^2 / (R * R^3)
###########################################################################

###########################################################################
# DATA SECTION

SZEDeltaThetaString = ""

# JS - define working directories and WL center position
ThetaInnerCut = 5./60 #(arcmin); (*M1206, §4.2 in umetsu+12*)
# TODO - some code from this section omitted; add it in (if relevant)

# "some conversion factors from observed quantities to physical quantities" -JS
nHeOnnH = 7.72 * 10**(-2) # primordial
mh, xeh, xh = (0.01, 0.005, 5*10**(-4)) # electrons from elements heavier than helium
nHOnnev = 1 / (1 + 2 * nHeOnnH + xeh)
muICMv = (1 + 4 * nHeOnnH + mh) / (2 + 3 * nHeOnnH + xeh + xh)
RhoGasOnnev = prelim.mu * (1 + 4 * nHeOnnH + mh) / (1 + 2 * nHeOnnH + xeh)
zLensingSourcesv = 20000 # reference redshift of WL background galaxies

DHv = lambda h: prelim.c / prelim.H0(h)


# read in weak lensing data from Umetsu + 2016
omegaM0v2 = 0.27
hv2 = 0.7
omegaLambda0v2 = 1 - omegaM0v2


# TODO - determine how this plays into the imprort process
cluster_configuration = cluster_config.ClusterConfig()
file_readers.import_cluster_info(cluster_configuration)
file_readers.import_gl_info(cluster_configuration)
file_readers.import_sz_info(cluster_configuration)
###########################################################################

wl_data = wl.wl_data(cluster_configuration)

# future goals: more data types, generic data import
