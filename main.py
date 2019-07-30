import os
# markov chain monte carlo variables (ML)
nMCMCTimes = 2
nMCMCTimesXSZ = 1
nMCMCTimesGL = 1
nBFTimesXSZ = 1
nSeedsTimes = 5*nMCMCTimes

# i'll put the 2D later (ML)
Run2DXSZ = True
Run2DGL = True
Run3D = True
Run3DExtension = False
RunPictures = True

# chi-square weight variables (ML)
ChiSquareWeightXSZ = 1
ChiSquareWeightGL = 1
ChiSquareWeightSB1D = 1
ChiSquareWeightSB2D = 1
ChiSquareWeightT = 1
ChiSquareWeightSZ = 1

SBMapString = "2D" # x-ray?
qPriorString = "flat" # matter shape priors, apparently the acceptable values are "flat";"nbody";"nbodyJS02";"flatICM";"spherical"
qICMPriorString = "none" # gas shape priors, values are "none","T"(TICM=TMat),"HE"(qICMi=qiphi),"HE+T"(TICM=TMat and qICM1=q1phi),"LS03"(approximated LS03)

# strings (ML)
GLProjStringToAddTmp1 = "example_v1.0"
XSZProjStringToAddTmp1 = "_example_v1.0" # "_v2017"
GLXSZ3DStringToAddTmp1 = "example_v1.0" # "";"_SaWLens";"_nPlanck";"_v01"

# additional sharp priors
SharpPriorsToAddString = "none" # defaults include "PUniv" "TUniv" and "none"

working_directory = os.path.join(os.getcwd(),"data") # be read in from a config file at some point 
print(os.getcwd())