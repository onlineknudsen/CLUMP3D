# probably move this back to main later

import math

# SubMKS' table is as follows, we'll probably use astropy and scipy for the vast majority of this
s = 1
day = 86400
year = 3.15576*10**7
g = 10**-3
gr = g
cm = 10**-2
km = 10**3
pc = 3.08568*10**16
kpc = 1000*pc
Mpc = 1000*kpc
Gpc = 1000*Mpc
AU = 1.49598*10**11
G = 6.673*10**-11
c = 299792458
kB = 1.38065*10**-23
me = 9.10939*10**-31
mu = 1.66054*10**-27
mp = 1.67262*10**-27
# MSun already there
TCMB = 2.728
sigmaT = 6.65246*10**-29
hPlankConstant = 6.62608*10**-34
eV = 1.60218*10**-19
keV = 1000*eV
erg = 10**-7
Jy = 10**-26

# units that are referenced later on, probably pull them from astropy in the near future
MKS = 299792458
GMKS = 6.673*10**-11 #(*G/.SubMKS*);
MSunMKS = 1.989103*10**30 #(*MSun/.SubMKS*);
pcMKS = 3.08567802*10**16 #(*Mpc/.SubMKS/.SubMKS*);
kpcMKS = 10**3*pcMKS #(*Mpc/.SubMKS/.SubMKS*);
MpcMKS = 10**6*pcMKS #(*Mpc/.SubMKS/.SubMKS*);
omegaM0v = 1 - 0.7
deltaOmegaM0v = 0.016
omegaLambda0v = 0.7
hv = 0.7
deltahv = 0.014
omegaM0XXL = 1 - 0.72
hXXL = 0.70
omegaM0WMAP9 = 0.28
omegaLambda0WMAP9 = 0.72
hWMAP9 = 0.70
sigma8WMAP9 = 0.82
omegaM0PSZ = 0.3
hPSZ = 0.7
omegaM0Planck = 0.3175
sigma8Planck = 0.8344
hPlanck = 0.671
omegaLambda0Planck = 1 - omegaM0Planck

# this was commented out originally, dunno if it'll be important, i'm guessing not though (ML)
# {\[CapitalOmega]M0Tmp1 = \[CapitalOmega]M0XXL, \[CapitalOmega]\
# \[CapitalLambda]0Tmp1 = 1. - \[CapitalOmega]M0XXL, 
#  hTmp1 = hXXL}; {\[CapitalOmega]M0Tmp1 = \[CapitalOmega]M0PSZ, \
# \[CapitalOmega]\[CapitalLambda]0Tmp1 = 1 - \[CapitalOmega]M0PSZ, 
#  hTmp1 = hPSZ};
# {\[CapitalOmega]M0Tmp1 = \[CapitalOmega]M0XXL, hTmp1 = hXXL};
# {\[CapitalOmega]M0Tmp1 = \[CapitalOmega]M0PSZ, hTmp1 = hPSZ};
# {\[CapitalOmega]M0Tmp1 = 0.3, hTmp1 = 0.7};

def EzLambda(z, omegaM0, omegaLambda0):
    return math.sqrt((1+z)**3 * omegaM0 + (1+z)**2 * (1 - omegaM0 - omegaLambda0) + omegaLambda0)

# dunno what this line does but we'll figure it out soon: DInter = Interpolation[Table[{z, DistLambda[0,z,omegaM0Tmp1,omegaLambda0Tmp1]},{z,0,5,0.02}]]

DToCGSFac = c/(cm*3.24078*10**-18*0.7)
print(DToCGSFac)