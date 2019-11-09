from astropy.cosmology import WMAP9 as cosmo
from astropy.cosmology import FlatLambdaCDM
from astropy.constants import c, G
from math import pi
import numpy as np

z_src_infinity = 20000.0

def conversion_factor(cc):
    '''cc is a ClusterConfig object, defined in cluster_config.py'''
    alt_cosmo = FlatLambdaCDM(70, cc.wl_omega_m)

    dd1 = cosmo.angular_diameter_distance(cc.redshift)
    ds1 = cosmo.angular_diameter_distance(z_src_infinity)
    dds1 = cosmo.angular_diameter_distance_z1z2(cc.redshift, z_src_infinity)

    dd2 = alt_cosmo.angular_diameter_distance(cc.redshift)
    ds2 = alt_cosmo.angular_diameter_distance(cc.src_redshift)
    dds2 = alt_cosmo.angular_diameter_distance_z1z2(cc.redshift, cc.src_redshift)

    crit_surf_density_1 = lensing_crit_surf_density(ds1, dd1, dds1)
    crit_surf_density_2 = lensing_crit_surf_density(ds2, dd2, dds2)
    return crit_surf_density_2 / crit_surf_density_1

def lensing_crit_surf_density(ds, dd, dds):
    return (c * c * ds)/(4 * pi * G * dds * dd)

def import_wl_cov_matrix(cc):
    '''cc is a ClusterConfig object, defined in cluster_config.py'''
    path = os.path.join(cc.working_directory, "GL", "U16", "MLE-ECOV+LSS_WL.pack")
    return np.loadtxt(path, skiprows=1)

def import_wl_convergence_map(cc):
    '''cc is a ClusterConfig object, defined in cluster_config.py'''
    hdul = fits.open(os.path.join(cc.working_directory, "GL", "U16", "MLE-E_WL.fits")) #saves it as an HDUList, will look up more documentation soon
    #hdul.info() would give us some debugging opportunities here
    head = hdul[0].header
    pix_size_ra = head["CDELT1"] # pixel size in degrees, x-direction (RA)
    pix_size_dec = head["CDELT2"] # ' ', y-direction (declination)
    ref_ra = head["CRVAL1"] # RA (right ascension, spherical coordinate system) in degrees
    ref_dec = head["CRVAL2"] # declination, degrees, relative to a reference pixel
    n_pix_ra = head["NAXIS1"] # number of pixels, RA direction
    n_pix_dec = head["NAXIS2"] # number of pixels, declination direction
    ref_ra_pix = head["CRPIX1"] # RA pixel number of reference pixel, one-indexed
    ref_dec_pix = head["CRPIX2"] # declination pixel number of the reference pixel, one-indexed

    length = n_pix_ra * n_pix_dec

    fits_data = hdul[0].data[::-1] # astropy imports files reversed so this unreverses it
    data = np.empty((length, 6))
    i = 0
    for (index, value) in np.ndenumerate(fits_data):
        row = index[0] + 1
        col = index[1] + 1
        data[i] = np.array([col, row,
            col - ref_ra_pix * pix_size_ra + ref_ra,
            row - ref_dec_pix * pix_size_ra + ref_dec,
            fits_data[index],
            0
        ])
        i += 1
    hdul.close()
    return data

# map conversion factor to convergence map and cov mat
