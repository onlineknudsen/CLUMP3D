import cluster_config as cc
from astropy.io import fits
import numpy as np
import os

def import_cluster_info(working_dir):
    with open(os.path.join(working_dir, "cluster_info.dat"), "r") as dat_file:
        i = 0
        for line in dat_file:
            line.strip()
            if line == "\n" or line[0] == "#" or line == "":
                continue
            else:
                if i == 0:
                    cc.redshift = float(line)

def import_sz_info(working_dir):
    with open(os.path.join(working_dir, "SZ", "sz_info.dat"), "r") as dat_file:
        i = 0
        for line in dat_file:
            line.strip()
            if line == "\n" or line[0] == "#" or line == "":
                continue
            else:
                if i == 0:
                    cc.frac_beam_smoothing = float(line)


def import_gl_info(working_dir):
    with open(os.path.join(working_dir, "GL", "gl_info.dat"), "r") as dat_file:
        theta_ev_sum = 0.0
        i = 0
        for line in dat_file:
            line.strip()
            if line == "\n" or line[0] == "#" or line == "":
                continue
            else:
                if i == 0:
                    cc.src_redshift = float(line)
                elif i == 1:
                    cc.nslv = float(line)
                elif i == 2:
                    cc.ra = float(line)
                elif i == 3:
                    cc.dec = float(line)
                else:
                    theta_ev_sum += float(line)
                i += 1
        cc.theta_ev = theta_ev_sum / 2

# trying to make FITS readers here

def import_gl_mean_info(working_dir):
    hdul = fits.open(os.path.join(working_dir, "GL", "U16", "MLE-E_WL.fits")) #saves it as an HDUList, will look up more documentation soon
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
            fits_data[index], # conversion factor
            0
        ])
        i += 1
    hdul.close()
    return data