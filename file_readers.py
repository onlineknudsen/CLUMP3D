import cluster_config as cc
from astropy.io import fits
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
                    cc.sze_sn = float(line)


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
    cc.mean_wl_cdelt1 = head["CDELT1"] # pixel size in degrees, x-direction (RA)
    cc.mean_wl_cdelt2 = head["CDELT2"] # ' ', y-direction (declination)
    cc.mean_wl_crval1 = head["CRVAL1"] # RA (right ascension, spherical coordinate system) in degrees
    cc.mean_wl_crval2 = head["CRVAL2"] # declination, degrees, relative to a reference pixel
    cc.mean_wl_naxis1 = head["NAXIS1"] # number of pixels, RA direction
    cc.mean_wl_naxis2 = head["NAXIS2"] # number of pixels, declination direction
    cc.mean_wl_crpix1 = head["CRPIX1"] # RA pixel number of reference pixel, one-indexed
    cc.mean_wl_crpix2 = head["CRPIX2"] # declination pixel number of the reference pixel, one-indexed

    # need to figure out what these lines do:
    '''kGridTmp1 = kTabFits[[2, 1]] // Chop;

        (*{0.,0} (physical coordinates) is the optical coordinates*)
        DataGridTabWL2 =
            Partition[
                Table[{({j, i}(*-CentralPixCoord*)), ({j, i} - {CRPIX1, CRPIX2})*{CDELT1,CDELT1} + {CRVAL1, CRVAL2}, kConvFac2*kGridTmp1[[i, j]],
       0.(*[Delta]k*)} // Flatten, {i, 1, NAXIS2}, {j, 1, NAXIS1}] //
     Flatten, 6];'''
    hdul.close()