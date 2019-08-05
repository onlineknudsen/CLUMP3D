import cluster_config
import astropy
from astropy.io import fits
import os

def import_cluster_info(working_dir):
    with open(os.path.join(working_dir, "cluster_info.dat"), "r") as dat_file:
        i = 0
        for line in dat_file:
            line.strip()
            if line == "\n" or line[0] == "#" or line[0] == "":
                continue
            else:
                if i == 0:
                    cluster_config.redshift = float(line)

def import_sz_info(working_dir):
    print("Not implemented yet")

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
                    cluster_config.src_redshift = float(line)
                elif i == 1:
                    cluster_config.nslv = float(line)
                elif i == 2:
                    cluster_config.ra = float(line)
                elif i == 3:
                    cluster_config.dec = float(line)
                else:
                    theta_ev_sum += float(line)
                i += 1
        cluster_config.theta_ev = theta_ev_sum / 2

# trying to make FITS readers here

def import_gl_mean_info(working_dir): # there might be a better name for this
    with fits.open(os.path.join(working_dir, "GL", "gl_info.dat")) = hdul #saves it as an HDUList, will look up more documentation soon
        #hdul.info() would give us some debugging opportunities here
        header = hdul[0].header
        cluster_config.mean_gl_cdelt1 = header["CDELT1"]
        cluster_config.mean_gl_cdelt2 = header["CDELT2"]
        cluster_config.mean_gl_crval1 = header["CRVAL1"]
        cluster_config.mean_gl_crval2 = header["CRVAL2"]
        cluster_config.mean_gl_naxis1 = header["NAXIS1"]
        cluster_config.mean_gl_naxis2 = header["NAXIS2"]
        cluster_config.mean_gl_crpix1 = header["CRPIX1"]
        cluster_config.mean_gl_crpix2 = header["CRPIX2"]

        # need to figure out what these lines do: 
        '''kGridTmp1 = kTabFits[[2, 1]] // Chop;
        
        (*{0.,0} (physical coordinates) is the optical coordinates*)
        DataGridTabWL2 = 
            Partition[
                Table[{({j, i}(*-CentralPixCoord*)), ({j, i} - {CRPIX1, CRPIX2})*{CDELT1,CDELT1} + {CRVAL1, CRVAL2}, kConvFac2*kGridTmp1[[i, j]], 
       0.(*\[Delta]k*)} // Flatten, {i, 1, NAXIS2}, {j, 1, NAXIS1}] //
     Flatten, 6];'''    