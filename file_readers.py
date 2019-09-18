import cluster_config as cc
from astropy.io import fits
import numpy as np
import os

def import_cluster_info():
    with open(os.path.join(cc.working_directory, "cluster_info.dat"), "r") as dat_file:
        i = 0
        for line in dat_file:
            line.strip()
            if line == "\n" or line[0] == "#" or line == "":
                continue
            else:
                if i == 0:
                    cc.redshift = float(line)

def import_sz_info():
    with open(os.path.join(cc.working_directory, "SZ", "sz_info.dat"), "r") as dat_file:
        i = 0
        for line in dat_file:
            line.strip()
            if line == "\n" or line[0] == "#" or line == "":
                continue
            else:
                if i == 0:
                    cc.frac_beam_smoothing = float(line)


def import_gl_info():
    with open(os.path.join(cc.working_directory, "GL", "gl_info.dat"), "r") as dat_file:
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
                elif i == 4:
                    cc.wl_omega_m = float(line)
                else:
                    theta_ev_sum += float(line)
                i += 1
        cc.theta_ev = theta_ev_sum / 2
