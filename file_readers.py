from astropy.io import fits
import numpy as np
import os

def import_cluster_info(cc):
    '''cc is a ClusterConfig object, defined in cluster_config.py'''
    with open(os.path.join(cc.get("working_directory"), "cluster_info.dat"), "r") as dat_file:
        i = 0
        for line in dat_file:
            line.strip()
            if line == "\n" or line[0] == "#" or line == "":
                continue
            else:
                if i == 0:
                    cc.set("redshift", float(line))

def import_sz_info(cc):
    '''cc is a ClusterConfig object, defined in cluster_config.py'''
    with open(os.path.join(cc.get("working_directory"), "SZ", "sz_info.dat"), "r") as dat_file:
        i = 0
        for line in dat_file:
            line.strip()
            if line == "\n" or line[0] == "#" or line == "":
                continue
            else:
                if i == 0:
                    cc.set("frac_beam_smoothing", float(line))

def import_gl_info(cc):
    '''cc is a ClusterConfig object, defined in cluster_config.py'''
    with open(os.path.join(cc.get("working_directory"), "GL", "gl_info.dat"), "r") as dat_file:
        theta_ev_sum = 0.0
        i = 0
        for line in dat_file:
            line.strip()
            if line == "\n" or line[0] == "#" or line == "":
                continue
            else:
                if i == 0:
                    cc.set("src_redshift", float(line))
                elif i == 1:
                    cc.set("nslv", float(line))
                elif i == 2:
                    cc.set("ra", float(line))
                elif i == 3:
                    cc.set("dec", float(line))
                elif i == 4:
                    cc.set("wl_omega_m", float(line))
                else:
                    theta_ev_sum += float(line)
                i += 1
        cc.set("theta_ev", theta_ev_sum / 2)
