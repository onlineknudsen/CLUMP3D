# this file used to be global variables that
# file_readers.py modified as it imports data
# changed to a class-structure (EM)

import os

class ClusterConfig:

    '''class type used to store configuration values'''
    # eventually add methods to modify fields, until then access directly

    def __init__(self):
        # GL data
        self.src_redshift = 0.0
        self.nslv = 0.0
        self.theta_ev = 0.0
        self.ra = 0.0
        self.dec = 0.0
        self.wl_omega_m = 0.0

        # global cluster info
        self.redshift = 0.0
        self.cluster_name = "macsj0429"
        self.working_directory = os.path.join("data", self.cluster_name)

        #czakon tab 3
        self.frac_beam_smoothing = 0.0
