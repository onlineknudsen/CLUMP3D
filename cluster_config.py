# this file used to be global variables that file_readers.py modified
# as it imports data changed to a class-structure with an internal dictionary (EM)

import os

class ClusterConfig:

    '''class type used to store configuration values'''
    # eventually add methods to modify fields, until then access directly

    def __init__(self):
        self.dict = {}

        # GL data
        self.dict['src_redshift'] = 0.0
        self.dict['nslv'] = 0.0
        self.dict['theta_ev'] = 0.0
        self.dict['ra'] = 0.0
        self.dict['dec'] = 0.0
        self.dict['wl_omega_m'] = 0.0

        # global cluster info
        self.dict['redshift'] = 0.0
        self.dict['cluster_name'] = "macsj0429"
        self.dict['working_directory'] = os.path.join("data", self.dict['cluster_name'])

        #czakon tab 3
        self.dict['frac_beam_smoothing'] = 0.0

    def __repr__(self):
        return "ClusterConfig [%s]" % str(self.dict)

    def set(self, field, value):
        self.dict[field] = value

    def get(self, field):
        return self.dict[field]
