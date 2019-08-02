import cluster_config
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