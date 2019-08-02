from cluster_config import GLConfig

def import_gl_info(path):
    with open(path, "r") as dat_file:
        theta_ev_sum = 0.0
        i = 0
        for line in dat_file:
            line.strip()
            if line[0] == "#" or line[0] == "":
                continue
            else:
                if i == 0:
                    GLConfig.src_redshift = float(line)
                elif i == 1:
                    GLConfig.nslv = float(line)
                else:
                    theta_ev_sum += float(line)
                i += 1
        GLConfig.theta_ev = theta_ev_sum / 2