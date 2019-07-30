def import_dat_file(file_path):
    with open(file_path, "r") as dat_file:
        data = []
        line = dat_file.readline()
        for line in dat_file:
            line.strip()
            if line[0] == "#" or line[0] == ";;" or line[0] == "":
                continue
            else:
                entries = line.split()
                new_entry = []
                for e in entries:
                    value = None
                    try:
                        value = float(e)
                    except ValueError:
                        value = e
                    new_entry.append(value)
                    data.append(new_entry)
    return data
