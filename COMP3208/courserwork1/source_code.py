import numpy as np

# Read the data
def read_data(file_path, is_train=True):
    data = []
    with open(file_path, 'r') as file:
        for line in file:
            parts = line.strip().split(',')
            if is_train:
                user_id, item_id, recommendation, timestamp = map(float, parts)
                data.append((int(user_id), int(item_id), float(recommendation), int(timestamp)))
            else:
                user_id, item_id, timestamp = map(int, parts)
                data.append((user_id, item_id, timestamp))
    return data