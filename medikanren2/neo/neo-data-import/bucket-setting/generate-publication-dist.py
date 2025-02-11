import pandas as pd
import math
import csv

file_base = '../../neo-data/raw_downloads_from_kge_archive/rtx-kg2-v2.10.0/'

file_path = file_base + 'scores.tsv'
max_bucket_size_percentage = 0.02
min_bucket_size_percentage = max_bucket_size_percentage * 0.7
max_bucket_size_min = 20000
min_bucket_size_min = max_bucket_size_min * 0.7

def export_to_tsv(data, filename):
    with open(filename, 'w', newline='') as file:
        writer = csv.writer(file, delimiter='\t')

        for record, nested_dict in data.items():
            for column, value in nested_dict.items():
                writer.writerow([record, column, value])

# Load the TSV file
data = pd.read_csv(file_path, sep='\t')

# Calculate the total number of edges for each predicate
total_edges_per_predicate = data.groupby('predicate')['num-edges'].sum()

# Define the maximum and minimum bucket size thresholds
max_bucket_size = total_edges_per_predicate.apply(lambda x: max(x * max_bucket_size_percentage, max_bucket_size_min))
min_bucket_size = total_edges_per_predicate.apply(lambda x: max(x * min_bucket_size_percentage, min_bucket_size_min))

# Calculate the number of buckets needed for each score of each predicate
buckets_needed = {}

for predicate, group in data.groupby('predicate'):
    scores_sorted = group.sort_values('score').reset_index(drop=True)
    handled_scores = set()  # To keep track of scores that have been handled

    for index, row in scores_sorted.iterrows():
        score = row['score']
        num_edges = row['num-edges']

        if score in handled_scores:
            continue  # Skip this score because it's already been handled

        if num_edges > max_bucket_size[predicate]:
            # Split into multiple buckets if more than 5% of the total edges
            num_buckets = math.ceil(num_edges / max_bucket_size[predicate])
            buckets_needed.setdefault(predicate, {})[score] = num_buckets
        elif num_edges >= min_bucket_size[predicate]:
            # Assign a single bucket if between 3.5% and 5%
            buckets_needed.setdefault(predicate, {})[score] = 1
        else:
            # Attempt to combine with subsequent scores if less than 3.5%
            current_total = num_edges
            start_score = score  # Track the starting score of the bucket

            for j in range(index + 1, len(scores_sorted)):
                next_score = scores_sorted.at[j, 'score']
                next_num_edges = scores_sorted.at[j, 'num-edges']

                if next_score in handled_scores:
                    continue  # Skip already handled scores

                if current_total + next_num_edges <= max_bucket_size[predicate]:
                    current_total += next_num_edges
                    handled_scores.add(next_score)  # Mark this score as handled
                    buckets_needed.setdefault(predicate, {})[next_score] = 0

                    if current_total >= min_bucket_size[predicate]:
                        break  # Stop combining if we reach the minimum size

            # Assign a bucket to the starting score of the combination
            buckets_needed.setdefault(predicate, {})[start_score] = 1
            handled_scores.add(start_score)  # Mark the starting score as handled

start_bucket_numbers = {predicate: {} for predicate in buckets_needed.keys()}
for predicate, scores in buckets_needed.items():
    start_number = 1
    last_start_number = start_number
    for score, num_buckets in sorted(scores.items(), key=lambda x: x[0]):
        if num_buckets==0:
            start_bucket_numbers[predicate][score] = last_start_number
        else:
            start_bucket_numbers[predicate][score] = start_number
            last_start_number = start_number
        start_number += num_buckets

export_to_tsv(buckets_needed, file_base + "buckets-needed.tsv")
export_to_tsv(start_bucket_numbers, file_base + "start-bucket-numbers.tsv")
