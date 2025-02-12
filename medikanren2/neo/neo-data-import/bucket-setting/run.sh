#!/usr/bin/env bash

# Exit immediately if a command exits with a non-zero status
set -e

echo "===== Step 1: Calculating the predicate and score distribution for RTX-KG2====="
racket rtx-kg2-publication-distribution.rkt

echo "===== Step 2: Checking for pandas in Python ====="
# Check if pandas is installed
if ! python3 -c "import pandas" &> /dev/null; then
  echo "pandas is not installed. Installing now..."
  pip3 install pandas
else
  echo "pandas is already installed."
fi

echo "===== Step 3: Assigning buckets ====="
python3 generate-publication-dist.py

echo "===== The bucket setting is completed. ====="
