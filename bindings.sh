#!/usr/bin/env bash

# This script will find all 'bindings' directories in the 'crates' directory
# then copy the .ts files from those directories to a corresponding directory
# under packages/wasm/dist/bindings, while changing the extension to .d.ts

# Starting directory
start_dir="crates"

# Destination directory
dest_dir="packages/wasm/dist/bindings"

# Find all 'bindings' directories under 'crates'
find "$start_dir" -type d -name 'bindings' | while read -r dir
do
    # Extract the subfolder name
    subfolder_name=$(basename "$(dirname "$dir")")

    # Create the destination directory
    mkdir -p "$dest_dir/$subfolder_name"

    # Copy and rename .ts files to .d.ts
    find "$dir" -name '*.ts' | while read -r tsfile
    do
        # Get the base filename, without the .ts extension
        base_name=$(basename "$tsfile" .ts)

        # Copy with modifications using sed
        sed 's/\(import.*from.*\)\("\|'\''\)\(.*\)\("\|'\''\);/\1\2\3.js\4;/' "$tsfile" > "$dest_dir/$subfolder_name/$base_name.d.ts"
    done
done
