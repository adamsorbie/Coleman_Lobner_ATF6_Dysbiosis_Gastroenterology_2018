echo "Enter filename containing patterns to match: "
read filename

echo "Enter fasta filename: " 
read fasta

echo "Enter output filename: " 
read output

grep -A1 -w -f "$filename" "$fasta" > "$output"