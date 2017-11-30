echo "Enter filename containing patterns to match: "
read filename

echo "Enter fasta filename: " 
read fasta

grep -A1 -w -f "$filename" "$fasta" > output.fasta