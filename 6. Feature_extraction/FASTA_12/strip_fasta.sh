echo "Enter filename containing patterns to match: "
read filename

echo "Enter fasta filename: " 
read fasta

grep -A2 -w -f "$filename" "$fasta" > output.fasta