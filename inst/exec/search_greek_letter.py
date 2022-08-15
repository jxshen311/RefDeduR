import regex
import sys

inFile = sys.argv[1]
outFile = sys.argv[2]

fin = open(inFile, "rt")

# output file to write the result to
fout = open(outFile, "wt")

# extract all greek letters
greek_letters = list()
# for each line in the input file
for line in fin:
        greek_letters.extend(regex.findall("\p{Block=Greek}", line))

greek_letters_unique = list(dict.fromkeys(greek_letters))

fout.write(str(greek_letters_unique))


# close input and output files
fin.close()
fout.close()