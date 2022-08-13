# Import unidecode module from unidecode
from unidecode import unidecode
import sys

inFile = sys.argv[1]
outFile = sys.argv[2]

fin = open(inFile, "rt")

# output file to write the result to
fout = open(outFile, "wt")

# non-ASCII text (Unicode string)
# check
# print(unidecode(u'á é è â î ô ñ ü ï ç'))

# for each line in the input file, Obtain Transliterated text
for line in fin:
        fout.write(unidecode(line))


# close files
fin.close()
fout.close()


