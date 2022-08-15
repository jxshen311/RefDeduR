# Import unidecode module from unidecode
from unidecode import unidecode
import regex
import sys

inFile = sys.argv[1]
outFile = sys.argv[2]

# input file
fin = open(inFile, "rt")

# output file to write the result to
fout = open(outFile, "wt")

# transliterate greek letters to alphabet
# Create transliteration dictionary
# Credit to https://gist.github.com/beniwohli/765262 for the initial dictionary list
greek_alphabet_dict = {
    u'\u0391': 'Alpha',
    u'\u0392': 'Beta',
    u'\u0393': 'Gamma',
    u'\u0394': 'Delta',
    u'\u0395': 'Epsilon',
    u'\u0396': 'Zeta',
    u'\u0397': 'Eta',
    u'\u0398': 'Theta',
    u'\u0399': 'Iota',
    u'\u039A': 'Kappa',
    u'\u039B': 'Lamda',
    u'\u039C': 'Mu',
    u'\u039D': 'Nu',
    u'\u039E': 'Xi',
    u'\u039F': 'Omicron',
    u'\u03A0': 'Pi',
    u'\u03A1': 'Rho',
    u'\u03A3': 'Sigma',
    u'\u03A4': 'Tau',
    u'\u03A5': 'Upsilon',
    u'\u03A6': 'Phi',
    u'\u03A7': 'Chi',
    u'\u03A8': 'Psi',
    u'\u03A9': 'Omega',
    u'\u03B1': 'alpha',
    u'\u03B2': 'beta',
    u'\u03B3': 'gamma',
    u'\u03B4': 'delta',
    u'\u03B5': 'epsilon',
    u'\u03B6': 'zeta',
    u'\u03B7': 'eta',
    u'\u03B8': 'theta',
    u'\u03B9': 'iota',
    u'\u03BA': 'kappa',
    u'\u03BB': 'lamda',
    u'\u03BC': 'mu',
    u'\u03BD': 'nu',
    u'\u03BE': 'xi',
    u'\u03BF': 'omicron',
    u'\u03C0': 'pi',
    u'\u03C1': 'rho',
    u'\u03C3': 'sigma',
    u'\u03C4': 'tau',
    u'\u03C5': 'upsilon',
    u'\u03C6': 'phi',
    u'\u03C7': 'chi',
    u'\u03C8': 'psi',
    u'\u03C9': 'omega',
    u'\u03d5': 'phi',
    u'\u03f5': 'epsilon'}

# Define function transliterating text
def transliterate(text, translit_dict):
    new_word = ''
    for letter in text:
        new_letter = ''
        if letter in translit_dict:
            new_letter = translit_dict[letter]
        else:
            new_letter = letter
        new_word += new_letter
    return new_word


# for each line in the input file, Obtain Transliterated text
for line in fin:
    if bool(regex.search("\p{Block=Greek}", line)):  # return TRUE if find
        # transliterate the line and write to output file
        transliterated_text = transliterate(line, greek_alphabet_dict)
        fout.write(transliterated_text)
    else:
        fout.write(line)


# close files
fin.close()
fout.close()