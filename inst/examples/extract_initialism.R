# Example 1
journal <- c("JOURNAL OF BIOLOGICAL CHEMISTRY",
             "Proteins: Structure, Function and Bioinformatics",
             "Archives of Biochemistry and Biophysics",
             "Clin Infect Dis",
             "Clinical Infectious Diseases",
             "CLINICAL INFECTIOUS DISEASES")

extract_initialism(journal)


# Example 2
journal_norm <- c("journal biological chemistry",
                  "proteins structure function bioinformatics",
                  "archives biochemistry biophysics",
                  "clin infect dis",
                  "clinical infectious diseases",
                  "clinical infectious diseases")

extract_initialism(journal_norm)
