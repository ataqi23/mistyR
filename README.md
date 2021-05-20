
# mistyR: An R Implementation of Musical Set Theory (MST)

Musical Set Theory is a mathematical formalization of various common objects found in the 12-TET tuning system. Although called set theory, musical set theory is more-so an application of group theory and combinatorics to 'pitch classes'. 
From pitch classes (the theoretical variant of a musical note), we are able to formally study objects like scales, musical inversions, transpositions, and various schools of harmony. 

## Misty

Misty is my current project to develop various objects of Musical Set Theory in R. I hope to one day generalize this for uses in practical musical analysis; vectorization of tonal features for neural network composition. Currently, there are two R modules for musical stylometry analysis of .krn files and .midi files, the letter of which is in development.

Currently, there are implementations for the following objects:
- Scales (Diatonic, Modal, Circle of Fifths)
- Chords (Inversions, Extensions, Tertiary Stacks)

## KRN Module

With Misty comes a module for extracting features from musical scores in the .kern file format. In addition to the feature extraction functionality, the module comes with accompanying functions to perform musical stylometry analysis fo the scores. The source code from this can be found from Emily Palmers **museR** package. 

Currently, there are implementations for the following features:
- Modal feature analysis (of scale degree frequencies)
- Rhythmic entropy
- Rhythmic subdivision frequencies

## Citations

Emily Palmer's museR package could be found in the link below.
https://github.com/empalmer/museR
