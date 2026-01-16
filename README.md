# Sounds through time: How has music changed since the 1960s?
IJC445 module report code.

The code can be used to explore how the features of popular music have evolved over time, since the 1960s. It will create a composite visualisation consisting of four separate visualisations.

Research question:
How have genre representation, artist type and associated acoustic characteristics of music changed across decades?

Instructions for downloading and running the code:

The code utilises the 'MusicOSet' which can be found on the following link: https://marianaossilva.github.io/DSW2019/. The code only uses:

- songs.csv (includes song names, song popularity (on Spotify) and artist ID)
- acoustic_features.csv (includes acoustic feature values for each song. These included acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence and tempo)
- albums.csv (includes album IDs, album names, artist IDs and album popularity (on Spotify))
- artists.csv (includes artist popularity scores (on Spotify), the artists’ main genre, and artist types)
- tracks.csv (includes release dates and album metadata)

These datasets need to be in the same folder as your R project in order to import them into R using the code provided.

The code is in two separate R documents:

1. musicpreprocessing.R
2. 445vis.R

Packages may need to be installed if not already.

Each code document can be run in its entirety at once. However, is imperative they are ran in the above order.

The code is commented throughout to aid understanding.
