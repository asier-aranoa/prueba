#API Spotify
install.packages("spotidy")
library(spotidy)
library(dplyr)

SPOTIFY_CLIENT_ID <-"79ba4f46f96048509889804d6ae01f8c"
SPOTIFY_CLIENT_SECRET <- "840aa37dd39a4b01a3d388f4d61ec931"

my_token <- get_spotify_api_token(SPOTIFY_CLIENT_ID, SPOTIFY_CLIENT_SECRET)


artist <- search_artists(
  "J Balvin",
  output = c("tidy"),
  limit = 20,
  offset = 0,
  token = my_token
)

artist_id <- print(artist[1,2])

mas_populares_usa<-get_artist_top_tracks(
  artist_id,
  country = "US",
  output = c("tidy"),
  limit = 50,
  offset = 0,
  token = my_token
)

artist_rel <- search_artists(
  "Khalid",
  output = c("tidy"),
  limit = 20,
  offset = 0,
  token = my_token
)

artist_rel_id <- print(artist_rel[1,2])

albums <- get_artist_albums(
  artist_rel_id,
  output = c("tidy"),
  limit = 50,
  offset = 0,
  token = my_token
)

albums <- albums[albums$artist_id==albums[2,2],]

albums <- arrange(albums, release_date)

album_id <- print(albums[1,4])

album_tracks <- get_album_tracks(
  album_id,
  output = "tidy",
  limit = 16,
  offset = 0,
  token = my_token
)

album_tracks <- arrange(album_tracks, -duration)

