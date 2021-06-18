library(spotidy)
library(dplyr)
SPOTIFY_CLIENT_ID <-"79ba4f46f96048509889804d6ae01f8c"
SPOTIFY_CLIENT_SECRET <- "840aa37dd39a4b01a3d388f4d61ec931"

my_token<-get_spotify_api_token(SPOTIFY_CLIENT_ID,SPOTIFY_CLIENT_SECRET)


artists<-search_artists(
  "J Balvin",
  output = c("tidy", "raw", "id"),
  limit = 20,
  offset = 0,
  token = my_token
)
artist_id<-as.data.frame(artists[1,2])



TOP_JB<-get_artist_top_tracks(
  artist_id,
  "US",
  output = c("tidy", "raw"),
  limit = 20,
  offset = 0,
  token = my_token
)

artist_id2<-as.data.frame(TOP_JB[2,2])

khalid<-search_artists(
  "Khalid",
  output = c("tidy"),
  limit = 20,
  offset = 0,
  token = my_token
)

Khalid<-as.data.frame(khalid[1,2])

albums<-get_artist_albums(
  Khalid,
  output = c("tidy"),
  limit = 50,
  offset = 0,
  token = my_token
)
album<-albums[5,4]
aa<-get_album_tracks(
  album,
  output = c("tidy"),
  limit = 50,
  offset = 0,
  token = my_token
)
cancion<-aa[13,5]
