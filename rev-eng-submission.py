import csv as csv

#Movies seen by test users
test_file = open('data/testInteractions.csv', 'rb')
test_csv = csv.reader(test_file)
header = test_csv.next()


#Seen movies map
userId = ''
seenMovies = []
seenMoviesByUserId = {}
for row in test_csv:
  # row is UserId, ItemId, Rating
  # If we are changing user
  if userId != row[0]:
    # Add the movie list if not the first row
    if userId != '':
      seenMoviesByUserId[userId] = seen_movies
    # Update current user and reset predicted movies
    userId = row[0]
    seen_movies = []
  # Add movie if seen
  seen_movies.append(row[1])
# Last user
seenMoviesByUserId[userId] = seen_movies

#No longer needed
test_file.close()

# Possible movie lens mapping 
# Calculated statically on Excel
mappedRatings_file = open('data/mappedPublicRatings.csv', 'rb')
mappedRatings_csv = csv.reader(mappedRatings_file)
header = mappedRatings_csv.next()

# Output file
prediction_file = open('rev-eng-submission.csv', 'wb')
prediction_csv = csv.writer(prediction_file)
prediction_csv.writerow(["UserId","RecommendedMovieIds"]) # Header

# Go though mapped ratings file
userId = ''
mapped_movies = []
for row in mappedRatings_csv:
  # row is UserId, ItemId, Rating
  # If we are changing user
  if userId != row[0]:
    # Write the prediction if not on the first iteration
    if userId != '':
      predicted_movies = " ".join(mapped_movies[:5])
      prediction_csv.writerow([userId,predicted_movies])
    # Update current user and reset predicted movies
    userId = row[0]
    mapped_movies = []
  # Add movie if not seen
  if row[1] not in seenMoviesByUserId[userId]:
    mapped_movies.append(row[1])
# Last user
predicted_movies = " ".join(mapped_movies[:5])
prediction_csv.writerow([userId,predicted_movies])

mappedRatings_file.close()
prediction_file.close()