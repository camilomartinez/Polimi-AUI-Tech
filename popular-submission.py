import csv as csv

#Most popular good rated movies
# Calculated statically on Excel
popular = ['3892', '3145', '3747', '1088', '1645', '1763', '456', '1190', '537', '3498']

#Movies seen by test users
test_file = open('data/testInteractions.csv', 'rb')
test_csv = csv.reader(test_file)
header = test_csv.next()

# Output file
prediction_file = open('popular-submission.csv', 'wb')
prediction_csv = csv.writer(prediction_file)
prediction_csv.writerow(["UserId","RecommendedMovieIds"]) # Header

userId = ''
movies = ''
for row in test_csv:
  # row is UserId, ItemId, Rating
  # If we are changing user
  if userId != row[0]:
    # Write the prediction if not on the first iteration
    if userId != '':
      predicted_movies = " ".join(movies[:5])
      prediction_csv.writerow([userId,predicted_movies])
    # Update current user and reset predicted movies
    userId = row[0]
    movies = list(popular)
  # Remove movie if seen
  if row[1] in movies:
    movies.remove(row[1])
# Last user
predicted_movies = " ".join(movies[:5])
prediction_csv.writerow([userId,predicted_movies])

test_file.close()
prediction_file.close()