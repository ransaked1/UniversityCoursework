import numpy as np

def load_data(file):
    with open(file) as f:
        rows = f.readlines()
    return np.asarray([row.strip('\n').split(',') for row in rows])

def save_prediction_matrix(file, predictions, timestamps):
    with open(file, 'w') as f:
        for i in range(len(predictions)):
            user_id = int(predictions[i, 0])
            item_id = int(predictions[i, 1])
            rating = predictions[i, 2]
            timestamp = int(timestamps[i][2])

            f.write(f"{user_id},{item_id},{rating},{timestamp}\n")

def build_rating_matrix(rows):
    n_users, n_items = rows[:, :2].astype(int).max(axis=0)
    rating_matrix = np.zeros((n_users, n_items), dtype=np.float16)
    user_ratings_sum = np.zeros(n_users)
    user_ratings_count = np.zeros(n_users, dtype=int)

    for user, item, rating, _ in rows:
        user_index = int(user) - 1
        item_index = int(item) - 1
        rating_matrix[user_index, item_index] = float(rating)  # Ensure rating is converted to float
        user_ratings_sum[user_index] += float(rating)  # Ensure rating is converted to float
        user_ratings_count[user_index] += 1

    # Calculate mean rating for each user
    mean_ratings = np.divide(user_ratings_sum, user_ratings_count, where=user_ratings_count != 0)

    # Mean centering the rating matrix
    for user_index in range(n_users):
        non_zero_indices = np.where(rating_matrix[user_index] != 0)[0]
        rating_matrix[user_index, non_zero_indices] -= mean_ratings[user_index]

    return rating_matrix, mean_ratings

def adjusted_cosine(i1_rating, i2_rating, w=None):
    intersection_indices = np.where((i1_rating != 0) & (i2_rating != 0))[0]
    
    if len(intersection_indices) == 0:
        return np.NaN
    
    if w is None:
        w = np.ones_like(intersection_indices, dtype=np.float64)
        
    i1_rating = i1_rating[intersection_indices]
    i2_rating = i2_rating[intersection_indices]
    
    numerator = np.sum(w * i1_rating * i2_rating)
    denominator = np.sqrt(np.sum(w * i1_rating ** 2)) * np.sqrt(np.sum(w * i2_rating ** 2))
    
    if denominator == 0:
        return np.NaN
    
    return numerator / denominator

def adjusted_cosine_similarity(rating_matrix, n=15):
    n_items = rating_matrix.shape[1]
    similarity_matrix = np.zeros((n_items, n_items), dtype=np.float16)
    
    for i in range(n_items):
        for j in range(i, n_items):  # Only compute upper triangular part
            similarity_matrix[i, j] = adjusted_cosine(rating_matrix[:, i], rating_matrix[:, j])
            similarity_matrix[j, i] = similarity_matrix[i, j]  # Similarity matrix is symmetric
    
    if n > 0:
        top_n_indices = np.argsort(-similarity_matrix, axis=1)[:, :n]
        for i in range(n_items):
            similarity_matrix[i, :] = 0
            similarity_matrix[i, top_n_indices[i]] = 1
    
    return similarity_matrix

def predict_ratings(rating_matrix, similarity_matrix, test_data, mean_score, mean_ratings):
    predictions = []

    for user, item, _ in test_data:
        user_index = int(user) - 1
        item_index = int(item) - 1

        # Get the ratings of the user for all items
        user_ratings = rating_matrix[user_index]

        # Get similarity values
        item_similarities = similarity_matrix[item_index]

        # Find non-zero and non-rated items
        non_zero_indices = np.where(user_ratings != 0)[0]

        prediction = None

        # Calculate prediction
        if len(non_zero_indices) > 0:
            denominator = np.sum(np.abs(item_similarities[non_zero_indices]))
            if denominator != 0:
                prediction = np.sum(user_ratings[non_zero_indices] * item_similarities[non_zero_indices]) / denominator

        # Handle NaN predictions
        if np.isnan(prediction):
            prediction = 0  # Assign the user mean score if prediction is NaN

        # Adjust prediction by adding the mean rating for the user
        prediction += mean_ratings[user_index]

        prediction = float(round(prediction))

        # Clip the predicted rating to the range [1.0, 5.0]
        prediction = np.clip(prediction, 1.0, 5.0)

        predictions.append([user_index + 1, item_index + 1, prediction])

    return np.array(predictions)

def run(train_data_file, test_data_file, save_file):
    # Load training and test data
    train_data = load_data(train_data_file)
    test_data = load_data(test_data_file)

    # Calculate the mean overall score of the training dataset
    mean_score = np.mean(train_data[:, 2].astype(float))

    # Build the rating matrix
    rating_matrix, mean_ratings = build_rating_matrix(train_data)

    # Compute similarity matrix
    similarity_matrix = adjusted_cosine_similarity(rating_matrix, 0)

    # Predict ratings for test data
    predictions = predict_ratings(rating_matrix, similarity_matrix, test_data, mean_score, mean_ratings)

    # Save predictions to file
    save_prediction_matrix(save_file, predictions, test_data)

if __name__ == '__main__':
    train_file = "train_100k_withratings.csv"
    test_file = "test_100k_withoutratings.csv"
    save_file = "results.csv"
    run(train_file, test_file, save_file)