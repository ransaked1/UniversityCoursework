### Imports
import numpy as np

# Used only for IO
import os
from tqdm import tqdm

#################################
#                               #
#    THEORETICAL BACKGROUND     #
#                               #
#################################

"""
COLLABORATIVE FILTERING:
Collaborative filtering is a recommendation system techniquew that predics user preference based on
preferences of similar users or items. It is based on the assumption that users who have agreed in
the past tend to agree on their choices in the future, items that are liked by users with similar
tastes will likely be liked by the current user.

Collaborative filtering does not rely on explicit feature extraction or domain knowledge of the items
recommended, insted focusing on capturing relationships and patterns in user-item interactions.

MATRIX FACTORISATION:
Matrix factorization is a technique used to decompose a matrix into the product of two or more matrices,
particularly useful in collaborative filtering recommendation systems. Given an input matrix A of size
m x n, matrix factorization aims to find two or more factor matrices the product of which approximates
the original matrix. 

Mathematically speaking:
A ≈ UVᵀ
Where U is a m x k user matrix, Vᵀ is a k x n item matrix and k is the number of factors specified as
a hyperparameter before training.

In this process the model captures underlying structure of the data by representing it in terms of
latent factors that describe the relationships between rows and columns in the original matrix, which
can then be used to make personalized recommendations based on the dot-product of the user and item
factor vectors.

The core concept used to find U and Vᵀ revolves around minimizing an objective function to reduce the
delta between the ground truth and predicted ratings across the training dataset. In our case, this is
the Mean Absolute Error that measure the absolute difference between the ground truth and predicted ratings.
Stochastic gradient descent is used minimize the objective function in our implementation. It iterates
over the user and item factor vectors adjusting them based on the prediction error for each instance
determined by learning rate and regularization term. The learning rate dictates the step size toward 
minimizing the loss function and the regularization term used to avoid overfittingby ecouraging the 
shrinking of factors towards zero thus preventing overly complex models.

OUR IMPLEMENTATION:
We improved on the matrix factorization model described above by introducing bias vectors. These are
added after computing the dot-product of the user and item matrices to express biases users might have
when rating items, straying from the average consensus. Same with movies that are outliers and are
rated higher or lower than the average. The bias vector aims to capture these outliers and bring them
back to the "average" when predictions are computed.

Also, we have split the datased used for training into batches of the size defined as a training parameter
to improve performace and use less memory during training with minimal impact on the model's efficiency.
At every training epoch the dataset is shuffled and new batches are created to avoid bias and generalise better.
This approach is also called mini-batch stochastic gradient descent.
"""


#################################
#                               #
#   IMPLEMENTATION PSEUDOCODE   #
#                               #
#################################

"""
CONSTANTS:
fc = factors count
bs = batch size
ec = epoch count
lr = learning rate
r = regularization term

VARIABLES:
um = randomly initialized user embeddings matrix
im = randomly initialized item embeddings matrix
uv = randomly initialized user bias vector
iv = randomly initialized item bias vector

IMPLEMENTATION:
initialize um, im, uv, iv with user and item counts
repeat ec:
    generate shuffled set of user-item pairs
    split into batches using bs
    for each batch:
        pred_rating = um . im + uv + iv
        rating = ratings(user, item)
        error = abs(pred_rating - rating)
        # adjust embedding matrices
        um = lr * (error * im - r * um)
        im = lr * (error * um - r * im)
        # adjust bias vectors
        uv = lr * (error - r * uv)
        iv = lr * (error - r * iv)
"""

#################################
#                               #
#   PRACTICAL IMPLEMENTATION    #
#                               #
#################################

### Loading datasets and IO

"""
    Load data from a file and convert it into a numpy array.

    INPUT = file (str): The path to the file containing the data.
    OUTPUT = np.ndarray: Numpy array containing the loaded data.
"""
def load_data(file):
    # Get total number of lines in the file
    with open(file) as f:
        total_lines = sum(1 for _ in f)

    # Initialize an empty list to store data
    data = []

    # Open the file and read data in chunks while displaying a progress bar
    with open(file) as f:
        with tqdm(total=total_lines, desc=f"Loading '{file}'") as pbar:
            for line in f:
                data.append(line.strip('\n').split(','))
                pbar.update(1)

    return np.asarray(data)

"""
    Create a directory if it doesn't exist.

    INPUT = path (str): The path of the directory to be created.
"""
def createdir(path):
    try:
        os.mkdir(path)
    except FileExistsError:
        print(f"'{path}' already exists")

"""
    Write evaluation results to a file.

    INPUT = dir (str): The directory where the evaluation file is located;
    e (int): Epoch number;
    train_eval (float): Evaluation result for the training set
"""
def write_eval(dir, e, train_eval):
    with open(os.path.join(dir, "evals.txt"), "a") as f:
        f.write(str((train_eval, e)) + "\n")

"""
    Write initial evaluation parameters to a file.

    INPUT = dir (str): The directory where the evaluation file is located;
    factors_count (int): Number of factors used in the model;
    learn_rate (float): Learning rate used in the model;
    regularization_term (float): Regularization term used in the model
"""
def write_eval_init(dir, factors_count, learn_rate, regularization_term):
    with open(os.path.join(dir, "evals.txt"), "a") as f:
        f.write(str((factors_count, learn_rate, regularization_term)) + "\n")

"""
    Save data to a file.

    INPUT = file (str): The path to the file where data will be saved;
    x (np.ndarray): Data to be saved
"""
def save_file(file, x):
    np.save(file, x, allow_pickle=True)


### Adjusting methods

"""
    Adjusts the factor matrices using stochastic gradient descent.

    INPUT = learning_rate (float): The learning rate parameter;
    regularization_term (float): The regularization term parameter;
    error (numpy.ndarray): The prediction error;
    factors1 (numpy.ndarray): The factor matrix to be adjusted;
    factors2 (numpy.ndarray): The factor matrix used for adjustment

    OOUTPUT = numpy.ndarray: The adjusted factor matrix
"""
def adjust_factors(learning_rate, regularization_term, error, factors1, factors2):
    # Remove single dimensional entries
    factors1 = np.squeeze(factors1)
    factors2 = np.squeeze(factors2)

    # Perform the adjustment
    adjusted_factors = factors1 + learning_rate * (error * factors2 - regularization_term * factors1)

    # Consistently reshape the resut to 2d arrays
    adjusted_factors = np.atleast_2d(adjusted_factors)

    return adjusted_factors

"""
    Adjusts the bias vectors using stochastic gradient descent.

    INPUT = learning_rate (float): The learning rate parameter;
    regularization_term (float): The regularization term parameter;
    error (numpy.ndarray): The prediction error;
    bias (numpy.ndarray): The bias vector to be adjusted

    OUTPUT = numpy.ndarray: The adjusted bias vector
    """
def adjust_bias(learning_rate, regularization_term, error, bias):
    adjusted_bias = bias + learning_rate * (error - regularization_term * bias)
    return adjusted_bias


### Error Measure

"""
    Calculate the Mean Absolute Error (MAE) between predicted and actual ratings.

    INPUT = pred: Predicted ratings; real: Actual ratings

    OUTPUT = mae: Mean Absolute Error between predicted and actual ratings
"""
def mae(pred, real):
    return np.mean(np.abs(pred - real))


### Prediction


"""
    Predict ratings for validation data using matrix factorization model.

    INPUT = val_data (numpy.ndarray): Validation data containing user IDs, item IDs, and actual ratings;
    user_embed_matrix (numpy.ndarray): Matrix containing user embeddings;
    item_embed_matrix (numpy.ndarray): Matrix containing item embeddings;
    user_bias_matrix (numpy.ndarray): Vector containing user biases;
    item_bias_matrix (numpy.ndarray): Vector containing item biases

    OUTPUT = pred: Predicted ratings;
    users: User IDs;
    items: Item IDs;
    user_factors: Embeddings for users;
    item_factors: Embeddings for items;
    user_biases: Biases for users;
    item_biases: Biases for items
"""
def predict(val_data, user_embed_matrix, item_embed_matrix, user_bias_matrix, item_bias_matrix):
    # Extract user and item IDs from validation data
    users = val_data[:, 0].astype(int) - 1
    items = val_data[:, 1].astype(int) - 1

    user_factors = user_embed_matrix[:, users]
    item_factors = item_embed_matrix[:, items]
        
    user_biases = user_bias_matrix[users]
    item_biases = item_bias_matrix[items]

    # Remove single-dimensional entries from factors
    user_factors = np.squeeze(user_factors)
    item_factors = np.squeeze(item_factors)

    # Perform dot product for scalar input or matrix multiplication for matrix input
    if user_factors.ndim == 1:
        pred = np.dot(user_factors, item_factors)
    else:
        pred = np.sum(user_factors * item_factors, axis=0)

    # Add user and item biases to predictions
    pred += user_biases + item_biases

    return pred, users, items, user_factors, item_factors, user_biases, item_biases


### Evaluation step

"""
    Evaluate the model on validation data.

    INPUT = val_data (numpy.ndarray): Validation data containing user-item pairs and their corresponding ratings;
    user_embed_matrix (numpy.ndarray): Matrix containing user embeddings;
    item_embed_matrix (numpy.ndarray): Matrix containing item embeddings;
    user_bias_matrix (numpy.ndarray): Vector containing user biases;
    item_bias_matrix (numpy.ndarray): Vector containing item biases

    OUTPUT = tuple: A tuple containing evaluation results and prediction details
"""
def evaluate(val_data, user_embed_matrix, item_embed_matrix, user_bias_matrix, item_bias_matrix):
    # Extract real ratings from validation data
    real_rating = val_data[:, 2].astype(np.float32)

    pred_rating, users, items, user_factors, item_factors, user_biases, item_biases = predict(
        val_data, 
        user_embed_matrix, 
        item_embed_matrix, 
        user_bias_matrix, 
        item_bias_matrix
    )

    error = mae(pred_rating, real_rating)
    return error, real_rating, pred_rating, users, items, user_factors, item_factors, user_biases, item_biases


### Predicting with pretrained model

"""
    Load pretrained model from saved files.

    INPUT = save_path (str): Path to the directory containing saved model files

    OUTPUT = tuple: A tuple containing user and item embeddings along with user and item biases
"""
def load_model(save_path):
    user_embed_matrix = np.load(os.path.join(save_path, "user_embed_matrix.npy"))
    item_embed_matrix = np.load(os.path.join(save_path, "item_embed_matrix.npy"))
    user_bias_matrix = np.load(os.path.join(save_path, "user_bias_matrix.npy"))
    item_bias_matrix = np.load(os.path.join(save_path, "item_bias_matrix.npy"))

    return user_embed_matrix, item_embed_matrix, user_bias_matrix, item_bias_matrix


"""
    Predict ratings using a pretrained model and save the results.

    INPUT = val_data (numpy.ndarray): Validation data containing user-item pairs;
    save_path (str): Path to the directory containing saved model files;
    save_file_name (str): Name of the file to save the prediction results
"""
def predict_with_loaded_model(val_data, save_path, save_file_name):
    user_embed_matrix, item_embed_matrix, user_bias_matrix, item_bias_matrix = load_model(save_path)

    # Make predictions on validation data
    _, _, predicted, _, _, _, _, _, _ = evaluate(
        val_data, 
        user_embed_matrix, 
        item_embed_matrix, 
        user_bias_matrix, 
        item_bias_matrix
    )

    # Round predictions to the nearest 0.5
    val_pred_rounded = np.round(predicted * 2) / 2

    # Save predictions to a new column in val_data
    val_data_with_pred = np.insert(val_data, -1, val_pred_rounded, axis=1)

    # Save results to save_file_name
    np.savetxt(os.path.join(save_path, save_file_name), val_data_with_pred, delimiter=',', fmt='%s')


### Training

"""
    Train the model using mini-batch stochastic gradient descent.

    INPUT = train_data (numpy.ndarray): Training data;
    user_embed_matrix (numpy.ndarray): User embedding matrix;
    item_embed_matrix (numpy.ndarray): Item embedding matrix;
    user_bias_matrix (numpy.ndarray): User bias matrix;
    item_bias_matrix (numpy.ndarray): Item bias matrix;
    batch_size (int): Size of each mini-batch;
    learning_rate (float): Learning rate for updating parameters;
    regularization_term (float): Regularization term to prevent overfitting

    OUTPUT = float: Mean evaluation metric computed over all mini-batches
"""
def train_iters(train_data, user_embed_matrix, item_embed_matrix, user_bias_matrix, item_bias_matrix, batch_size, learning_rate, regularization_term):
    train_evals = []
    indexes = np.arange(len(train_data))
    np.random.shuffle(indexes)
    batch_count = len(train_data) // batch_size

    for batch_num in tqdm(range(batch_count), desc="Training Progress For Epoch"):
        start_index = batch_num * batch_size
        end_index = min((batch_num + 1) * batch_size, len(train_data))
        batch = train_data[indexes[start_index:end_index]]

        # Evaluate the rating prediction performance on the training dataset
        train_eval, real_rating, pred_rating, users, items, user_factors, item_factors, user_biases, item_biases = evaluate(
            batch, 
            user_embed_matrix, 
            item_embed_matrix, 
            user_bias_matrix, 
            item_bias_matrix
        )

        # Compute prediction error
        error = real_rating - pred_rating

        # Adjust user factor vectors
        adjusted_user_factors = adjust_factors(learning_rate, regularization_term, error, user_factors, item_factors)

        # Adjust item factor vectors
        adjusted_item_factors = adjust_factors(learning_rate, regularization_term, error, item_factors, user_factors)

        # Save the adjusted factor vectors back into their corresponding matrices
        user_embed_matrix[:, users] = adjusted_user_factors
        item_embed_matrix[:, items] = adjusted_item_factors

        # Adjust biases
        adjusted_user_biases = adjust_bias(learning_rate, regularization_term, error, user_biases)
        adjusted_item_biases = adjust_bias(learning_rate, regularization_term, error, item_biases)

        # Save the adjusted user and item biases back into their corresponding vectors
        user_bias_matrix[users] = adjusted_user_biases
        item_bias_matrix[items] = adjusted_item_biases

        train_evals.append(train_eval)

    return np.mean(train_evals)

"""
    Train the model over multiple epochs and save the trained model and evaluation results.

    INPUT = train_data (numpy.ndarray): Training data;
    val_data (numpy.ndarray): Validation data;
    user_embed_matrix (numpy.ndarray): User embedding matrix;
    item_embed_matrix (numpy.ndarray): Item embedding matrix;
    user_bias_matrix (numpy.ndarray): User bias matrix;
    item_bias_matrix (numpy.ndarray): Item bias matrix;
    batch_size (int): Size of each mini-batch;
    epoch_count (int): Number of training epochs;
    learning_rate (float): Learning rate for updating parameters;
    regularization_term (float): Regularization term to prevent overfitting;
    save_path (str): Directory path to save the trained model and evaluation results;
    save_file_name (str): Name of the file to save evaluation results
"""
def train(train_data, val_data, user_embed_matrix, item_embed_matrix, user_bias_matrix, item_bias_matrix, batch_size, epoch_count, learning_rate, regularization_term, save_path, save_file_name):
    train_evals = []
    val_predicted = []

    # Initialize file to save evaluation results
    write_eval_init(save_path, factors_count, learn_rate, regularization_term)

    for e in tqdm(range(epoch_count), desc="Training Progress"):
        # Train the model for one epoch
        train_eval = train_iters(
            train_data, 
            user_embed_matrix, 
            item_embed_matrix, 
            user_bias_matrix, 
            item_bias_matrix, 
            batch_size, 
            learning_rate, 
            regularization_term
        )

        # Make predictions on validation data
        _, _, predicted, users, _, _, _, _, _ = evaluate(
            val_data, 
            user_embed_matrix, 
            item_embed_matrix, 
            user_bias_matrix, 
            item_bias_matrix
        )

        val_predicted = predicted

        train_evals.append(train_eval)
        

        write_eval(save_path, e, train_eval)
        print(f"Epoch eval:{train_eval}")

    # Round predictions to the nearest 0.5
    rounded_predictions = np.round(val_predicted * 2) / 2

    rounded_prediction = np.clip(rounded_predictions, 0.5, 5.0)

    # Save predictions to a new column in val_data
    val_data_with_pred = np.insert(val_data, -1, rounded_prediction, axis=1)

    # Save results to save_file_name
    np.savetxt(os.path.join(save_path, save_file_name), val_data_with_pred, delimiter=',', fmt='%s')

    # Save the trained model
    np.save(os.path.join(save_path, "user_embed_matrix.npy"), user_embed_matrix)
    np.save(os.path.join(save_path, "item_embed_matrix.npy"), item_embed_matrix)
    np.save(os.path.join(save_path, "user_bias_matrix.npy"), user_bias_matrix)
    np.save(os.path.join(save_path, "item_bias_matrix.npy"), item_bias_matrix)


### Driver methods

"""
    Driver method to run the training and evaluation process for matrix factorization.

    INPUT = train_data_file (str): File path for the training dataset;
    test_data_file (str): File path for the test dataset;
    save_file_name (str): File name to save the results;
    factors_count (int): Number of factors for matrix factorization;
    batch_size (int): Size of the batch for training iterations;
    epoch_count (int): Number of epochs for training;
    learn_rate (float): Learning rate for gradient descent;
    regularization_term (float): Regularization term to prevent overfitting;
    predict_with_saved_model (bool): Flag indicating whether to predict using a saved model
"""
def run(train_data_file, test_data_file, save_file_name, factors_count, batch_size, epoch_count, learn_rate, regularization_term, predict_with_saved_model):
    train_data = None
    test_data = None

    # Load training and test data
    if not predict_with_saved_model:
        train_data = load_data(train_data_file)
    test_data = load_data(test_data_file)

    # Split into training and validation
    val_data = test_data

    # Get the number of users and items from both datasets
    users_count = max(np.max(train_data[:, 0].astype(int)), np.max(test_data[:, 0].astype(int)))
    items_count = max(np.max(train_data[:, 1].astype(int)), np.max(test_data[:, 1].astype(int)))

    user_embed_matrix = np.random.randn(factors_count, users_count)
    item_embed_matrix = np.random.randn(factors_count, items_count)

    # Initialize bias matrices
    user_bias_matrix = np.random.randn(users_count)
    item_bias_matrix = np.random.randn(items_count)

    # Create directory to save results
    save_path = "results"
    createdir(save_path)

    if predict_with_saved_model:
        predict_with_loaded_model(val_data, save_path, save_file_name)
    else:
        train(train_data, val_data, user_embed_matrix, item_embed_matrix, user_bias_matrix, item_bias_matrix, batch_size, epoch_count, learn_rate, regularization_term, save_path, save_file_name)

if __name__ == '__main__':
    # Constants
    train_file = "train_20M_withratings.csv"
    test_file = "test_20M_withoutratings.csv"
    save_file_name = "results.csv"

    factors_count = 20
    batch_size = 4096
    epoch_count = 75

    learn_rate = 0.0025
    regularization_term = 0.03

    predict_with_saved_model = False

    run(train_file, test_file, save_file_name, factors_count, batch_size, epoch_count, learn_rate, regularization_term, predict_with_saved_model)