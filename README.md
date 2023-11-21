###### CANSSI-Comp
## Summary of the steps taken to prepare the data and create new variables to feed into the binary classification model

I noticed that many horses did not have numerical position data in their history. Their positions in races were regularly marked by one or more of the following values: BS, DQ, FL, NP, PU, UN, UR, WC. Therefore, I excluded all these horses from the dataset because the (numerical) position is essential for the rest of the work.

Some horses consistently had numerical positions in their history, while others had both numerical and non-numerical positions like BS, DQ, etc. For the latter, I replaced all instances where they did not have numerical position with the mode of position they most frequently occupied throughout their career. In other words, I calculated the frequency of all positions they had and replaced instances without numerical position with the most frequent one, which was the position they most often held throughout their career.

I've created a set of variables related to all the participants in the races. Each race involves multiple horses, several jockeys, various trainers, horses from different lineages, etc. These variables aim to capture all these relationships and their impact on a horse's victory in a competition. In general, I've created the following types of variables for each participant in the race (horse, jockey, trainer, SireID, DamId). Thus, for each of these variables, there are five variants:

- **PriorPosition1st**: Number of times an opponent in the race has finished in the first position before this race.
- **PriorNumberOfRaces**: Number of races completed by an opponent before the current race.
- **DelayLastRace**: Duration between the last two races of all opponents in the race. If it's the actor's first race, I've set it as 365.
- **PosLast3**: Sum of positions obtained by the actor in their last 3 races. If it's the first race, I've set it as -3; if it's the second, I've multiplied the position of the first race by 3; if it's the third, I've multiplied the position of the first race by 1.5, the second by 1.5, and summed these values.
- **OppsPriorPos1st**: The total sum of the number of times opponents in the race have won a race before this one (sum of how many times each opponent finished first before the current race).
- **OppsPriorPosLast3**: Similar to OppsPriorPos1st, I've calculated the sum for OppsPriorPosLast3 by taking the values of PosLast3 for each opponent and adding them up per race.
OppsDelayLastRace: Sum of the DelayLastRace of opponents in the same race.
- **NumNAPrior**: Number of times an opponent did not have a numerical position before the race.
- **PrevBeatenMargin**: Margin of defeat in the previous race.
- **PrevPriceSP**: Starting Price (SP) of the previous race (since the SP for the race is available after it, I've taken the one from the previous race).
- **PIRPosition**: Performance Index Rating (PIR) position from the previous race.
- **n_horses**: Number of competitors in the race.
- **PriorSumMoneyWon**: Total sum of money won before the race.
- **PriorSumPos**: Sum of positions obtained before the current race.

Other created variables:

- **RaceOutCome**: 1 if a horse has won the race, 0 otherwise. Note that two horses can win a race if they finish in a tie.
- **HasPosition**: Yes (1) or No (0) indicating if a horse had a numerical position in a race.

Recoded variables:

- **FoalingCountry1**: Indicates the country of birth of the horse; takes the value of 1 if it's France and 0 otherwise.
- **RaceMoney_Cat**: Categorization of the variable RacePrizeMoney into Low, Medium, and High. It's classified as Low when the RacePrizeMoney amount is less than or equal to the 33rd percentile of the overall distribution of RacePrizeMoney for all races, as 'Medium' if it falls between the 33rd and 66th percentile, and High if it exceeds the 66th percentile.
- **Distance_Cat**: Categorization of the distance to be covered into Short, Medium, and Long. It's considered Short when the distance to be covered is less than or equal to the 33rd percentile of the distribution of distances for all races, 'Medium' if it's between the 33rd and 66th percentile, and Long if it exceeds the 66th percentile.
- The same type of categorization is applied to **Saddlecloth_Cat** and **WideOffRail_Cat**.

- **StartingLine_Cat**: Recoding of StartingLine where -1 corresponds to "F", 1 to "G", and 2 to "H".
- **WetnessScale_Cat**: Recoding of WetnessScale where 3 corresponds to "G", 1 to "F", 4 to "G", 7 to "H", and 9 to 'H’.
- **NoFrontCover_Cat**: Recoding of NoFrontCover where -9 corresponds to "F", 0 to "G", and 1 to "H".

The following variables were used with minimal modification:

**Surface**, **StartType**, **RacingSubType**, **Gender**, **Barrier**, **FrontShoes**, **HandicapDistanc**e, **HindShoes**, **HorseAge**, **WeightCarried**.

#### Handling class imbalance in the training set

I followed the competition guidelines by splitting the dataset into two parts: one ending on October 30, 2021, serving as the training set, and the other starting after this date, forming the test dataset. We identified an imbalance where one class was overrepresented compared to the other: 1,028,949 occurrences for class 0 and 137,595 occurrences for class 1. To address this situation, we applied over and under-sampling methods to the training set.
Initially, we used the under-sampling method by randomly selecting a number of observations from the majority class equal to the number of observations in the minority class to create a balanced new training set. Then, due to insufficient computational resources to train a model after over-sampling the minority class to the same level as the majority class, we opted for a mixed method of over and under-sampling. We over-sampled the minority class to 583,272 occurrences (approximately 4 times the initial number of observations in this class) and under-sampled the majority class to the same level.
Ultimately, we obtained two (training) datasets on which we trained the model. The one that yielded the best performance was where the under-sampling was performed, achieving a performance of over 80%. It's with the predictions from this model that I calculated the predicted probabilities to then create the 'win_probability' column, which I will detail the steps for immediately after.

### Calculation process for the 'win_probability' column

After training the model, I computed the predicted probabilities for the test dataset. This process generated an array containing the probability for each horse to win the race, meaning to be classified in category 1 of the dependent variable.

Initially, the sum of these probabilities per race exceeded 1. To adjust this sum to be equal to 1 per race, I normalized the probabilities using the softmax function. This function calculates the exponential of each probability and then divides this value by the sum of the exponentials of all probabilities per race. These new probabilities are then saved in the 'win_probability' column of the win_probability.parquet [file](https://github.com/JBobyM/CANSSI-Comp/blob/main/win_probability.parquet), representing the column to predict in the competition.

### The Model

The model I've developed is a binary classification neural network implemented using TensorFlow. It operates on the 'RaceOutcome' variable, where '1' indicates a horse winning the race and '0' denotes otherwise. The explanatory variables encompass all discussed features, including continuous variables that were standardized and categorical variables that underwent one-hot encoding. After standardizing the continuous variables and performing one-hot encoding for categorical ones, the finalized model comprises 92 explanatory variables. The training dataset contains 275,190 observations, evenly distributed with 137,595 instances for each class of the dependent variable.

Initially, the dataset preparation was conducted using R. However, due to technical issues encountered when attempting to download Keras in R, I transitioned to Python to execute the model training and evaluation.

### Enhancing Horse Race Predictions: Balancing Precision and Sensitivity
During the model training and hyperparameter tuning, I deliberately opted to prioritize precision over sensitivity for the positive class (1). I specifically chose the model's hyperparameters that minimize false positives—instances where the model wrongly predicts horses as winners when they've actually lost. I made this decision to avoid potential financial losses, such as betting on a horse the model predicted would win but ultimately didn't, or other negative consequences.

This choice somewhat impacted sensitivity. It was challenging to simultaneously minimize false negatives—cases where the model predicted horses wouldn't win when they actually did. Nonetheless, the model maintains an acceptable sensitivity rate while retaining over 80% precision. Overall, the classification performance exceeds 90%.
