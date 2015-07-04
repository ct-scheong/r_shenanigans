This R script consists of 2 algorithms, <b>Perceptrain</b> and <b>Classify</b>.

Perceptrain(S,y) takes a two-class set of linearly separable data and generates a a hyperplane classifier (z) trained by a Perceptron algorithm as well as a running history of the iterations until convergence was reached.

Classify(S,z) takes the same data set, along with the hyperplane classifier (z) and outputs the resulting labels for each data point.

Here are some plots showing the algorithm in action over some randomly generated data (separated into test and training subsets).

<img src="https://raw.githubusercontent.com/datatista/machinelearningfun/master/linearclassification/LC_perceptron1.png">
