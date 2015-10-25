This is a deep dive analysis into the user click behavior for a Email Digest website. There are 2 datafiles, one is a user database containing a record of each article sent in each digest, along with details on the article such as type (blog, news, etc.) and category (finance, consumer tech, etc.).
The second file is an Apache log file with a string record for each instance that an article has been clicked by a user.

<b>Tools Used:</b>

I used SQLite Studio to read in the database file and export a CSV file, and R Studio to analyze and clean both data files.

<b>Techniques Used:</b>

I first delimited the Apache log file to extract the pertinent information, such as article id and submission time. Then I cleaned the date formats on both datasets and joined them in R using the user_id as the key. Then I use kmeans clustering to group users together based on similar clicking habits.  There might be users who like finance articles, or users who like tech news, etc. I ended up going with 10 user groups. 

To actually analyze the data, I first tried the Naïve Bayes classifier because it is tolerant of categorical variables with a large number of categories, and also gives a probability for each prediction in addition to the label. I found the false negative rate from the Naïve Bayes model to be too high if I classify probabilities p>0.5 as clicked, but if I lower the threshold, it gives me a high false positive rate as a tradeoff.  At a threshold of p = 0.4, I get a 15.9% false positive rate and a 56.2% false negative rate.

Given the skewed distribution of clicks to non-clicks for each article sent to each user the NB algorithm would be more likely to predict an article to be not clicked, even if it was actually clicked. I tried Adaboost next. Adaboost also gives you a probability for each prediction, and has a learning aspect to the method, which makes it more robust than Naïve Bayes. The Adaboost model gives a false positive rate of 14.9% and a false negative rate of 39.8%, which gives us an overall error rate of 15.7%.

<b>Analysis:</b>

On a week by week basis, the overall click rate for article links fluctuate between a max of 16.67% and a min of 15.94%, and the overall trend from the beginning of January to the end of March appear to be constant. We will therefore need to examine what is the cause of the fluctuations.

<img src="https://github.com/datatista/machinelearningfun/blob/master/adaboost/plot3.png">

Combining the type and the topic for each article id, we first come up with a combined grouping called “article type”. An example of an article type would be Business Development Blog Post. Even though Biz Dev News was in the top 5, it has a much lower click rate than the other article types. Thus, these types of articles should not be sent as frequently. 

<img src="https://github.com/datatista/machinelearningfun/blob/master/adaboost/table1.png">
<img src="https://github.com/datatista/machinelearningfun/blob/master/adaboost/plot5.png">

Next, we take a look at the top article types with the highest click rates. These articles have a much higher than overall click rate of about 16.5%. The question now remains: why is there such a discrepancy? It may be that there is not a high enough level of customization in the email digests being sent.

<img src="https://github.com/datatista/machinelearningfun/blob/master/adaboost/table2.png">
<img src="https://github.com/datatista/machinelearningfun/blob/master/adaboost/plot4.png">

An Adaboost model provides the predicted likelihood that a user would click an article link, based on factors such as type, topic, week, and user group. We can fine-tune the customization for each user using this clustering method, and applying a classification model such as Adaboost to determine the top 5 or top 10 article types preferred by each group.

<img src="https://github.com/datatista/machinelearningfun/blob/master/adaboost/table3.png">

<b>Conclusion:</b>

There are not enough articles on Public Finance being sent out to users, despite a very favorable click rate. Try to source more of these articles, so they can be included at a more frequent basis. On the other hand, articles on Business development news have a below average click rate, so these should not be included as frequently.

Furthermore, I would like to see data on the date in which a user signs up for the email digest. This would help me determine if the age of a user’s account has an effect on their likelihood to click on an article. Similarly, I would like to see the date in which past users unsubscribe from the email. There may also be a correlation between the types of articles sent that day and their choice to unsubscribe. This will help increase the retention rate of the subscriber base.
