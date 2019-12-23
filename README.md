# Customer Churn Reduction
Acquiring new customers costs more than maintaining current customers. When a current
customer is lost, we lose future revenue and also the resources and investment that was
spent to acquire that customer.
## Objective
* Our goal is to create a logistic regression model that accurately predicts the potential churn of
customers (true positives) and has minimum misidentifications (false positives).
* Once the customers who are most likely to churn are identified, a targeted approach can be used
to address the factors that most influence churn.
### Data Overview
*A preliminary look at the data shows our overall churn rate is 14.5%. Customers with an International Plan have a much higher churn rate than the general population(42.4%).Those with a voicemail plan have a smaller churn rate than the general population (8.68%)*
![bargraph1](https://user-images.githubusercontent.com/54346057/71378823-3eac7380-2597-11ea-824d-787d4a7debc0.JPG)

### Implementing Logistic Regression
*A model is only ever as good as the data that drives it. For that reason, it is essential to spend time investigating the properties of your data and whether any manipulation may be in order prior to beginning the modeling process.Complete Analysis is performed using R*
![Churngif](https://user-images.githubusercontent.com/54346057/71378867-58e65180-2597-11ea-96cf-3b8c07fefded.gif)

### Estimation of the model’s performance

*There are so many ways one can access the performance of the logistic regression model. we have considered the ROC curve and AUC*
![ROC](https://user-images.githubusercontent.com/54346057/71378914-74e9f300-2597-11ea-86ca-63a200e617ab.JPG)
*In the ROC plot, you want your points on the curve to get closer to the northwest (0,1) for your model to be more accurate. The closer your points are to the diagonal line, the less accurate your model is. AUC is the area under the curve of ROC.*
![RocPlot](https://user-images.githubusercontent.com/54346057/71378917-76b3b680-2597-11ea-802e-0659ef6b2c41.JPG)
### Insights and Conclusions
*Focus on factors most influencing churn.Identify areas where we can make improvements that will make our customers want to stay.*

![conclusion](https://user-images.githubusercontent.com/54346057/71378921-787d7a00-2597-11ea-8455-6253a1a1f386.JPG)
