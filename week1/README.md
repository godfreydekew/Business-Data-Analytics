## Week 1 Mind Map
![Week 1 mind map](/week1/images/week1_mindmap.png)

## Class Brief Notes

```R
    plot(mDrug)
    lines(mDrug, col = "red")
    lines(mPlacebo, col = "green")
```

## Result
You can clearly see the randomness: sometimes the drug is better than the placebo, and vice versa. That’s why we cannot rely solely on descriptive statistics. Essentially, we are flipping a coin. Also note that the average of these means will be close to the population mean.

![Week 1 result plot](/week1/images/plot1.png)


## Seminar Explanation

But what if we explain this with a real-life example?

Which chicken feed makes fatter chickens? That was Mr. Roger asking himself. As an educated farmer, he decided to calculate the mean of feed A and feed B to see which one is much better.

![Feed comparison data](/week1/images/sem1.png)

### Results
- Feed A mean: 142.95
- Feed B mean: 135.26

### Hypothesis Testing
![Hypothesis testing diagram](/week1/images/sem2.png)

So basically, feed A is a better feed, right? This represents the alternative hypothesis.

However, if we take two random samples of feed A and calculate their means, they will be different. Why are they different?

It's called **naturally occurring randomness**. So how on earth do we know which one is better? We use inferential statistics.

### Conclusion
![Statistical conclusion](/week1/images/sem3.png)
