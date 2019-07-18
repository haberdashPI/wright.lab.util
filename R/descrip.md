The key point to remember is that, for a data set with the columns day, sid, pretest, condition and threshold, we want to find:

     threshold - slope.threshold * (pretest - mean(pretest))

The way I've written this is a bit messy because I wasn't as familiar with how to elegantly manipulate data in R when I wrote this code. Let me describe it, step by step.

For this description I'll reference an example using the following call.

    covariate.adjust(df,'day','threshold','pretest')

Okay, on to the function definition:

    covariate.adjust <- function(df,x,y,covariate,prefix='adj.'){

The first thing we do is set up the name of a variable for the slope we're going to calculate. In the example above, slope.name = 'slope.threshold'.

      slope.name = paste(prefix,'slope.',y,sep='')

We then estimate a separate slope for the variables threshold (y) and pretest (covariate) for each day (x). For instnace, for the post-test day, this is just the slope betwene the pre- and post-test (which is what we normally plot in our pre- vs. post-test plots). This is done using `ddply` which applies the given function to each subset of `df` with a different value for `x`.

      slopes = ddply(df,x,function(df){
      
The first step to find the slope is to perform a regression using `lm`. The below call to lm would be equivalent, in the working example, to `lm(threshold ~ pretest)`. We then use `coef(...bla bla...)[2]` to extract the second coefficient of the regression (the slope).

        df = data.frame(slope=
            coef(lm(reformulate(response = y, termlabels=covariate),df))[2])

Then we rename the column so it matches the slope.name we defined above ("slope.threshold").

        colnames(df) = slope.name
        df
      })

After calculating the slopes, we add the slopes to the data using `merge`. Merge combines two data frames by matching the columns that are shared, and adding columns that are not, repeating them if the shared columns are the same. Since we calculated a different slope for each day, merging these slopes with `df` will add a new column (`slope.threshold`) that will be the same for all rows with the same day (but different for rows with different days).

      df = merge(df,slopes)

Now that we've calculated a slope between threshold and pre-test for each day we can correct for the pre-test. First we create a new column called 'mean.pretest' (`paste('mean',covariate,sep='.')`) and set it equal to the mean of the pretest.

      df[,paste('mean',covariate,sep='.')] = mean(df[,covariate],na.rm=TRUE)

Then, finally, we correct for the pre-test. The below code is equivalent to the description I provided at beginning for the example.

      df[,paste(prefix,y,sep='')] = df[,y] - df[,slope.name] *
        (df[,covariate] - mean(df[,covariate],na.rm=TRUE))
      df
    }
