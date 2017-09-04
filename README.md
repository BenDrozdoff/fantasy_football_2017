<h1>Fantasy Football 2017</h1>

<h4>An overly simplistic Version 0 of a fantasy football projection system and fantasy auction value generator</h4>

<h5>How to Use</h5>

Alter the league parameter variables in the R script, alter or mentally reaffirm the usage rates in usage_priors.csv, and then run the script.  The qb_rankings, rb_rankings, wr_rankings, and te_rankings will have positional rankings, and the compiled_rankings will have the overall rankings and fantasy auction values, if applicable. 

Feedback, constructive criticism, and suggestions for future verisons are always appreciated!

<h5>Why This System:</h5>

The primary features that my projection offers that, as far as I can tell, most free, publically available projection systems do not are:

1) Projecting week-by-week rather than the entire season
2) Adjusting for defenses
3) Flexibility based on beliefs about playing time
4) 2 years of data weighted exponentially by recency
5) Option to adjust for inflation due to keepers

Schedule has a substantial impact on fantasy output--thus, it makes sense to look at training data in this context, and predict for future games in this context.  However, any regression based model will be unsuccessful in doing this. Enter mixed effects models--I will defer to <a href="http://www.baseballprospectus.com/article.php?articleid=25514">Jonathan Judge</a> for an explanation of how they work, as this article (and all of his work at Baseball Prospectus) inspired my interest this statistical technique.  

<h4>Model Specifics</h4>

Using the 2016 and 2015 seasons as training data (weighting for recency), I modelled the following statistics at a game level, given the offensive team, defensive team, and stadium:

Rush Attempts<br/>
Rush Yards<br/>
Rushing TDs<br/>
Passing Attempts<br/>
Passing YDs<br/>
Passing TDs<br/>
Interceptions<br/>

Using these models, I predicted the numerical output of each statistic for every game in the 2017 NFL season (except week 17). I then examined the distribution of these statistics at an individual level relative to the rest of the team to create expected outcomes for this year.  While I would have loved to have used a strictly data driven model to determine the usages, the amount of metadata required (draft, salaries, injuries, etc.) seemed well beyond the scope of V0.  Plus, this method allows for the flexibility mentioned above. Think Alshon Jeffery will get a higher share of targets than what I have allocated? Now we can quantify the extent to which his value should be increased. Feel free to adjust these values, and alert me if you think I've made any egregious errors.

<h4>Model Weaknesses</h4>

As legendary statstician George Box once said, "All models are wrong, but some are useful." While I can only hope to achieve the lofty standard for utility in the fantasy football space, I want to be as transparent as possible about the weaknesses of this model. I hope that these do not dissuade you from the overall utility of the model, and instead provide you with context as to how to use this information.

<h5>Coaching / Scheme Changes</h5>

This model is trained on prior season, so it will have no way of knowing if a team plans to run more or pass less than it did in past seasons.  Additionally, injuries and personnel differences are a factor here as well.  A team that just picked a RB in the first round is probably going to run more than it did last year, but my model has no way of knowing this.  

<h5>Unit Effectiveness</h5>

Along the same lines, the projections for how effectively Leonard Fournette will run the ball this year are built on how effectively TJ Yeldon and Chris Ivory ran last year.  One can reasonably expect that the Jaguars will run the ball more frequently and more effectively than they did last year, given this investment of draft capital, but in its current state my model cannot predict if Leonard Fournette will be an effective RB. However, we do know that (in all likelihood) a large portion of the rushing unit (namely the offensive line) has some carryover from year to year. Knowing the extent to which the RB influences effective rushing as opposed to the offensive line is a much harder problem to solve, and is currently omitted in my system.  

<h5>Only Two Players Per Position Per Team</h5>

This can be changed relatively easily, but it's entirely possible that one team's RB3 is more valuable than another's RB2. This system should be fine for regular 12 team leagues, and for the really late picks in deeper leagues I tend to rely on upside more than expected value, but this is nonetheless a weakness I want to be transparent about.

<h4>Takeaways From This Model</h4>

This model predicts fantasy points as though it were week 18 of 2016. You can mentally put a larger error bar on players and teams who have had the most coaching and personnel changes between the end of last year and today.

<h4>Special Thanks</h4>

Maksim Horowitz and the NFLScrapR package--none of this would be possible without this JSON parsing package that I highly recommend.

Chang-Hsin Lee for answering countless statistics and economics questions in building this, as well as being a generally awesome mentor to me.

Chad Marxen and Adam Conrad for discussing at a high level how we would build an ideal projection system and fantasy auction value generator, and motivating me to build this to beat both of you in our leagues ;)

