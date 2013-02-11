library(pvclust)
library(scales)

# Categorize postures as clean or dirty.
posture.df$dirtyposture <- 0
posture.df$dirtyposture[posture.df$posture %in% c('hover', 'other')] <- 1
posture.df$dirtyposture[posture.df$posture == 'stand' & posture.df$sex == 'female'] <- 1

# Male as binary
posture.df$male <- posture.df$sex == 'male'

# Make multivariate
posture.df.multivariate <-  dcast(posture.df[c('participantId', 'male', 'cleanliness', 'task', 'privacy', 'dirtyposture')], participantId + male ~ cleanliness + task + privacy, value.var = 'dirtyposture')
rownames(posture.df.multivariate) <- posture.df.multivariate$participantId
posture.df.multivariate$participantId <- NULL

# Separate by sex
posture.df.multivariate.female <- subset(posture.df.multivariate, !male)
posture.df.multivariate.male <- subset(posture.df.multivariate, male)
posture.df.multivariate.male$male <- posture.df.multivariate.female$male <- NULL

# Cluster
cluster <- function(df.multivariate, k = 3, display = F) {
    d <- dist(df.multivariate, method = "euclidean") # distance matrix
    fit <- hclust(d, method="ward") 

    groups <- cutree(fit, k=k) # cut tree into 5 clusters

    if (display == T) {
        print('plotting')

        # display dendogram
        plot(fit)

        # draw dendogram with red borders around the k clusters 
        r <- rect.hclust(fit, k=k, border="red")

        # Write group numbers
        text(
            cumsum(sapply(r,length)) - sapply(r, length)/2, 
            rep(0, length(r)),
            paste(unique(groups[fit$ord])),
            font = 2, col = 2, cex = 3
        )
    }

    # Cluster centers
    centroids <- sapply(unique(groups), function(group) {
        colMeans(df.multivariate[groups==group,])
    })
    
    list(
        fit = fit,
        d = d,
        groups = groups,
        centroids = centroids
    )
}

explore <- function() {
    # Hover when dirty and urinating, hover when public and urinating, hover only when it's really bad.
    cluster(posture.df.multivariate.female, 3)
    
    # The new groups separate whether people hover when it's clean.
    cluster(posture.df.multivariate.female, 4)
    
    # Add a "never hover" group.
    cluster(posture.df.multivariate.female, 5)
    
    
    
    # Hover or not?
    cluster(posture.df.multivariate.male, 2)
    
    # Hover only when it's dirty?
    cluster(posture.df.multivariate.male, 3)
    
    # Who are these people for which action doesn't matter?
    a <- (cluster(posture.df.multivariate.male, 4)$groups == 4)
    names(a[a])
    
    # These seven people contain four-fiths of the 'other' postures for males.
    sum(subset(posture.df, participantId %in% names(a[a]))$posture == 'other')
    sum(subset(posture.df, sex == 'male' & !(participantId %in% names(a[a])))$posture == 'other')
    
    # So the new group is people who say 'other', presumably
    # because they won't consider using the toilet.
    cluster(posture.df.multivariate.male, 4)
}

# Custom distance functions
prop.persons.wrong <- function(centroid, test.data.row) {
    sum((centroid - test.data.row)^2) / 12
}

prop.questions.wrong <- function(centroid, test.data.row) {
    sum(abs(centroid - test.data.row) / 12)
}

# Compute the distance validation function on the folded data.
validate <- function(training.data, test.data, k, prop.dist.func) {
    # training.data and test.data are data frames.
    # dist.func compares a row to a data frame.
    cl <- cluster(training.data, k)
    distances <- apply(test.data, 1, function(test.data.row) {
        min(apply(cl$centroids, 2, function(centroid) {
            prop.dist.func(centroid, test.data.row)
        }))
    })
    1 - mean(distances)
}


# Measure distance between centroids and test data.
# Return the average correctness of a person (between 0 and 1)
# and the average number of questions correct per person (between 0 and 12)
simulate.once <- function(full.df, k) {
    x <- sample.int(nrow(full.df), size = nrow(full.df) * 4 / 5)
    training.data <- full.df[x,]
    test.data <- full.df[-x,]
    c(
        person = validate(training.data, test.data, k, prop.persons.wrong),
        question = validate(training.data, test.data, k, prop.questions.wrong)
    )
}

# Run the version that is easy to interpret.
small.words <- function() {
    # Run the cross validations
    set.seed(823742)

    # So inefficient
    for (k in 2:10) {
        cross.validation.all <- adply(1:100, 1, function(i) {
            simulate.once(posture.df.multivariate[-1], k)
        })[-1]

        p.person <- ggplot(cross.validation.all) + aes(x = person) + geom_histogram(binwidth = 0.025) +
            scale_x_continuous(
                paste(
                    'Proportion of people correct (1 - (SSE / 12))',
                    paste('If the model (k = ', k, ')were completely irrelevant, we would expect the data to be centered at 75%.', sep = ''),
                    sep = '\n'
                ),
                labels = percent, limits = 0:1
            ) +
            scale_y_continuous('Number of simulations') +
            labs(title = 'How well does the model fit? Distances from centroids in five-fold cross-validation')

        p.question <- ggplot(cross.validation.all) + aes(x = question) + geom_histogram(binwidth = 0.025) +
            scale_x_continuous(
                paste(
                    'Proportion of questions correct (1 - (sum of absolute error) / 12)',
                    paste('If the model (k = ', k, ')were completely irrelevant, we would expect the data to be centered at 50%.', sep = ''),
                    sep = '\n'
                ),
                labels = percent, limits = 0:1
            ) +
            scale_y_continuous('Number of simulations') +
            labs(title = 'How well does the model fit? Distances from centroids in five-fold cross-validation')

        png(paste('graphs/cluster.fit.question', k, 'png', sep = '.'), width = 1200, height = 600)
        print(p.question)
        dev.off()
        png(paste('graphs/cluster.fit.person', k, 'png', sep = '.'), width = 1200, height = 600)
        print(p.person)
        dev.off()
    }
}

# Using a package that already does all that
# http://www.is.titech.ac.jp/~shimo/prog/pvclust/

# Which scenario distinctions aren't very distinguishable?
# fit.scenario <- pvclust(posture.df.multivariate[-1], nboot = 100, method.dist = 'euclidean')

# Which people act similarly?
fit.participant <- pvclust(t(posture.df.multivariate[-1]), nboot = 10000, method.dist = 'euclidean', method.hclust = 'ward')

# Explore
annotate.k <- function(fit, k, group.names = 1:k, df = posture.df.multivariate) {
    plot(fit$hclust, axes = F, xlab = '', ylab = '', sub = '',
         main = paste(k, 'clusters of participants')
    )
    r <- rect.hclust(fit$hclust, k)
    groups <- cutree(fit$hclust, k = k)

    # Write group numbers
    # x <- weighted.mean(rev(fit$hclust$height)[c(k - 1, k)], c(1, 2))
    y <- rev(fit$hclust$height)[k] + 2
    text(
        cumsum(sapply(r,length)) - sapply(r, length) / 3, 
        rep(y, length(r)),
        group.names[unique(groups[fit$hclust$order])],
        font = 1, col = 2, cex = 1, srt = -60, pos = 2
    )
    centroids <- sapply(unique(groups), function(group) {
        colMeans(df[groups==group,])
    })
    list(
        centroids = centroids,
        groups = groups
    )
}

print('Sex is returned in the centroids, but it\'s not used for the clustering.')

a.2 <- function() annotate.k(fit.participant, 2, c('Mostly males', 'Mostly females'))

a.3 <- function() annotate.k(fit.participant, 3, c('Mostly males', 'Females', 'Males who hover when defecating at dirty toilets'))

a.4 <- function() annotate.k(fit.participant, 4, c(
  'Mostly males',
  'Mostly females',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Females who hover, &c. only at dirty toilets'
))

a.5 <- function() annotate.k(fit.participant, 5, c(
  'Mostly males',
  'Males who always use the standard (sit/stand) posture',
  'Mostly females',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Females who hover, &c. only at dirty toilets'
))
a.6 <- function() annotate.k(fit.participant, 6, c(
  'Mostly males',
  'Males who always use the standard (sit/stand) posture',
  'Mostly females',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who hover, &c. only at dirty toilets'
))

a.7 <- function() annotate.k(fit.participant, 7, c(
  'Mostly males',
  'Males who always use the standard (sit/stand) posture',
  'Mostly females',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only at dirty toilets'
))
#subset(posture.df, participantId %in% names(a.7$groups)[a.7$groups == 3])
#subset(posture.df, participantId %in% names(a.7$groups)[a.7$groups == 6])

a.8 <- function() annotate.k(fit.participant, 8, c(
  'Mostly males',
  'Males who always use the standard (sit/stand) posture',
  'Mostly females who hover, &c., except when urinating at private, non-dirty toilets',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only at dirty toilets',
  'Females who hover, &c., except when urinating at private, non-dirty toilets'
))
a.9 <- function() annotate.k(fit.participant, 9, c(
  'Mostly males',
  'Males who always use the standard (sit/stand) posture',
  'Mostly females who hover, &c., except when urinating at private, non-dirty toilets',
  'Females who hover when toilets are unpleasant',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only at dirty toilets',
  'Females who hover, &c., except when urinating at private, non-dirty toilets'
))
a.10 <- function() annotate.k(fit.participant, 10, c(
  'Mostly males',
  'Males who always use the standard (sit/stand) posture',
  'Females who hover at dirty toilets and when urinating at unspecified-cleanliness toilets',
  'Females who hover when toilets are unpleasant',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Mostly females who hover, &c., except when urinating at private, non-dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only at dirty toilets',
  'Females who hover, &c., except when urinating at private, non-dirty toilets'
))

a.11 <- function() annotate.k(fit.participant, 11, c(
  'Mostly males',
  'Males who always use the standard (sit/stand) posture',
  'Females who hover at dirty toilets and when urinating at unspecified-cleanliness toilets',
  'Females who hover when toilets are unpleasant',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Mostly females who hover, &c., except when urinating at private, non-dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only at dirty toilets',
  'Males who said they used "other" postures a lot',
  'Females who hover, &c., except when urinating at private, non-dirty toilets'
))
# The new group sounds weird. It's because they marked "other".
# print(subset(posture.df, participantId %in% names(a.11$groups)[a.11$groups == 1]))
# print(subset(posture.df, participantId %in% names(a.11$groups)[a.11$groups == 10]))
# print(sum(subset(posture.df, participantId %in% names(a.11$groups)[a.11$groups == 10])$posture == 'other'))
# print(sum(posture.df$posture == 'other'))

a.12 <- function() annotate.k(fit.participant, 12, c(
  'Mostly males',
  'Males who always use the standard (sit/stand) posture',
  'Females who hover at dirty toilets and when urinating at unspecified-cleanliness toilets',
  'Females who hover when toilets are unpleasant',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Mostly females who hover, &c., except when urinating at private, non-dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only when urinating at dirty toilets',
  'Females who hover, &c. only at dirty toilets',
  'Males who said they used "other" postures a lot',
  'Females who hover, &c., except when urinating at private, non-dirty toilets'
))

a.13 <- function() annotate.k(fit.participant, 13, c(
  'Mostly males',
  'Males who always use the standard (sit/stand) posture',
  'Females who hover at dirty toilets and when urinating at unspecified-cleanliness toilets',
  'Females who hover when toilets are unpleasant',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Mostly females who hover, &c., except when urinating at private, non-dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only when urinating at dirty toilets',
  'Females who hover, &c. only at dirty toilets',
  'Males who said they used "other" postures a lot',
  'Females who hover, &c., except when urinating at private, non-dirty toilets',
  'Females who hover, &c., except when urinating at private, non-dirty toilets'
))

a.14 <- function() annotate.k(fit.participant, 14, c(
  'Mostly males',
  'Males who always use the standard (sit/stand) posture',
  'Females who hover at dirty toilets and when urinating at unspecified-cleanliness toilets',
  'Females who hover when toilets are unpleasant',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Mostly females who hover, &c., except when urinating at private, non-dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only when urinating at dirty toilets',
  'Females who hover, &c. only at dirty toilets',
  'Males who said they used "other" postures a lot',
  'Females who hover, &c., except when urinating at private, non-dirty toilets',
  'People who always use dirty postures (including "other")',
  'Females who hover, &c., except when urinating at private, non-dirty toilets'
))
# print(subset(posture.df, participantId %in% names(a.14$groups)[a.14$groups == 13]))

a.15 <- function() annotate.k(fit.participant, 15, c(
  'People who are more likely to hover when defecating',
  'Males who always use the standard (sit/stand) posture',
  'Females who hover at dirty toilets and when urinating at unspecified-cleanliness toilets',
  'Females who hover when toilets are unpleasant',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Mostly females who hover, &c., except when urinating at private, non-dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only when urinating at dirty toilets',
  'Females who hover, &c. only at dirty toilets',
  'Males who said they used "other" postures a lot',
  'Females who hover, &c., except when urinating at private, non-dirty toilets',
  'People who always use dirty postures (including "other")',
  'Males who only hover in when defecating at a dirty public toilet',
  'Females who hover, &c., except when urinating at private, non-dirty toilets'
))

a.16 <- function() annotate.k(fit.participant, 16, c(
  'People who are more likely to hover when defecating',
  'Males who always use the standard (sit/stand) posture',
  'Females who hover at dirty toilets and when urinating at unspecified-cleanliness toilets',
  'Females who hover when toilets are unpleasant',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Mostly females who hover, &c., except when urinating at private, non-dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only when urinating at dirty toilets',
  'Females who hover, &c. only at dirty toilets',
  'Males who said they used "other" postures a lot',
  'Females who hover, &c., except when urinating at private, non-dirty toilets',
  'People who always use dirty postures (including "other")',
  'Males who only hover in when defecating at a dirty public toilet',
  'Females who hover, &c., except when urinating at private, non-dirty toilets', # Categorize more?
  'Females who hover, &c., except when urinating at private, non-dirty toilets' # Categorize more?
))

a.17 <- function() annotate.k(fit.participant, 17, c(
  'Males who mostly sit unless they\'re defecating at a dirty toilet',
  'Males who always use the standard (sit/stand) posture',
  'Mostly females who mostly sit unless it\'s a dirty public toilet.',
  'Females who hover at dirty toilets and when urinating at unspecified-cleanliness toilets',
  'Females who hover when toilets are unpleasant',
  'Males who hover, &c. only when defecating at dirty toilets',
  'Mostly females who hover, &c., except when urinating at private, non-dirty toilets',
  'Females who hover, &c. only when urinating',
  'Females who always hover, &c.',
  'Females who hover, &c. only when urinating at dirty toilets',
  'Females who hover, &c. only at dirty toilets',
  'Males who said they used "other" postures a lot',
  'Females who hover, &c., except when urinating at private, non-dirty toilets',
  'People who always use dirty postures (including "other")',
  'Males who only hover in when defecating at a dirty public toilet',
  'Females who hover, &c., except when urinating at private, non-dirty toilets', # Categorize more?
  'Females who hover, &c., except when urinating at private, non-dirty toilets' # Categorize more?
))

pdf('graphs/annotated.clusters.pdf', width = 14, height = 8.5)
par(xpd = T, mar = c(3, 5, 4, 2) + 0.1)
a.2()
a.3()
a.4()
a.5()
a.6()
a.7()
a.8()
a.9()
a.10()
a.11()
a.12()
a.13()
a.14()
a.15()
a.16()
a.17()
dev.off()

always.clean <- ddply(posture.df.multivariate, 'male', function(df) {
    sum(rowSums(df[-1]) == 0) / nrow(df)
})

# These groupings ignore the urination/defecation relationship:
# Males are more likely to hover when defecating, and 
# females are more likely to hover when urinating.

manual.groupings.8 <- data.frame(
    sex = c('female', 'male'),
    hover.never = always.clean[,2], # No group for females, group 2 for males
    hover.only.when.bad = c(24 / 103, 30 / 69), # Group 7 and group 4
    hover.sometimes = c((39 + 14 + 9) / 103, 22 / 69), # Groups 3, 5 & 8 and group 1
    hover.nearly.always = c(9 / 103, 0) # Group 6 for females
)

# Based on the above
manual.groupings.english.8 <- data.frame(
    sex = c('female', 'male'),
    hover.never = c('hardly any', 'one-fourth'),
    hover.only.when.bad = c('one-fifth', 'two-fifths'),
    hover.sometimes = c('one-half', 'one-third'),
    hover.nearly.always = rep('hardly any', 2)
)

# manual.groupings.17 <- data.frame(
#     sex = c('female', 'male'),
#     hover.never = always.clean[,2],
#     hover.when.bad = c(24 / 103, 30 / 69),
#     hover.when.soso = c((9 + )/103, /69),
#     hover.always = c(/103, /69)
# )

.ndirty <- data.frame(
    count = rowSums(posture.df.multivariate[-1]),
    male = posture.df.multivariate$male
)
ndirty <- dcast(.ndirty, count ~ male, value.var = 'male', fun.aggregate = length)
names(ndirty) <- c('n.dirty', 'female', 'male')
ndirty$female <- ndirty$female / 103
ndirty$male <- ndirty$male / 69

p.ndirty <- ggplot(melt(ndirty, 'n.dirty', variable.name = 'sex', value.name = 'p')) +
    aes(x = n.dirty, y = p, color = sex, group = sex) +
    scale_y_continuous('Proportion of participants', labels = percent) +
    scale_x_continuous('Number of questions where a dirty posture was marked', breaks = 0:12, labels = 0:12) +
    geom_line() +
    geom_text(x = 0.5, y = 0.35, label = 'Always sit\n(or stand for males)', color = 'black') +
    geom_text(x = 3.5, y = 0.3 , label = 'Normally sit\n(or stand for males)', color = 'black') +
    geom_text(x =11.5, y = 0.1 , label = 'Rarely sit', color = 'black') +
    labs(title = 'Within-person rates of dirty postures')
