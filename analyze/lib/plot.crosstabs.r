plot.crosstabs <- function(crosstabs.real, crosstabs.fake.slice){
    odds <- subset(crosstabs.real,
        sex == crosstabs.fake.slice[1,'sex'] & 
        task == crosstabs.fake.slice[1,'task'] & 
        privacy == crosstabs.fake.slice[1,'privacy'] & 
        cleanliness == crosstabs.fake.slice[1,'cleanliness']
    )[1,'odds']
    
    plot.fake <- ggplot(crosstabs.fake.slice) + aes(x = odds) + geom_histogram() +
        labs(title = paste(sapply(crosstabs.fake.slice[1,id.vars], as.character), collapse = ', ')) +
        #scale_x_log10('Odds')
        scale_x_continuous('Odds')

    plot.fake + geom_vline(xintercept = odds)
}
