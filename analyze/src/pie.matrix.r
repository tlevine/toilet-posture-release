pie.matrix.rough <- ggplot(subset(posture.df, cleanliness == 'unspecified')) + aes(x = posture, fill = sex) + geom_bar() + facet_grid(sex ~ task + privacy) + coord_polar(theta = 'x')

pie.matrix.pretty <- pie.matrix.rough + theme(axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

pie.matrix.decision.tree <- ggplot(posture.df) +
    aes(x = (posture == 'hover'), fill = sex) + geom_bar() +
    facet_grid(sex ~ task + acceptable) + coord_polar(theta = 'x') +
    theme(
        axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()
    )

#pdf('graphs/pie.matrix.pdf', width = 11, height = 8.5)
#print(pie.matrix.pretty)
#dev.off()
