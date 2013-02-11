paper.table <- dcast(posture.df, sex + task + privacy + cleanliness ~ posture)

n.male <- rowSums(subset(paper.table, sex == 'male')[5:8])[1]
n.female <- rowSums(subset(paper.table, sex == 'female')[5:8])[1]

note.percent <- function(cell, n) {
    sprintf('%d (%d%%)', cell, round(100 * cell / n))
}

paper.table.book <- paper.table

for (posture in unique(posture.df$posture)) {
    paper.table.book[paper.table$sex == 'male',posture] <-
        note.percent(paper.table[paper.table$sex == 'male',posture], n = n.male)
    paper.table.book[paper.table$sex == 'female',posture] <-
        note.percent(paper.table[paper.table$sex == 'female',posture], n = n.female)
}
