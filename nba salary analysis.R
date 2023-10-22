data = read.csv("path to the csv file")

# Basic Overview
colnames(data)
table(data$Position)

# Salary by Position
positions_to_remove <- c("PG-SG", "SF-SG","SG-PG","SF-PF")
filtered_data <- data[!(data$Position %in% positions_to_remove), ]
filtered_data$Salary <- filtered_data$Salary / 1e6
filtered_data$Position <- factor(filtered_data$Position, levels = unique(filtered_data$Position))
boxplot(Salary ~ Position, data = filtered_data, 
        xlab = "Position", ylab = "Salary (in millions USD)", 
        main = "NBA Player Salaries by Position",
        names = levels(filtered_data$Position))

# Salary vs. Performance
plot_with_r2 <- function(x, y, title) {
  plot(x, y, 
       xlab = "Salary", 
       ylab = title,
       main = paste("Salary vs.", title))
  lm_model <- lm(y ~ x)
  abline(lm_model, col = "red")
  r2 <- summary(lm_model)$r.squared
  text(max(x), 1, bquote(R^2 == .(round(r2, 3))), pos = 2, col="blue")
}
par(mfrow = c(3, 3))
plot_with_r2(filtered_data$Salary, filtered_data$PTS, "Points Per Game")
plot_with_r2(filtered_data$Salary, filtered_data$AST, "Assits Per Game")
plot_with_r2(filtered_data$Salary, filtered_data$TRB, "Total Rebounds Per Game")
plot_with_r2(filtered_data$Salary, filtered_data$STL, "Steals Per Game")
plot_with_r2(filtered_data$Salary, filtered_data$BLK, "Blocks Per Game")
plot_with_r2(filtered_data$Salary, filtered_data$FG, "Field Goals Made Per Game")
plot_with_r2(filtered_data$Salary, filtered_data$X3P., "Three Point Percentage")
plot_with_r2(filtered_data$Salary, filtered_data$X2P., "Two Point Percentage")
plot_with_r2(filtered_data$Salary, filtered_data$FT., "Free Throw Percentage")

# Team Salary Composition
par(mfrow = c(1, 1))
filtered_data = filtered_data[!grepl("/", filtered_data$Team), ] 
team_data <- aggregate(filtered_data$Salary, by=list(filtered_data$Team), FUN=sum)
unique_teams <- unique(filtered_data$Team)
team_data <- data.frame()
for (team in unique_teams) {
  team_subset <- filtered_data[filtered_data$Team == team, ]
  team_subset$Salary[is.na(team_subset$Salary)] <- 0
  total_salary <- sum(team_subset$Salary)
  new_team_row <- data.frame(Team = team)
  for (position in c("PG","SF","PF","SG","C")) {
    pos_subset <- team_subset[team_subset$Position == position,]
    pos_subset$Salary[is.na(pos_subset$Salary)] <- 0
    total_pos_salary <- sum(pos_subset$Salary)
    if (!is.na(position)) {
      new_team_row[, position] <- total_pos_salary/total_salary
    } else {
      new_team_row[, position] <- 0
    }
  }
  team_data = rbind(team_data,new_team_row)
}
transposed_team_data <- t(team_data)
colnames(transposed_team_data) <- as.character(unlist(transposed_team_data[1, ]))
transposed_team_data <- transposed_team_data[-1, ]
barplot(as.matrix(transposed_team_data), col = rainbow(nrow(transposed_team_data)))
legend("bottomright", legend = rownames(transposed_team_data), fill = rainbow(nrow(transposed_team_data)),xpd=TRUE, inset=c(0.06,-0.5),horiz=TRUE)
title("Team Salary Composition")

