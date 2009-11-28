source('model.R')

xpReadDefault <- function() {
	xpRead('stories.csv', 'themes.csv', 50)
}

themeImplemented <- function(stories, theme.stories) {
	theme.stories <- themeStories(length(stories), theme.stories)
	sum(theme.stories) == sum(theme.stories & stories)
}

xpValue <- function(problem, stories) {
	value <- sum(problem$story.values * stories)

	nThemes = length(problem$theme.values)
	for (i in 1:nThemes) {
		if (themeImplemented(stories, problem$theme.stories[[i]])) {
			value <- value + problem$theme.values[i]
		}
	}

	value
}

xpEffort <- function(problem, stories) {
	sum(problem$story.costs * stories)
}

# Assist the customer by giving a constant value to each theme
xpThemeConstant <- function(problem, const) {
	problem$theme.values <-
		sapply(problem$theme.values, function(x) { const })
	problem
}

storyError <- function(s1, s2) {
	sum(xor(s1, s2))
}

selectedDefault <- function() {
	read.csv('stories.csv', header=T)[,5]
}

themeConstantError <- function() {
	selected <- selectedDefault()
	problem <- xpReadDefault()
	error <- c()
	for (i in 0:99) {
		opt <- xpSolve(xpThemeConstant(problem, i))
		error[i] <- storyError(selected, opt$xp.stories)
	}
	error
}
