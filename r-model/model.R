# R implementation of our optimization model for XP planning
library("lpSolve")

parseStoriesString <- function(stories.string) {
	as.numeric(strsplit(as.character(stories.string), ", ")[[1]])
}

xpRead <- function(story.file, theme.file, budget) {
	problem <- list()

	# read stories
	stories <- read.csv(story.file, header=TRUE)
	problem$story.names <- as.vector(stories[,1])
	problem$story.values <- as.vector(stories[,2])
	problem$story.costs <- as.vector(stories[,3])
	problem$story.deps <- lapply(as.vector(stories[,4]), parseStoriesString)

	themes <- read.csv(theme.file, header=TRUE)
	problem$theme.names <- as.vector(themes[,1])
	problem$theme.values <- as.vector(themes[,2])
	problem$theme.stories <- lapply(as.vector(themes[,3]), parseStoriesString)

	problem$budget <- budget

	problem
}

xpCheckProblem <- function(problem) {
	if (length(problem$story.values) != length(problem$story.costs)) {
		print("story.values and story.costs should have same length")
		return(FALSE)
	}
	if (length(problem$story.values) != length(problem$story.deps)) {
		print("story.values and story.deps should have same length")
		return(FALSE)
	}
	if (length(problem$story.values) != length(problem$story.names)) {
		print("story.values and story.names should have same length")
		return(FALSE)
	}
	if (length(problem$theme.values) != length(problem$theme.stories)) {
		print("theme.values and theme.stories should have same length")
		return(FALSE)
	}
	if (length(problem$theme.values) != length(problem$theme.names)) {
		print("theme.values and theme.names should have same length")
		return(FALSE)
	}
	if ("story.taboo" %in% names(problem) && !is.na(problem$story.taboo) && 
		length(problem$story.values) != length(problem$story.taboo)) {
		print("story.values and story.taboo should have same length")
		return(FALSE)
	}
	return(TRUE)
}

addCons <- function(cons, thisCons) {
	cons$mat <- rbind(cons$mat, thisCons$mat)
	cons$dir <- c(cons$dir, thisCons$dir)
	cons$rhs <- c(cons$rhs, thisCons$rhs)
	cons
}

xpSolveInternal <- function(
		story.values, story.costs, story.deps,
		theme.values, theme.stories,
		budget, story.taboo = NULL) {

	nStories = length(story.values)
	nThemes = length(theme.values)

	objective = c(story.values, theme.values)

	budgetCons <- list(
		mat = matrix(c(story.costs, rep(0, nThemes)),
			ncol = nStories + nThemes),
		dir = c("<="),
		rhs = c(budget))

	cons <- budgetCons

	for (i in 1:nStories) {
		if (!is.na(story.deps[[i]])) {
			thisCons = depCons(nStories, nThemes, i, story.deps[[i]])
			cons <- addCons(cons, thisCons)
		}
	}

	for (i in 1:nThemes) {
		thisCons = themeCons(nStories, nThemes, i, theme.stories[[i]])
		cons <- addCons(cons, thisCons)
	}

	if (!is.null(story.taboo)) {
		thisCons = tabooCons(nStories, nThemes, story.taboo)
		cons <- addCons(cons, thisCons)
	}

	nVars = nStories + nThemes
	lp(direction="max",
		objective.in=objective,
		const.mat=cons$mat,
		const.dir=cons$dir,
		const.rhs=cons$rhs,
		all.bin=TRUE)
}

xpSolve <- function(problem) {
	if (xpCheckProblem(problem) == FALSE) {
		return
	}
	out <- xpSolveInternal(
		problem$story.values, problem$story.costs, problem$story.deps,
		problem$theme.values, problem$theme.stories,
		problem$budget, problem$story.taboo)
	out$xp.problem <- problem
	nStories = length(problem$story.values)
	out$xp.stories <- out$solution[1:nStories]
	out
}

xpShrink <- function(solution, budget, greedy = FALSE) {
	problem <- solution$xp.problem
	if (problem$budget == budget) {
		return(solution)
	}

	if (greedy) {
		problem$story.taboo <- (1 - solution$xp.stories)
		problem$budget <- budget 
	} else {
		problem$story.taboo <- (1 - solution$xp.stories)
		problem$budget <- problem$budget - 1
	}
	xpShrink(xpSolve(problem), budget)
}

xpPrint <- function(s) {
	problem <- s$xp.problem
	nStories = length(problem$story.values)
	nThemes = length(problem$theme.values)
	nVars = nStories + nThemes

	cat("Stories selected:\n")
	print(problem$story.names[s$solution[1:nStories] == 1], quote=F)
	cat("Themes selected:\n")
	print(problem$theme.names[s$solution[(nStories+1):nVars] == 1], quote=F)
	cat("Value realized:\n")
	print(s$objval)
}

themeStories <- function(totalStories, stories) {
	sapply(c(1:totalStories), function(x) { x %in% stories })
}

themeCons <- function(totalStories, totalThemes, theme, stories) {
	nStories = length(stories)

	storyW = themeStories(totalStories, stories) 
	themeW = sapply(c(1:totalThemes), function(x) { x == theme })

	themeW1 = themeW * (-nStories)
	rhs1 = 0

	themeW2 = themeW * -1
	rhs2 = nStories - 1
	
	mat = matrix(c(storyW, themeW1, storyW, themeW2),
		nrow = 2, ncol = totalStories + totalThemes, byrow = TRUE)

	list(mat = mat, dir = c(">=", "<="), rhs = c(rhs1, rhs2))
}

depCons <- function(totalStories, totalThemes, story, deps) {
	nDeps = length(deps)
	thisW <- sapply(c(1:totalStories), function(x) { x == story })
	depsW <- sapply(c(1:totalStories), function(x) { x %in% deps })

	storyW <- thisW * nDeps - depsW
	themeW <- rep(0, times=totalThemes)

	mat = matrix(c(storyW, themeW),
		nrow = 1, ncol = totalStories + totalThemes, byrow = TRUE)

	list(mat = mat, dir = c("<="), rhs = c(0))
}

tabooCons <- function(totalStories, totalThemes, taboos) {
	mat = matrix(c(taboos, rep(0, times=totalThemes)),
		nrow = 1, ncol = totalStories + totalThemes, byrow = TRUE)
	list(mat = mat, dir = c("<="), rhs = c(0))
}
