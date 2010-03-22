# R implementation of our optimization model for XP planning
library("lpSolve")

parseStoriesString <- function(stories.string) {
	as.numeric(strsplit(as.character(stories.string), ", ")[[1]])
}

uptoAndIncludingBag <- function(nStories, nThemes, nBags, k) {
	bagIdx <- c(rep(1, times=k), rep(0, times=(nBags - k)))
	storySel <- rep(bagIdx, each=nStories)
	themeSel <- rep(bagIdx, each=nThemes)
	c(storySel, themeSel, themeSel)
}

onlyBag <- function(nStories, nThemes, nBags, k) {
	bagIdx <- rep(0, nBags)
	bagIdx[k] <- 1
	storySel <- rep(bagIdx, each=nStories)
	themeSel <- rep(bagIdx, each=nThemes)
	c(storySel, themeSel, themeSel)
}

# Theme constraint (using dummy variable): set z_{theme,bag} to 1 iff
# all stories in theme are implemented in one of the bags 1..bag.
themeCons <- function(nStories, nThemes, nBags, t, stories) {
	s <- length(stories)
	a <- sapply(c(1:nStories), function(x) { x %in% stories })
	tIdx <- sapply(c(1:nThemes), function(x) { x == t })
	nVars <- nBags * (nStories + 2 * nThemes)

	m <- matrix(nrow = (2 * nBags), ncol = nVars)
	r <- rep(0, times = 2 * nBags)
	d <- rep("", times = 2 * nBags)
	for (k in 1:nBags) {
		storySel <- c(
			rep(a, times = k),
			rep(0, times = (nStories * (nBags - k))))
		themeSel <- rep(0, times = (nThemes * nBags))
		dummySel <- c(
			rep(0, times = (k - 1) * nThemes),
			tIdx,
			rep(0, times = (nBags - k) * nThemes))

		i1 <- k * 2 - 1
		m[i1, ] <- c(storySel, themeSel, -1 * s * dummySel)
		r[i1] <- 0
		d[i1] <- ">="

		i2 <- i1 + 1
		m[i2, ] <- c(storySel, themeSel, -1 * dummySel)
		r[i2] <- s - 1
		d[i2] <- "<="
	}

	list(mat = m, dir = d, rhs = r)
}

# Theme only once: set y_{theme, bag} = 1 iff z_{theme, bag} = 1 and
# (z_{theme, bag - 1} = 0, or bag = 1).
themeOnlyOnce <- function(nStories, nThemes, nBags, t) {
	nVars <- nBags * (nStories + 2 * nThemes)

	storySel <- rep(0, times = (nStories * nBags))
	tIdx <- sapply(c(1:nThemes), function(x) { x == t })
	themeSel <- rep(tIdx, times = nBags)

	m <- matrix(nrow = nBags, ncol = nVars)
	r <- rep(0, times = nBags)
	d <- rep("==", times = nBags)

	# z_t,1 - y_t,1 = 0
	m[1, ] <-
		c(storySel, -1 * themeSel, themeSel) *
		onlyBag(nStories, nThemes, nBags, 1)
	for (k in 2:nBags) {
		# z_t,k - z_t,(k-1) - y_t,k = 0
		m[k, ] <-
			c(storySel, -1 * themeSel, themeSel) *
			onlyBag(nStories, nThemes, nBags, k) + 
			c(0 * storySel, 0 * themeSel, -1 * themeSel) *
			onlyBag(nStories, nThemes, nBags, k - 1)
	}

	list(mat = m, dir = d, rhs = r)
}

storyDeps <- function(nStories, nThemes, nBags, i, deps) {
	nDeps <- length(deps)
	depsW <- sapply(c(1:nStories), function(x) { x %in% deps })
	thisW <- sapply(c(1:nStories), function(x) { x == i })
	nVars <- nBags * (nStories + 2 * nThemes)

	m <- matrix(nrow = nBags, ncol = nVars)
	r <- rep(0, times = nBags)
	d <- rep("<=", times = nBags)

	otherSel <- rep(0, times = (nBags * 2 * nThemes))

	# s * x_i,k - sum_(1..k) a_#,k * x_#,k
	for (k in 1:nBags) {
		m[k, ] <- 
			c(nDeps * rep(thisW, times = nBags), otherSel) *
				onlyBag(nStories, nThemes, nBags, k) -
			c(rep(depsW, times = nBags), otherSel) *
				uptoAndIncludingBag(nStories, nThemes, nBags, k)
	}

	list(mat = m, rhs = r, dir = d)
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
		bag.discounts, bag.budgets) {

	nStories <- length(story.values)
	nThemes <- length(theme.values)
	nBags <- length(bag.discounts)
	nVars <- nBags * (nStories + 2 * nThemes)

	# Objective: optimize total expected value
	storyValue <-
		rep(bag.discounts, each=nStories) * rep(story.values, times=nBags)
	themeValue <-
		rep(bag.discounts, each=nThemes) * rep(theme.values, times=nBags)
	dummyValue <-
		rep(0, times=(nThemes * nBags))
	objective <- c(storyValue, themeValue, dummyValue)

	# Budget constraint: stories completed in bags prior to (including) k may
	# not exceed total complexity of bag.budgets[k]
	complexity <-
		c(rep(story.costs, times=nBags), rep(0, times=(2 * nThemes * nBags)))
	complexityMatrix <- matrix(nrow = nBags, ncol = nVars)
	for (k in 1:nBags) {
		complexityMatrix[k, ] <-
			uptoAndIncludingBag(nStories, nThemes, nBags, k) * complexity
	}
	budgetCons <- list(
		mat = complexityMatrix,
		dir = rep("<=", nBags),
		rhs = bag.budgets)
	cons <- budgetCons

	# Each story only once
	storyMatrix <- matrix(nrow = nStories, ncol = nVars)
	for (i in 1:nStories) {
		storyIdx <- rep(0, times = nStories)
		storyIdx[i] <- 1
		storyMatrix[i, ] <- c(
			rep(storyIdx, times = nBags),
			rep(0, times = (2 * nThemes * nBags)))
	}
	storyOnlyOnceCons <- list(
		mat = storyMatrix,
		dir = rep("<=", nStories),
		rhs = rep(1, nStories))
	cons <- addCons(cons, storyOnlyOnceCons)

	# Theme constraint (using dummy variable): set z_{theme,bag} to 1 iff
	# all stories in theme are implemented in one of the bags 1..bag.
	# Theme only once: set y_{theme, bag} = 1 iff z_{theme, bag} = 1 and
	# (z_{theme, bag - 1} = 0, or bag = 1).
	for (t in 1:nThemes) {
		thisCons <- themeCons(nStories, nThemes, nBags, t, theme.stories[[t]])
		cons <- addCons(cons, thisCons)
		thisCons <- themeOnlyOnce(nStories, nThemes, nBags, t)
		cons <- addCons(cons, thisCons)
	}

	# Story dependencies: x_j,k = 1 only if for all i \prec j, x_i,k = 1
	for (i in 1:nStories) {
		if (!is.na(story.deps[[i]])) {
			thisCons = storyDeps(nStories, nThemes, nBags, i, story.deps[[i]])
			cons <- addCons(cons, thisCons)
		}
	}

	lp(direction="max",
		objective.in=objective,
		const.mat=cons$mat,
		const.dir=cons$dir,
		const.rhs=cons$rhs,
		all.bin=TRUE)
}

xpRead <- function(story.file, theme.file, bag.file) {
	problem <- list()

	# read stories: name,value,complexity,dependencies
	stories <- read.csv(story.file, header=TRUE)
	problem$story.names <- as.vector(stories[,1])
	problem$story.values <- as.vector(stories[,2])
	problem$story.costs <- as.vector(stories[,3])
	problem$story.deps <- lapply(as.vector(stories[,4]), parseStoriesString)

	# read themes: name,value,stories
	themes <- read.csv(theme.file, header=TRUE)
	problem$theme.names <- as.vector(themes[,1])
	problem$theme.values <- as.vector(themes[,2])
	problem$theme.stories <- lapply(as.vector(themes[,3]), parseStoriesString)

	# read bags: name,discount,budget
	bags <- read.csv(bag.file, header=TRUE)
	problem$bag.names <- as.vector(bags[,1])
	problem$bag.discounts <- as.vector(bags[,2])
	problem$bag.budgets <- as.vector(bags[,3])

	problem
}

xpSolve <- function(problem) {
	out <- xpSolveInternal(
		problem$story.values, problem$story.costs, problem$story.deps,
		problem$theme.values, problem$theme.stories,
		problem$bag.discounts, problem$bag.budgets)
	out$xp.problem <- problem
	out
}

xpPrint <- function(s) {
	problem <- s$xp.problem
	nStories = length(problem$story.values)
	nThemes = length(problem$theme.values)
	nBags = length(problem$bag.names)

	for (k in 1:nBags) {
		budget = problem$bag.budgets[k]
		discount = problem$bag.discounts[k]
		cat(paste(
			problem$bag.names[k], "(discount ", discount,
			", budget ", budget, ")\n", sep=""))
		cat("\tStories selected:\n")
		selectedStories <- s$solution[1:nStories + (k - 1) * nStories] == 1
		for (name in problem$story.names[selectedStories]) {
			cat(paste("\t\t", name, "\n", sep=""))
		}
		storyCost = (sum(problem$story.costs[selectedStories]))
		cat(paste(
			"\tComplexity: ", storyCost, sep=""))
		storyVal = sum(problem$story.values[selectedStories])
		cat(paste(
			"\tValue: ", storyVal, " (", storyVal * discount, ")\n", sep=""))
		cat("\tThemes selected:\n")
		selectedThemes <- s$solution[1:nThemes + nBags * nStories + (k - 1) * nThemes] == 1
		for (name in problem$theme.names[selectedThemes]) {
			cat(paste("\t\t", name, "\n", sep=""))
		}
		themeVal = sum(problem$theme.values[selectedThemes])
		cat(paste(
			"\tValue: ", themeVal, " (", themeVal * discount, ")\n", sep=""))
	}

	cat("Expected value: ", s$objval, "\n", sep="")
}
