Each learning algorithm, along with a helper function can be found in the learning_algorithms/ folder. The files correspond to:
learners_pay.R - takes into account x traits, learns all x; with a penalty dependency structure
learners_window_pay.R - takes into account x traits, learns just one; penalty dependency structure
learners_bonus_window.R - takes into account x traits, learns just one; bonus dependency structure

run.R plots the payoffs for a given combinations of n (number of traits) and s (number of levels) and a series of sdev values for all 5 learning algorithms.
