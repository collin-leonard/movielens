Title: Movie Recommendation Project
Author: Collin Leonard

DESCRIPTION: 

Using movielens data, create a recommendation system based on movie, user, genre, and time biases as well as exploring methods of matrix factorization.

FILES:

test-validation.R:
  downloads data from http://files.grouplens.org/datasets/movielens/ml-10m.zip, processes it and generates a folder ("data")   containing the sets edx and validation
  
movie-rec-project.R:
  R code that runs the SVD and bias training methods for prediction, as well as generates figures for the final report
  
movie-rec-project.Rmd:
  Markdown file with the report, uses a project structure with data and figs in subfolders
