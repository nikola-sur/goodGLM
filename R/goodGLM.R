#' Generalized Hosmer-Lemeshow Test for GLMs
#' Performs a global goodness-of-fit test on objects of class 'glm'. The test is
#' based on a generalization of the Hosmer-Lemeshow test. For more information,
#' see the two papers by Surjanovic, Lockhart, and Loughin (2020) and
#' Surjanovic and Loughin (2021).
#'
#' @param mod GLM object.
#' @param groups Number of groups.
#' @param group_mode Group mode.
#'
#' @return
#' @export
#'
#' Some parts of this code are based on
#' http://www.chrisbilder.com/categorical/Chapter5/AllGOFTests.R by Tom Loughin
#' Modified by Nikola Surjanovic.
#' Previous comment from 'AllGOFTests.R':
#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 1-10-13                                                     #
# PURPOSE: Functions to compute Hosmer-Lemeshow, Osius-Rojek, and   #
#     Stukel goodness-of-fit tests                                  #
#                                                                   #
# NOTES:                                                            #
#####################################################################
# Single R file that contains all three goodness-of fit tests


# Adapted from program published by Ken Kleinman as Exmaple 8.8 on the SAS and R blog, sas-and-r.blogspot.ca
#  Assumes data are aggregated into Explanatory Variable Pattern form.
goodGLM <- function(mod, groups, group_mode) {
  return(0)
}

