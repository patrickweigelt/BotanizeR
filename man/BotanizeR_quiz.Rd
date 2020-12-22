\name{BotanizeR_quiz}
\alias{BotanizeR_quiz}
\title{Botany identification quiz}
\description{
Navigates through species from floraweb.de (or a defined subset thereof), shows pictures and descriptions and other hints and let's the user guess the species name. Number of tries and attempts are used to calculate scores that will be used to update probabilities for the random sampling of subsequent species.
}
\usage{
BotanizeR_quiz()
}
\arguments{
}
\details{
}
\value{
A vector of new probabilities per species.
}
\details{}
\section{Note}{}

\examples{
data(floraweb_species)
floraweb_species <- floraweb_species[which(floraweb_species$SUMMER == 1)] # subset for nearly 200 species forestry students in Göttingen learn.
BotanizeR_quiz()
}
