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
After running the main function BotanizeR_quiz() you need to click into the console to type the species names in there. If you have no clou, press enter and the next hint will appear (you have ten tries). If you type the name slightly wrong the function will tell you. If the genus is correct it will also tell you. It shows several photos if available. If you want to skip a species write "skip". If you want to cancel the quiz write "exit". Don't hit Esc if you want to save your progress. The function counts your attempts and successes and uses this as sampling probabilities for choosing the species randomly. The better you know a species the less likely it will be shown again.
}
\value{
A vector of new probabilities per species.
}
\details{}
\section{Note}{}

\examples{
# Species list for Germany with IDs from floraweb.de
data(floraweb_species)

# subset for nearly 200 species forestry students in Göttingen learn
floraweb_species <- floraweb_species[which(floraweb_species$SUMMER==1),]

# Type in species name, or press enter for next hint or type "skip" and press 
# enter for next species or type "exit" to end quiz and save results
floraweb_species_trained <- BotanizeR_quiz(species_list = floraweb_species, 
                                           hints = c("description","status",
                                                     "habitat","family","German name"))
}
