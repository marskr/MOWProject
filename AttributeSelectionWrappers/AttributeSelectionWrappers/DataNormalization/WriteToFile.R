# method that saves cluster calculated data to a given file
writeToFile = function(resName, resPart, resFile) {
    write(resName, resFile, sep = separator, append = appending)
    write(resPart, resFile, sep = separator, append = appending)
}