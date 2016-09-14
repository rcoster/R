parse_OFX <- function(x) {
    output <- list()
    nomes <- NA
    i <- 0
    x <- gsub('^[^<]*|[^ >]*$', '', paste(x, collapse = ''))
    while (x != '') {
        i <- i+1
        pos <- gregexpr('<[^/>]*>', x)[[1]]
        tag <- substr(x, pos + 1, attr(pos, 'match.length') - 1)
        posOut <- gregexpr(sprintf('<%s>.*?</%1$s>', tag), x)[[1]]
        posOutAlt <- gregexpr('<', x)[[1]]
        if (posOut[1] > -1) {
            saldo <- substr(x, posOut[1] + nchar(tag) + 2, attr(posOut, 'match.length')[1] - nchar(tag) - 3)
            output[[i]] <- if (grepl('<', saldo)) { parseOFX(saldo) } else { saldo }
            x <- sub(sprintf('^<%s>.*?</%1$s>', tag), '', x)
        } else if (length(posOutAlt) > 1) {
            saldo <- substr(x, posOutAlt[1] + nchar(tag) + 2, posOutAlt[2] - 1)
            output[[i]] <- if (grepl('<', saldo)) { parseOFX(saldo) } else { saldo }
            x <- sub(sprintf('^<%s>[^<]*', tag), '', x)
        } else  {
            output[[i]] <- gsub('^<.*>', '', x)
            x <- ''
        }
        nomes[i] <- tag
    }
    names(output) <- nomes
    output
}
