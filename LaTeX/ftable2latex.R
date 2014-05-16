# ftable2latex v1.0
# Código da postagem http://www.dadosaleatorios.com.br/2014/05/exportando-objetos-ftable-para-latex.html
#
# Os parâmetros da função são:
#
# tab	 - Objeto da classe ftable que deseja exportar
# package	 - Pacote de tabela que se deseja usar no LaTeX ('tabular' (Padrão); 'longtable' ou outro pacote que tenha a mesma sintaxe que esses)
# labels	 - Vetor com a descrição das variáveis, que serão utilizadas nas tabelas
# cv.length	 - Tamanho máximo permitido para os leveis das variaveis das colunas (Padrão: igual ao parâmetro length)
# lv.length	 - Tamanho máximo permitido para os leveis das variaveis das linhas (Padrão: igual ao parâmetro length)
# length	 - Tamanho máximo permitido para os leveis das variaveis, quando não especificado o limite específico de cada variavel (Padrão: Não há limite)
# format	 - Formatação dos números dos conteúdos para tabelas no formato to sprintf() (Padrão: '%.0f' = Arredondar para 0 casas decimais)
# layout.colvars - Layout dos títulos das variaveis das colunas (1 ou 2 (Padrão))
# ...	 - Parâmetros a serem repassados para a função cat()

# Sites usados como apoio:
# http://maksim.sorokin.dk/it/2010/05/22/multirow-and-multicolumn-spanning-in-latex/
# http://la-tex.dreamwidth.org/3017.html
# http://tex.stackexchange.com/questions/56101/spaces-between-rows-and-cols-in-a-table-better-ways-than-mine
#

ftable2latex <- function(tab, package = 'tabular', labels = vector(), cv.length = length, rv.length = length, length = Inf, format = '%.0f', layout.colvars = 1, ...) {

# Função de apoio para quebrar uma linha grande em várias menores.
.table <- function(text, length) {
if (length(text) > 1) { return(mapply(.table, text, length = length)) }
text <- strwrap(text, width = length)
if (length(text) == 1) { return(text) }
else {
return(paste('\\begin{tabular}{@{\\hskip 0pt}l}', paste(text, collapse = ' \\\\ '), '\\end{tabular}'))
}
}
rowvars <- attr(tab, 'row.vars')
colvars <- attr(tab, 'col.vars')
.ncol <- ncol(tab) + length(rowvars) + ifelse(layout.colvars == 1, 1, 0)
colmerge <- rowmerge <- colrep <- rowrep <- 0
collength <- mapply(length, attr(tab, 'col.vars'))
for (i in 1:length(colvars)) {
colmerge[i] <- prod(tail(collength, -i))
colrep[i] <- prod(head(collength, i - 1))
}

rowlength <- mapply(length, attr(tab, 'row.vars'))
for (i in 1:length(rowvars)) {
rowmerge[i] <- prod(tail(rowlength, -i))
rowrep[i] <- prod(head(rowlength, i - 1))
}

# Criando a tabela
cat(sprintf('\\begin{%s}{%s}\n\\hline\n', package, paste(c(rep('l', length(rowvars)), rep('r', .ncol - length(rowvars))), collapse='')), append = TRUE, ...)

# Aplicando os labels
names(rowvars) <- ifelse(names(rowvars) %in% names(labels), labels[names(rowvars)], names(rowvars))
names(colvars) <- ifelse(names(colvars) %in% names(labels), labels[names(colvars)], names(colvars))

# Criando as linhas de títulos
if (layout.colvars == 1) {
for (i in 1:length(colvars)) {
if (i == 1) {
row <- c(sprintf(ifelse(length(colvars) > 1, sprintf('\\multirow{%d}{*}{%%s}', length(colvars)), '%s'), .table(names(rowvars), length = rv.length)), names(colvars)[i], sprintf('\\multicolumn{%d}{|c}{%s}', colmerge[i], .table(colvars[[i]], length = cv.length)))
}
else {
row <- c(rep('', length(rowvars)), names(colvars)[i], sprintf('\\multicolumn{%d}{|c}{%s}', colmerge[i], rep(.table(colvars[[i]], length = cv.length), colrep[i])))
}
if (i < length(colvars)) {
cat(sprintf(paste(paste0(row, collapse = ' & '), '\\\\ \\cline{%d-%d} \n'), length(rowvars) + 1, .ncol), append = TRUE, ...)
}
else {
cat(paste(paste0(row, collapse = ' & '), '\\\\ \\hline \n'), append = TRUE, ...)	
}
}
}
else {
for (i in 1:length(colvars)) {
if (i == 1)	{
row1 <- c(sprintf('\\multirow{%d}{*}{%s}', length(colvars) * 2, .table(names(rowvars), length = rv.length)), sprintf('\\multicolumn{%d}{|c}{%s}', prod(collength),  names(colvars)[i]))
row2 <- c(rep('', length(rowvars)), sprintf('\\multicolumn{%d}{|c}{%s}', colmerge[i], rep(.table(colvars[[i]], length = cv.length), colrep[i])))
}
else {
row1 <- c(rep('', length(rowvars)), sprintf('\\multicolumn{%d}{|c}{%s}', prod(collength),  names(colvars)[i]))
row2 <- c(rep('', length(rowvars)), sprintf('\\multicolumn{%d}{|c}{%s}', colmerge[i], rep(.table(colvars[[i]], length = cv.length), colrep[i])))	
}
cat(sprintf(paste(paste0(row1, collapse = ' & '), '\\\\ \\cline{%d-%d} \n'), length(rowvars) + 1, .ncol), append = TRUE, ...) # A linha 1 é padrão para todos
if (i < length(colvars)) {
cat(sprintf(paste(paste0(row2, collapse = ' & '), '\\\\ \\cline{%d-%d} \n'), length(rowvars) + 1, .ncol), append = TRUE, ...)
}
else {
cat(paste(paste0(row2, collapse = ' & '), '\\\\ \\hline \n'), append = TRUE, ...)	
}
}
}

# Criando o conteudo da tabela

## Criando as colunas das variáveis
cols <- rev(do.call(expand.grid, c(rev(attr(tab, 'row.vars')), stringsAsFactors = FALSE)))
for (i in 1:ncol(cols)) {
cols[, i] <- sprintf(ifelse(rowmerge[i] > 1, sprintf('\\multirow{%d}{*}{%%s}', rowmerge[i]), '%s'), .table(cols[, i], length = rv.length))
cols[rep(c(FALSE, rep(TRUE, rowmerge[i] - 1)), nrow(cols) / rowmerge[i]), i] <- ''
}
cols[, i] <- sprintf('\\multicolumn{%d}{l|}{%s}', ifelse(layout.colvars == 1, 2, 1), cols[, i])

## Criando as linhas horizontais
hlines <- rep('', nrow(cols))
if (length(rowvars) > 1) {
for (i in length(rowvars):2 - 1) {
hlines[rep(c(rep(FALSE, rowmerge[i] - 1), TRUE), nrow(cols) / rowmerge[i])] <- sprintf('\\cline{%d-%d}', i, .ncol)
}
}
hlines[length(hlines)] <- '\\hline'

## Criando as linhas de conteudo
for (i in 1:nrow(tab)) {
cat(paste(paste0(c(cols[i, ], sprintf(format, tab[i, ])), collapse = ' & '), sprintf('\\\\ %s \n', hlines[i])), append = TRUE, ...)
}

# Encerrando a tabela
cat(sprintf('\\end{%s} \\\\\n', package), append = TRUE, ...)
}
