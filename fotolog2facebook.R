token <- '' # Token de acesso do Facebook gerado aqui: https://developers.facebook.com/tools/explorer/. É preciso conceder acesso à 'user_photos'
user <- '' # Usuário no Fotolog.com
nome <- 'Fotolog' # Nome do Álbum
descricao <- 'Resolvi trazer as fotos do meu Fotolog para o face!' # Descrição do album.
anos <- 2004:2015 # Anos que serão copiados.

library(httr)
Album <- httr::POST(url = 'https://graph.facebook.com/v2.10/me/albums/',
			httr::content_type('application/json'),
			body = list(access_token = token,
						name = nome,
						message = descricao,
						privacy = list(value = 'SELF')),
			encode = 'json') 
AlbumID <- content(Album)$id			
		
if (status_code(Album) == 200) {		
	for (ano in anos) { 
		for (mes in 1:12) {
			dados <- suppressWarnings(readLines(sprintf('http://www.fotolog.com/%s/archive/%d/%d/', user, mes, ano)))
			dados <- paste0(dados, collapse = '')
			pos_post <- gregexpr(sprintf('<div class="day_num">[0-9]*</div><a href="http://www.fotolog.com/%s/[0-9]*/">', user), dados)[[1]]
			post <- mapply(substr, dados, pos_post, pos_post + attr(pos_post, 'match.length') - 1)
			names(post) <- NULL

			pos_links <- gregexpr(sprintf('http://www.fotolog.com/%s/[0-9]*/', user), post)
			links <- mapply(function(post, pos) { substr(post, pos, pos + attr(pos, 'match.length') -1) }, post, pos_links)
			pos_data <- gregexpr(sprintf('>[0-9]{1,2}<', user), post)
			data <- mapply(function(post, pos) { substr(post, pos + 1, pos + attr(pos, 'match.length') - 2) }, post, pos_data)

			for (i in seq_along(data)) {
				if (links[i] != '') { 
					dados <- suppressWarnings(readLines(links[i]))
					tituloPos <- gregexpr('</?title>', dados[1])[[1]]
					titulo <- iconv(substr(dados[1], tituloPos[1] + 7, tituloPos[2] - 1), 'UTF8', 'latin1')
					foto <- substr(dados[1], regexpr('<meta property="og:image" content="', dados[1]) + 35, regexpr('"><meta property="og:url"', dados[1]) - 1)
					
					dados <- paste0(dados, collapse = '')
					desc_pos <- regexpr('<meta itemprop=\"description\" content=\".*?<meta itemprop=\"image\"', dados)
					descricao <- substr(dados[1], desc_pos + 38, desc_pos + attr(desc_pos,"match.length") - 25)
					descricao <- gsub(' \\|[^|]*$', '', gsub('&lt;BR&gt;', '\n', iconv(substr(dados[1], desc_pos + 38, desc_pos + attr(desc_pos,"match.length")), 'UTF8', 'latin1')))
					descricao <- gsub('&quot;', '"', descricao) # Deve ter uma maneira mais inteligente...
					
					coment_pos <- gregexpr('<div class="flog_img_comments">.*?</div>', dados)[[1]]
					comentarios <- iconv(mapply(substr, dados, coment_pos, coment_pos + attr(coment_pos, 'match.length') - 1), 'UTF8', 'latin1')
					names(comentarios) <- NULL
					
					autor_pos <- gregexpr('<a[^<]*?</a>', comentarios)
					autor <- mapply(function(post, pos) { gsub('<.*?>', '', substr(post, pos, pos + attr(pos, 'match.length') -1)) }, comentarios, autor_pos)
					names(autor) <- NULL 
					
					comentarios <- gsub('http://flashfs.blindecho.net/riddle', '', gsub('^.*?[0-9/]{10}\n\n', '', gsub('<.*?>', '', gsub('<br>', '\n', gsub('&lt;', '<', gsub('&gt;', '>', comentarios))))))
					ntry <- 1
					
					repeat {
						Foto <- httr::POST(url = sprintf('https://graph.facebook.com/v2.10/%s/photos/', AlbumID),
								httr::content_type('application/json'),
								body = list(access_token = token,
											url = foto,
											caption = sprintf('Descricao: %s\n\nComentarios:\n %s', descricao, ifelse(comentarios[1] == '', '', paste(sprintf('Autor: %s\nComentario: %s', rev(autor), rev(comentarios)), collapse = '\n\n'))),
											backdated_time = strptime(sprintf('%s/%s/%s 12:00', ano, mes, data[i]), format = "%Y/%m/%d %H:%M")),
								encode = 'json')
							ntry <- ntry + 1
						if (ntry > 5) { 
							cat(sprintf('Erro! Foto: %s\n', links[i]))
							break
						}
						if (status_code(Foto)) { break }
					}
				}
			}
		}
	}
} else {
	cat('Erro ao criar álbum!\n')
}
