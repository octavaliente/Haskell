{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
type Libro = (String, Int, String)
type Biblioteca = [Libro]

libros :: Biblioteca
libros = [("El Visitante", 592,"Stephen King"), ("Shingeki1",40,"Isayama"),("Shingeki3",40,"Isayama"),("Shingeki127",40,"Isayama")
        ,("Shingeki127",40,"Isayama"),("Fundacion",230,"Asimov"),("Sandman5",35,"Gaiman"),("Sandman10",35,"Gaiman"),("Sandman12",35,"Gaiman")
        ,("Eragon",544,"Paolini"),("Eldest",704,"Paolini"),("Brisignr",700,"Paolini"),("Legado",811,"Paolini")]

obtenerPaginas :: Libro -> Int
obtenerPaginas (titulo, paginas, autor) = paginas

cantidadPaginasTotales :: Biblioteca -> Int
cantidadPaginasTotales unaBiblioteca = sum (map obtenerPaginas unaBiblioteca)

promedioHojas :: Biblioteca -> Int
promedioHojas unaBiblioteca = div (cantidadPaginasTotales unaBiblioteca) (length unaBiblioteca)

esDeStephenKing :: String -> Bool
esDeStephenKing unAutor = unAutor == "Stephen King" 

esDeEragon :: String -> Bool
esDeEragon unAutor = unAutor == "Paolini"

esFundacionDeAsimov :: Libro -> Bool
esFundacionDeAsimov (titulo, paginas, autor) = titulo == "Fundacion" && autor == "Asimov" && paginas == 230 

lecturaObligatoria :: Libro -> Bool
lecturaObligatoria (titulo, paginas, autor) = esDeStephenKing autor || esDeEragon autor || esFundacionDeAsimov (titulo, paginas, autor)

autor :: Libro -> String
autor (_,_,unAutor) = unAutor

esDe :: String -> Libro -> Bool
esDe unAutor unLibro = ((==unAutor).autor) unLibro

esLecturaFantasiosa :: Libro -> Bool
esLecturaFantasiosa unLibro = esDe "Gaiman" unLibro || esDe "Paolini" unLibro

esFantasiosa :: Biblioteca -> Bool
esFantasiosa unaBiblioteca = any esLecturaFantasiosa unaBiblioteca

{--}

esVocal :: Char -> Bool
esVocal unCaracter = elem unCaracter "aeiouAEIOUÁÉÍÓÚ"

eliminarVocal :: String -> String
eliminarVocal unaPalabra = filter (not . esVocal) unaPalabra

titulo :: Libro -> String
titulo (unTitulo,_,_) = unTitulo

concatenarTitulos :: Biblioteca -> String
concatenarTitulos unaBiblioteca = concatMap titulo unaBiblioteca

nombreDeLaBiblioteca :: Biblioteca -> String
nombreDeLaBiblioteca unaBiblioteca = (eliminarVocal . concatenarTitulos) unaBiblioteca

{--}

cantidadPaginas :: Libro -> Int
cantidadPaginas (_,pags,_) = pags

esLigera :: Libro -> Bool
esLigera unLibro = cantidadPaginas unLibro < 40

bibliotecaLigera :: Biblioteca -> Bool
bibliotecaLigera unaBiblioteca = all esLigera unaBiblioteca

{--}

genero :: Libro -> String
genero unLibro
  | esDe "Stephen King" unLibro = "Terror"
  | esDeAutorJapones unLibro    = "Manga"
  | esLigera unLibro            = "Comic"
  | otherwise                   = "Sin Categoria"

esDeAutorJapones :: Libro -> Bool
esDeAutorJapones unLibro = autor unLibro == "Isamaya"
