import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
  do char '"'
     content <- many quotedChar
     char '"' <?> "quote at end of cell"
     return content

quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main = 
  do c <- getContents
     case parse csvFile "(stdin)" c of
         Left e -> do putStrLn "Error parsing input:"
                      print e
         Right r -> mapM_ print r

{-
  That's a full featured CSV parser in just 21 lines of code, plus an additional 10 lines for the
  parserCSV and main utility functions.

  let's look at the changes in this program from the previous version. First, a cell 
  may now be either a bare cell or a quoted cell. We give the quotedCell option first
  because we want to follow that path if the first character in a cell is the quote mark.

  The quotedCell begins and ends with a quote mark and contains zero or more characters. 
  These cahracters cannot be copied directly though befcause they may contain embeeded, 
  doubled-up quote marks themselves, so we define a custom quotedChar to process them.

  When we are processing characters inside a quotedcell, we first say noneOf "\""  thus wil
  match and return any single character as along as it is not the quote mark. Otherwise,
  if it is the quote mark, we see if we have two in a row. If so, we return a single quote
  mark to go on our result String.

  notice that try in quotedChar is on the right side of <|>. Recall that we said that try
  has an effect only if it is on the left side of <|>. This try does occur on the left side
  of a <|> but on th eleft of one that must be within the implementation of many.

-}
