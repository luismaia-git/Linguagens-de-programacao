import Data.Char


-- Parser em Haskell
-- Disciplina de Linguagens de Programação
-- Autor: Luís Antonio Da Silva Maia - 493458


-- Tipos de dados sendo definidos aqui
data Token
  = Program
  | Separacao | End | IntegerToken Integer
  | Id String | Begin | Assign
  | AbertoParentese | FechadoParentese | If
  | Then | Else | While
  | Do | Read | Write
  | COP String | TOP String | EOP String
  deriving (Show)

-- Dados da árvore definidos aqui
newtype AstInteger = AstInteger Integer deriving (Show)

newtype AstId = AstId String deriving (Show)

data AstCop
  = Igual
  | Dif | MaiorDoQue
  | MenorDoQue | MaiorOuIgualDoQue
  | MenorOuIgualDoQue
  deriving (Show)


data AstStat = AstSemicolon AstStat AstStat 
  | Atribuicao AstId Expressao
  | AstIf Expressao AstStat AstStat 
  | AstWhile Expressao AstStat 
  | AstRead AstId 
  | AstWrite Expressao deriving (Show)

data Expressao = Identificador AstId 
  | Inteiro AstInteger 
  | Operacao AstOp Expressao Expressao 
  | Comparacao AstCop Expressao Expressao deriving (Show)



data AstProg = Prog AstId AstStat deriving (Show)

data AstOp = Soma 
  | Subtracao 
  | Mult 
  | Div deriving (Show)

isNumStr :: String -> Bool
isNumStr str = case dropWhile isDigit str of
  ""  -> True
  _ -> False


pToken::[String] -> [Token]
pToken ls = [key_word x | x <- ls]


key_word :: String -> Token
key_word "program" = Program
key_word ";" = Separacao
key_word "begin" = Begin
key_word "end" = End
key_word ":=" = Assign
key_word "(" = AbertoParentese
key_word ")" = FechadoParentese
key_word "if" = If
key_word "then" = Then
key_word "else" = Else
key_word "while" = While
key_word "do" = Do
key_word "read" = Read
key_word "write" = Write
key_word "==" = COP "=="
key_word "!=" = COP "!="
key_word ">" = COP ">"
key_word "<" = COP "<"
key_word "=<" = COP "=<" 
key_word ">=" = COP ">="
key_word "+" = EOP "+"
key_word "-" = EOP "-"
key_word "*" = TOP "*"
key_word "/" = TOP "/"
key_word ls
  | isNumStr ls =
    IntegerToken (read ls :: Integer)
  | otherwise = Id ls

pStrings :: [String] -> AstProg
pStrings xs = let (tks, tree) = parserProg (pToken xs) in tree

-- Parser
parserProg :: [Token] -> ([Token], AstProg)
parserProg ls = case head ls of
  Program ->
    let s2 = tail ls
     in let (tokens, idToken) = parserId s2
         in case head tokens of
              Separacao ->
                let s4 = tail tokens
                 in let (tokens, statTree) = pStatement s4
                     in case head tokens of
                          End -> (tail tokens, Prog idToken statTree)


pStatement :: [Token] -> ([Token], AstStat)
pStatement ((Id str) : ls) =
  case head ls of
    Assign ->
      let (tks, expr) = pExpr (tail ls)
       in (tks, Atribuicao (AstId str) expr)
pStatement (Begin : ls) = pSequenceStat ls pStatement isSEP
pStatement (If : ls) =
  let (tks, parsedC) = pComp ls
   in case head tks of
        Then ->
          let (tk2, x1) = pStatement (tail tks)
           in case head tk2 of
                Else ->
                  let (tks3, x2) = pStatement (tail tk2)
                   in (tks3, AstIf parsedC x1 x2)
pStatement (While : ls) =
  let (tks, c) = pComp ls
   in case head tks of
        Do ->
          let (tk2, x) = pStatement (tail tks)
           in (tk2, AstWhile c x)
pStatement (Read : ls) = let (tks, rIdentificador) = parserId ls in (tks, AstRead rIdentificador)
pStatement (write : ls) = let (tks, expr) = pExpr ls in (tks, AstWrite expr)


isTOP :: Token -> Bool
isTOP (TOP _) = True
isTOP _ = False

isCOP :: Token -> Bool
isCOP (COP _) = True
isCOP _ = False

isEOP :: Token -> Bool
isEOP (EOP _) = True
isEOP x = False

isSEP :: Token -> Bool
isSEP Separacao = True
isSEP _ = False



pComp :: [Token] -> ([Token], Expressao)
pComp ls = parseSequenceExpr ls pExpr isCOP

pTerm :: [Token] -> ([Token], Expressao)
pTerm ls = parseSequenceExpr ls parseFact isTOP

pExpr :: [Token] -> ([Token], Expressao)
pExpr ls = parseSequenceExpr ls pTerm isEOP


pSequenceStat :: [Token] -> ([Token] -> ([Token], AstStat)) -> (Token -> Bool) -> ([Token], AstStat)
pSequenceStat ls nonTerm sep =
  let (tks, statTree) = nonTerm ls
   in if sep (head tks)
        then
          let x = head tks
              (x2, statTree2) = pSequenceStat (tail tks) nonTerm sep
           in case x of
                Separacao -> (x2, AstSemicolon statTree statTree2)
        else (tks, statTree)

parseSequenceExpr :: [Token] -> ([Token] -> ([Token], Expressao)) -> (Token -> Bool) -> ([Token], Expressao)
parseSequenceExpr ls nonTerm sep =
  let (tks, statTree) = nonTerm ls
   in if sep (head tks)
        then
          ( let x = head tks
                (tk2, statTree2) = parseSequenceExpr (tail tks) nonTerm sep
             in case x of
                  (EOP "+") -> (tk2, Operacao Soma statTree statTree2)
                  (EOP "-") -> (tk2, Operacao Subtracao statTree statTree2)
                  (TOP "*") -> (tk2, Operacao Mult statTree statTree2)
                  (TOP "/") -> (tk2, Operacao Div statTree statTree2)
                  (COP "==") -> (tk2, Comparacao Igual statTree statTree2)
                  (COP "!=") -> (tk2, Comparacao Dif statTree statTree2)
                  (COP ">") -> (tk2, Comparacao MaiorDoQue statTree statTree2)
                  (COP "<") -> (tk2, Comparacao MenorDoQue statTree statTree2)
                  (COP "=<") -> (tk2, Comparacao MenorOuIgualDoQue statTree statTree2)
                  (COP ">=") -> (tk2, Comparacao MaiorOuIgualDoQue statTree statTree2)
                  _ -> error "Operador quebrado"
          )
        else (tks, statTree)

parseFact :: [Token] -> ([Token], Expressao)
parseFact ls = case head ls of
  (IntegerToken i) -> (tail ls, Inteiro (AstInteger i))
  (Id str) -> (tail ls, Identificador (AstId str))
  AbertoParentese ->
    let s2 = tail ls
     in let (tks, s3) = pExpr s2
         in case head tks of
              FechadoParentese -> (tail tks, s3)


parserTokenIdToAstId :: Token -> AstId
parserTokenIdToAstId (Id str) = AstId str

parserId :: [Token] -> ([Token], AstId)
parserId ls =
  let tokenHead = head ls
   in case tokenHead of
        Id str -> (tail ls, AstId str)


pIsIdent :: Token -> Bool
pIsIdent AbertoParentese = False
pIsIdent FechadoParentese = False
pIsIdent t = pIsAtom t

pIsAtom :: Token -> Bool
pIsAtom (Id _) = True
pIsAtom _ = False



--Programa principal
main = do print(pStrings ["program","foo", ";", "while", "(","a","+","3", ")", "<","b", "do", "b", ":=", "b", "+", "1", "end"])


