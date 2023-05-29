-- Made by Leandro Fontellas Laurito & Kauan Gazarra Oliveira Vilela
import Control.Exception
import Data.List
import Data.Typeable
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

type Codigo = Int

type Nome = String

type Preco = Float

type Produto = (Codigo, Nome, Preco)

data Errors
  = ErrorWithMessage !String
  deriving (Show, Typeable)

instance Exception Errors

tabelaProdutos :: [Produto]
tabelaProdutos = [(001, "Chocolate", 5.25), (002, "Biscoito", 8.50), (003, "Laranja", 4.60), (004, "Sabao", 24.90), (005, "Batata Chips", 6.90), (006, "Doritos", 8.90)]

cadastrarNovoProduto :: Produto -> [Produto] -> [Produto]
cadastrarNovoProduto (codigo, nome, preco) produtos =
  let findIndexResult = find (\(c, _, _) -> c == codigo) produtos
   in if findIndexResult == Nothing then produtos ++ [(codigo, nome, preco)] else throw (ErrorWithMessage "Product ID already registered.")

removerUmProduto :: Int -> [Produto] -> [Produto]
removerUmProduto codigo produtos = [(c, n, p) | (c, n, p) <- produtos, c /= codigo]

alterarPrecoDeUmProduto :: Int -> Preco -> [Produto] -> [Produto]
alterarPrecoDeUmProduto _ _ [] = []
alterarPrecoDeUmProduto codigo novo_preco ((c, n, p) : t) = if c == codigo then (c, n, novo_preco) : t else (c, n, p) : t

geraNotaFiscal :: [Int] -> [Produto] -> IO ()
geraNotaFiscal codigos produtos =
  let filteredProdutos = getProductsByIds codigos produtos
   in do
        hSetBuffering stdout NoBuffering
        putStrLn "**********Nota Fiscal*********"
        putStrLn ""
        imprime filteredProdutos
        putStrLn ""
        imprimeSoma filteredProdutos

getProductsByIds :: [Int] -> [Produto] -> [Produto]
getProductsByIds codigos produtos = [p | p@(codigo, _, _) <- produtos, codigo `elem` codigos]

imprime :: [Produto] -> IO ()
imprime [] = putStr ""
imprime ((_, nome, preco) : t) =
  let tamanhoNome = length nome
      stringPreco = show preco
      decimals = floor ((preco - fromIntegral (floor preco)) * 100)
      formattedName = show (floor preco) ++ "." ++ show decimals
      tamanhoPreco = length formattedName
      numPontos = 30 - tamanhoNome - tamanhoPreco
   in do
        putStr nome
        putStr (replicate numPontos '.')
        putStrLn formattedName
        imprime t

imprimeSoma :: [Produto] -> IO ()
imprimeSoma [] = putStr ""
imprimeSoma produtos =
  let soma = sum [preco | (_, _, preco) <- produtos]
      decimals = floor ((soma - fromIntegral (floor soma)) * 100)
      formattedName = show (floor soma) ++ "." ++ show decimals
      numPontos = 30 - 5 - length formattedName
   in do
        putStr "Total"
        putStr (replicate numPontos '.')
        putStrLn formattedName
