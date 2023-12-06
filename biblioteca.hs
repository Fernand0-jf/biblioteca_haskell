"# biblioteca_haskell" 
import Data.List
import Data.Char

type Pessoa = String
type Livro = String
type Emprestimo = (Pessoa, Livro)
type Emprestimos = [Emprestimo]
type Situacao = String
type Quantidade = Int




emprestadoPorPessoa :: Pessoa -> Emprestimos -> [Livro] 
emprestadoPorPessoa pes liv = [ a | (a,b)<- liv, a==pes ]

livroPorPessoa :: Livro -> Emprestimos -> [Pessoa]
livroPorPessoa liv pes = [ b| (a,b) <- pes, b == liv]

situacaoLivro :: Livro -> Emprestimos -> Situacao
situacaoLivro liv sit 
  |null (livroPorPessoa liv sit)  = "Nao esta emprestado"
  |otherwise                      = "Esta emprestado"


qtddLivrosP :: Pessoa -> Emprestimos -> Quantidade
qtddLivrosP pes ms = sum ( [ 1 | (a,b) <- ms, a == pes ] )

adicionarEmprestimo :: Emprestimo -> Emprestimos -> Emprestimos
adicionarEmprestimo emp emps = emp:emps

retirarEmprestimo :: Emprestimo -> Emprestimos -> Situacao
retirarEmprestimo emp emps 
  |bancoDeDados == emps = "nao esta no banco de dados"
  |otherwise            = "ja foi realizada sua devolucao"

  where  
    bancoDeDados :: Emprestimos
    bancoDeDados = [(a,b) | (a,b) <- emps , fst emp/=a]
