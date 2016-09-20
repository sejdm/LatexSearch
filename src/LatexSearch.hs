{-# LANGUAGE OverloadedStrings #-}
module LatexSearch (
   searchLatex
    )
  where

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator as C
import Data.Text as DT
import Data.Text.IO as TO
import Control.Applicative
import Pipes
import Pipes.Text.IO as T
import Pipes.Prelude as PI
import Control.Monad as M
import qualified Data.Char as C

pbegin = string "\\begin{" *> takeTill (=='}') <* char '}'
pend = string "\\end{" *> takeTill (=='}') <* char '}'
penv = (\x y -> LatexEnv x (pack y)) <$> pbegin <*> manyTill anyChar pend
ppar = LatexPar . pack <$> manyTill anyChar (string "\n\n" <|> C.lookAhead (string "\\begin"))
platexblock = penv <|> ppar
platex = many platexblock

data MyLatex = LatexEnv Text Text | LatexPar Text deriving (Show)


pipeMany :: Monad m => Parser MyLatex -> Text -> Pipe Text MyLatex m ()
pipeMany f t = do case parse f t of
                      Done i r -> yield r >> pipeMany f i
                      Partial g -> partialParse g
  where partialParse g = do x <- await
                            case g x of
                              Done i r -> yield r >> pipeMany f i
                              Partial g' -> partialParse g'


latexParsePipe :: Monad m => Pipe Text MyLatex m ()
latexParsePipe = pipeMany platexblock ""


pipeMap f = forever $ do x <- await
                         yield $ f x

render (LatexEnv n t) = DT.map C.toUpper n `append` ": " `append` t `append` "\n\n"
render (LatexPar t) = "Par:" `append` t `append` "\n\n"

isTheorem (LatexEnv _ _) = True
isTheorem _ = False

getText (LatexPar x) = x
getText (LatexEnv _ x) = x

checkWords xs t = Prelude.all (`DT.isInfixOf` (getText t)) $ Prelude.map pack xs


searchLatex f xs = runSafeT $ runEffect (T.readFile f >-> latexParsePipe >-> PI.filter (checkWords xs) >-> pipeMap (render) >-> T.stdout)

--checkParser f = do Partial f <- parse platex <$> TO.readFile f; let Done _ r = f "" in M.mapM_ (Prelude.putStrLn . Prelude.show . render) $ Prelude.filter isTheorem r
