{-# LANGUAGE OverloadedStrings #-}
module LatexSearch (
   searchLatex
    , searchLatex
    , searchLatexFile
    , searchLatexFiles
    , searchLatexDir
    , searchLatexDirs
    )
  where

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator as C
import Data.Text as DT
import Control.Applicative
import Pipes
import Pipes.Text.IO as T
import Pipes.Prelude as PI
import Control.Monad as M
import qualified Data.Char as C
import System.Directory
import Data.List as L

--pend = string "\\end{" *> takeTill (=='}') <* char '}'
--penv = (\x y -> LatexEnv x (pack y)) <$> pbegin <*> manyTill anyChar pend
pbegin = string "\\begin{" *> takeTill (=='}') <* char '}'
pend n = string $ "\\end{" `append` n `append` "}"
penv = do n <- pbegin
          if n == "document" then return $ LatexEnv "document" ""
          else do c <- manyTill anyChar (pend n)
                  return $ LatexEnv n (pack c)

ppar = LatexPar . pack <$> manyTill anyChar (string "\n\n" <|> C.lookAhead (string "\\begin"))
platexblock = penv <|> ppar
platex = many platexblock

data MyLatex = LatexEnv Text Text | LatexPar Text | TheoremProof MyLatex MyLatex deriving (Show)


pipeMany :: Monad m => Parser a -> Text -> Pipe Text a m ()
pipeMany f t = do case parse f t of
                      Done i r -> yield r >> pipeMany f i
                      Partial g -> partialParse g
                      Fail _ _ _ -> return ()

  where partialParse g = do x <- await
                            case g x of
                              Done i r -> yield r >> pipeMany f i
                              Partial g' -> partialParse g'
                              Fail _ _ _ -> return ()


latexParsePipe :: Monad m => Pipe Text MyLatex m ()
latexParsePipe = pipeMany platexblock ""


pipeMap f = forever $ await >>= yield . f

render (LatexEnv n t) = DT.map C.toUpper n `append` ": " `append` t `append` "\n\n"
render (LatexPar t) = "Par:" `append` t `append` "\n\n"


renderLatex (LatexEnv n t) = "\\begin{" `append` n `append` "}" `append` t `append` "\\end{" `append` n `append` "}\n\n"
renderLatex (LatexPar t) = t `append` "\n\n"
renderLatex (TheoremProof t t') = renderLatex t `append` renderLatex t'

isTheorem (LatexEnv "theorem" _) = True
isTheorem (LatexEnv "lemma" _) = True
isTheorem (LatexEnv "corollary" _) = True
isTheorem _ = False


isProof (LatexEnv "proof" _) = True
isProof _ = False

getText (LatexPar x) = x
getText (LatexEnv _ x) = x


isEnvKey :: String -> Bool
isEnvKey x = Prelude.any (L.isInfixOf x) ["definition", "theorem", "lemma", "proof", "example"]

getName (LatexPar _) = Nothing
getName (LatexEnv n _) = Just (Prelude.show n)

checkWords a@(x:xs) t | isEnvKey x = ((L.isInfixOf x <$> getName t) == Just True) && (Prelude.all (`DT.isInfixOf` (getText t)) $ Prelude.map pack xs)
                      | otherwise = Prelude.all (`DT.isInfixOf` (getText t)) $ Prelude.map pack a





--checkPair xs (t, t') = (isTheorem t && isProof t' && (checkWords xs t || checkWords xs t')) || 

--latexSearchPipe xs = latexParsePipe >-> PI.filter (checkWords xs) >-> pipeMap (renderLatex)
latexSearchPipe xs = latexParsePipe >->  PI.filter (checkWords xs) >-> pipeMap (renderLatex)

--runPipe = runSafeT . runEffect
searchLatex xs = runSafeT $ runEffect (T.stdin >-> latexSearchPipe xs >-> T.stdout)

searchLatexFile f xs = runSafeT $ runEffect (T.readFile f >-> latexSearchPipe xs >-> T.stdout)


searchLatexFiles fs xs = M.mapM_ (flip searchLatexFile xs) fs

isTexFile :: String -> Bool
isTexFile = L.isSuffixOf "tex"

searchLatexDir d xs = (Prelude.filter isTexFile <$> getDirectoryContents d) >>= flip searchLatexFiles xs . (L.map (d++))

searchLatexDirs ds xs = M.mapM_ (flip searchLatexDir xs) ds


--checkParser f = do Partial f <- parse platex <$> TO.readFile f; let Done _ r = f "" in M.mapM_ (Prelude.putStrLn . Prelude.show . render) $ Prelude.filter isTheorem r
