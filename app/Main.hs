module Main (main) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Foldable
import Lib
import System.Process
import Text.Printf

type Samples = Float
type Seconds = Float
type Hz = Float
type Pulse = Float

outputPath :: FilePath
outputPath = "output.bin"

-- O volume corresponde à amplitude das ondas
volume :: Float
volume = 0.5

-- Como as ondas digitais não são continuas,
--  o sample rate é a taxa de samples por segundo
--  que vao ser retidas. Vamos usar 44.100 (44.1kHz) por padrão.
sampleRate :: Samples
sampleRate = 44100.0

-- O Pitch Standard corresponde a frequencia 440Hz
--  e é usado como a nota de afinação padrão (ISO 16).
-- Por isso, vamos usá-la como a nota base. 
pitchStandard :: Hz
pitchStandard = 440.0


-- a função freq é basicamente f(x)=a sen(b (x+c))
-- onde "a" é o volume e "b" é o step (de acordo com o hz)
-- "c" seria o offset, mas não usaremos
freq :: Hz -> Seconds -> [Pulse]
freq hz duration = 
  map (* volume) $
  map sin $
  map (* step) [0.0 .. sampleRate * duration ]
  where
    step = (hz * 2 * pi) / sampleRate      

wave :: [Pulse]
wave = concat [freq 440.0 1, freq 540.0 1]

save :: FilePath -> IO ()
save path = BL.writeFile path $ BB.toLazyByteString $ fold $ map BB.floatLE wave

play :: IO ()
play = do 
  save outputPath
  runCommand $ printf "ffplay -f f32le -ar 44100 %s" outputPath
  return ()



main :: IO ()
main = someFunc
