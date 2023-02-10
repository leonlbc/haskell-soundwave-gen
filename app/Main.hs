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
type Semitones = Float

outputPath :: FilePath
outputPath = "output.bin"

-- O volume corresponde à amplitude das ondas
volume :: Float
volume = 0.3

-- Como as ondas digitais não são continuas,
--  o sample rate é a taxa de samples por segundo
--  que vao ser retidas. Vamos usar 44.100 (44.1kHz) por padrão.
sampleRate :: Samples
sampleRate = 44100.0

-- O Pitch Standard (A440) corresponde a frequencia 440Hz
--  e é usado como a nota de afinação padrão (ISO 16).
-- Por isso, vamos usá-la como a nota base. 
pitchStandard :: Hz
pitchStandard = 440.0

-- Essa é a formula para gerar as frequência do
--  sistema de oitavas (12-TET) a partir do pitch standard.
-- Nesse sistema os semitons sao separados por uma razão
-- logaritmica de raiz indice 12 de 2.
intervalo :: Semitones -> Hz
intervalo semi = pitchStandard * (2 ** (1.0/12.0)) ** semi

-- Gera uma nota "n" semitons acima do pitch standard.
note :: Semitones -> Seconds -> [Pulse]
note n duration = freq (intervalo n) duration

-- a função freq é f(x)=a sen(b(x))
--  onde "a" é o volume e "b" é o step
freq :: Hz -> Seconds -> [Pulse]
freq hz duration = 
  map (* volume) $
  zipWith (*) release $ zipWith (*) attack sinewave
  where
    step = (hz * 2 * pi) / sampleRate
    sinewave = map sin $ map (* step) [0.0 .. sampleRate * duration ]
    attack = map (min 1) [0.0, 0.001 .. ]
    release = reverse $ take (length sinewave) attack   

majorScale :: [Pulse]
majorScale = concat [note 0 duration,
               note 2 duration,
               note 4 duration,
               note 5 duration,
               note 7 duration,
               note 9 duration,
               note 11 duration,
               note 12 duration
              ]
  where duration = 1.0

save :: FilePath -> [Pulse] -> IO ()
save path w = BL.writeFile path $ BB.toLazyByteString $ fold $ map BB.floatLE w

play :: IO ()
play = do 
  save outputPath majorScale
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputPath
  return ()

main :: IO ()
main = someFunc
