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
type Beats = Float

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

-- Beats por minuto
bpm :: Beats
bpm = 136.0

-- Segundos por beat (tempo)
--  porque a duração de nossa frequência está em segundos.
beatDuration :: Seconds
beatDuration = 60.0 / bpm

-- Gera uma nota "n" semitons acima do pitch standard.
--  A duração está em beats.
note :: Semitones -> Beats -> [Pulse]
note n beats = freq (intervalo n) (beats * beatDuration)

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

darude :: [[Pulse]]
darude = [note 0 0.25,note 0 0.25,note 0 0.25,note 0 0.25,
         note 0 0.5,note 0 0.25,note 0 0.25,note 0 0.25,
         note 0 0.25,note 0 0.25,note 0 0.25,note 0 0.5, 
         note 5 0.25,note 5 0.25,note 5 0.25,note 5 0.25,
         note 5 0.25,note 5 0.25,note 5 0.5,note 3 0.25,
         note 3 0.25,note 3 0.25,note 3 0.25,note 3 0.25,
         note 3 0.25,note 3 0.5,note (-2) 0.25,note (-2) 0.25,
         note 0 0.25,note 0 0.25,note 0 0.25,note 0 0.25,
         note 0 0.5,note 0 0.25,note 0 0.25,note 0 0.25,
         note 0 0.25,note 0 0.25,note 0 0.25,note 0 0.5,
         note 5 0.25,note 5 0.25,note 0 0.25,note 0 0.25,
         note 0 0.25,note 0 0.25,note 0 0.5,note 0 0.25,
         note 0 0.25,note 0 0.25,note 0 0.25,note 0 0.25,
         note 0 0.25,note 0 0.5,note 5 0.25,note 5 0.25]

music :: [Pulse]
music = concat $ darude ++ darude

save :: FilePath -> [Pulse] -> IO ()
save path w = BL.writeFile path $ BB.toLazyByteString $ fold $ map BB.floatLE w

play :: IO ()
play = do 
  save outputPath music
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputPath
  return ()

main :: IO ()
main = someFunc
