module Main (main) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Foldable
import System.Process
import Text.Printf
import Data.Maybe
import qualified Data.List as L
import Data.Char

type Samples = Float
type Seconds = Float
type Hz = Float
type Pulse = Float
type Semitones = Float
type Beats = Float
type Octaves = Float
type Notation = String

outputPath :: FilePath
outputPath = "output.bin"

-- O volume corresponde à amplitude das ondas
-- English Translation: The volume corresponds to the amplitude of the wave
volume :: Float
volume = 0.5

-- Como as ondas digitais não são continuas,
--  o sample rate é a taxa de samples por segundo
--  que vao ser retidas. Vamos usar 44.100 (44.1kHz) por padrão.
-- English translation: As the digital waves can't be continuous, the sample rate is the
--  how many samples per second that are recorded.
--  We're going to use the 44.100 (44.1Hz) rate, as it is the standard for music.
sampleRate :: Samples
sampleRate = 44100.0

-- O Pitch Standard (A440) corresponde a frequencia 440Hz
--  e é usado como a nota de afinação padrão (ISO 16).
-- Por isso, vamos usá-la como a nota base. 
-- English Translation: The Pitch Standard (A440) corresponds
--  to the 440Hz frequency and is used as the standard tuning note (ISO 16).
--  So we will use it as the base note.
pitchStandard :: Hz
pitchStandard = 440.0

-- Essa é a formula para gerar as frequência do
--  sistema de oitavas (12-TET) a partir do pitch standard.
-- Nesse sistema os semitons sao separados por uma razão
-- logaritmica de raiz indice 12 de 2.
-- English Translation: This is the formula for generating octave system frequencies (12-TET) from standard pitch.
--  In this system the semitones are separated by a logarithmic ratio root index 12 of 2.
intervalo :: Semitones -> Hz
intervalo semi = pitchStandard * (2 ** (1.0/12.0)) ** semi

-- Beats Por Minuto
-- English Translation: Beats Per Minute
bpm :: Beats
bpm = 136.0

-- Segundos por beat (tempo)
--  porque a duração de nossa frequência está em  Hertz (ciclos por segundo).
-- English Translation: Seconds per beat (tempo), because the duration
--  of our frequency is in Hertz (cycles per second)
beatDuration :: Seconds
beatDuration = 60.0 / bpm

-- Gera uma nota "n" semitons acima/abaixo do pitch standard.
--  A duração está em beats.
-- English Translation: Generates a note "n" semitones above/below the standard pitch.
--  Duration is in beats (changes according to the BPM).
note :: Semitones -> Beats -> [Pulse]
note n beats = freq (intervalo n) (beats * beatDuration)

-- Gera uma nota a partir da notação musical
-- English Translation: Returns a note from the corresponding music notation.
noteByN :: Notation -> Octaves -> Beats -> [Pulse]
noteByN notation octave beats = freq (intervalo (notationToSemi notation octave)) (beats * beatDuration)

-- a função freq é f(x)=a sen(b(x))
--  onde "a" é o volume e "b" é o step
-- English Translation: This function, in mathematical notation, is f(x)=a sin(b(x))
--  where "a" is the volume and "b" is the step
freq :: Hz -> Seconds -> [Pulse]
freq hz duration = 
  map (* volume) $
  zipWith (*) release $ zipWith (*) attack sinewave
  where
    step = (hz * 2 * pi) / sampleRate
    sinewave = map sin $ map (* step) [0.0 .. sampleRate * duration ]
    attack = map (min 1) [0.0, 0.001 .. ]
    release = reverse $ take (length sinewave) attack   

-- Atualiza a função noteByN para receber uma tupla ("Notacao", Oitava)
noteByNTuple :: Beats -> (Notation, Octaves) -> [Pulse]
noteByNTuple beats x = noteByN (fst x) (snd x) beats

-- Gera um acorde com uma lista de tuplas notacao-oitava
-- English Translation: Generates a chord from a list of notation-octave tuples
chord :: [(Notation, Octaves)] -> Beats -> [Pulse]
chord noteList beats =
  map (/ln) $
  foldr (zipWith (+)) (repeat 0.0) $
  map (noteByNTuple beats) noteList
  where
    ln = fromIntegral $ length noteList
-- Example: [("A", 4), ("D", 4), ("E", 4)]

fullOctave :: [Notation]
fullOctave = ["A","A#","B","C","C#","D","D#","E","F","F#","G","G#"]

defaultOctave :: Octaves
defaultOctave = 4

-- Converte notação para semitons ex.: "A" 4 -> 0 ; "A#" 4 -> 1 ; ...
--   TODO: Tratar erro quando notation não existe
-- English Translation: Convert notation to semitones
--  E.g.: "A" 4 -> 0 ; "A#" 4 -> 1 ; ...
notationToSemi :: Notation -> Octaves -> Semitones
notationToSemi notation oct = (+) octDiff $ noteIndex
  where
    octDiff = (oct - defaultOctave) * 12.0
    noteIndex = fromIntegral $ fromJust $ (L.elemIndex (map toUpper notation) fullOctave)

save :: FilePath -> [Pulse] -> IO ()
save path w = BL.writeFile path $ BB.toLazyByteString $ fold $ map BB.floatLE w

play :: IO ()
play = do
  save outputPath darudeFourTimes
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputPath
  return ()

main :: IO ()
main = do
  play 

darudeFourTimes :: [Pulse]
darudeFourTimes = concat $ iterate ((++) darude) darude !! 3

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
