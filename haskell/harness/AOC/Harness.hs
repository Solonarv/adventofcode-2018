module AOC.Harness where

import Control.Monad
import Data.Foldable  
import qualified Data.List as List
import Data.Proxy
import GHC.TypeLits
import System.Exit (die)
import System.IO (hSetEncoding, utf8, stdout, stderr)
import Text.Printf
import Text.Read

import Data.Finite
import Data.Finite.Internal -- We need this to define a PrintfArg (Finite n) instance
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified System.Console.ANSI as Ansi
import Options.Applicative

import AOC.Solution

type Day = Finite 25

data RunTarget = RunAll
               | RunSolution Day
               | RunSolutionPart Day [Part]

data Opts = O
  { oCfg          :: FilePath
  , oInputDataDir :: FilePath
  }

data Command = CmdFetchInput Opts Day
             | CmdTest       RunTarget
             | CmdSolve Opts RunTarget Bool

parseOpts :: Parser Opts
parseOpts = O
  <$> strOption 
    (  long "cfg"
    <> value "aoc.toml"
    <> showDefaultWith id
    <> metavar "CONFIG_FILE"
    <> help "Configuration file (TOML) to read session token from."
    )
  <*> strOption
    (  long "input"
    <> short 'i'
    <> value "input"
    <> showDefaultWith id
    <> metavar "DIR"
    <> help "Directory where input data is stored."
    )

parseDay :: Parser Day
parseDay = argument readMFinite
  (  metavar "DAY"
  <> help "Which day's challenge to fetch or run the solution for. Omit to run all solutions."
  )

parsePart :: Parser [Part]
parsePart = strArgument
  (  metavar "PART"
  <> help "Which part of a challenge to run. Omit to run all parts."
  )

parseRunTarget :: Parser RunTarget
parseRunTarget = runTarget <$> optional parseDay <*> optional parsePart
  where
    runTarget Nothing _ = RunAll
    runTarget (Just day) Nothing = RunSolution day
    runTarget (Just day) (Just part) = RunSolutionPart day part

parseCommand :: Parser Command
parseCommand =
  hsubparser
    (  command "fetch" 
        (info fetchCmd (progDesc "Fetch the input for a given day."))
    <> command "test"
        (info testCmd (progDesc "Test one or more solution(s) using the challenge's example inputs."))
    <> command "run"
        (info runCmd (progDesc "Run one or more solution(s) on the actual input."))
    )
  where
    fetchCmd = CmdFetchInput <$> parseOpts <*> parseDay
    testCmd = CmdTest <$> parseRunTarget
    runCmd = CmdSolve
      <$> parseOpts
      <*> parseRunTarget
      <*> switch
        (  long "submit"
        <> help "Automatically submit the answer."
        )

data Cfg = Cfg
  { cfgToken :: Maybe String
  }

parseCfgFile :: FilePath -> IO Cfg
parseCfgFile _ = pure (Cfg Nothing)

aocMain :: {- | The year we're in -} Int -> {- | The solutions -} Solutions -> IO ()
aocMain yr solutions = do
  hSetEncoding stdout utf8
  cmd <- execParser $ info (parseCommand <**> helper) (progDesc $ "Advent of Code " <> show yr <> " solutions.")
  case cmd of
    CmdFetchInput opts day      -> fetchInput opts day
    CmdTest target              -> runTest target solutions
    CmdSolve opts target upload -> runSolve opts target upload solutions

type Solutions = Vector Solution

solutionsFromList :: [Solution] -> Solutions
solutionsFromList = Vector.fromList

solutionForDay :: Solutions -> Day -> Maybe Solution
solutionForDay solutions day = solutions Vector.!? (fromIntegral day - 1)

die' :: String -> IO ()
die' s = do
  Ansi.hSetSGR stderr [Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Red ]
  die s

fetchInput :: Opts -> Day -> IO ()
fetchInput opts day = do
  printf "Fetching input for day %v.\n" day
  cfg <- parseCfgFile (oCfg opts)
  case cfgToken cfg of
    Nothing -> die' "Can't fetch input: missing session token!"
    Just tok -> die' "fetchInput: not implemented"

runTest :: RunTarget -> Solutions -> IO ()
runTest target solutions = case target of
  RunAll -> flip Vector.imapM_ solutions $ \i sln -> do
    printf "Running tests for day %v...\n" (i+1)
    runTestsOn sln (parts sln)
  RunSolution day -> case solutionForDay solutions day of
    Nothing -> die' $ printf "There is no solution for day %v!" day
    Just sln -> runTestsOn sln (parts sln)
  RunSolutionPart day ps -> case solutionForDay solutions day of
    Nothing -> die' $ printf "There is no solution for day %v!" day
    Just sln -> for_ ps $ \part ->
      if part `notElem` (parts sln)
        then die' $ printf "The solution for day %v does not have a part %v!" day part
        else runTestsOn sln [part]

runTestsOn :: Solution -> [Part] -> IO ()
runTestsOn Solution{tests,decodeInput,solvePart,showResult} parts =
  for_ (zip [1..] tests) $ \(n :: Int, input :=> expected) -> do
    Ansi.setSGR []
    printf "  Test #%v" n
    case decodeInput input of
      Nothing -> do
        Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red]
        printf "    Couldn't decode input.\n"
      Just dat -> for_ parts $ \part ->
        for_ (List.lookup part expected) $ \expectedResult -> do
          case solvePart part dat of
            Nothing -> do
              Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red]
              printf "    %v: [✘] No solution.\n" part
            Just raw -> do
              let result = showResult part raw
              if result == expectedResult
                then do
                  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Green]
                  printf "    %v: [✔] Passed.\n" part
                else do
                  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Red]
                  printf "    %v: [✘] Failed, expected: %v, got: %v\n" part expectedResult result

runSolve :: Opts -> RunTarget -> Bool -> Solutions -> IO ()
runSolve opts target upload solutions = case target of
  RunAll -> flip Vector.imapM_ solutions $ \i sln -> do
    runSolveOn (i+1) opts upload sln (parts sln)
  RunSolution day -> case solutionForDay solutions day of
    Nothing -> die' $ printf "There is no solution for day %v!" day
    Just sln -> runSolveOn (fromIntegral day) opts upload sln (parts sln)
  RunSolutionPart day ps -> case solutionForDay solutions day of
    Nothing -> die' $ printf "There is no solution for day %v!" day
    Just sln -> for_ ps $ \part ->
      if part `notElem` (parts sln)
        then die' $ printf "The solution for day %v does not have a part %v!" day part
        else runSolveOn (fromIntegral day) opts upload sln [part]

runSolveOn :: Int -> Opts -> Bool -> Solution -> [Part] -> IO ()
runSolveOn day opts upload Solution{decodeInput,solvePart,showResult} parts = do
  Ansi.setSGR []
  printf "Running solution for day %v...\n" day
  let infile = printf "%s/day%.2d.txt" (oInputDataDir opts) day
  input <- readFile infile
  case decodeInput input of
    Nothing -> do
      Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red]
      printf "  Couldn't decode input: %v\n" infile
    Just dat -> for_ parts $ \part -> do
      case solvePart part dat of
        Nothing  -> do
          Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red]
          printf "  %v: [✘] No solution.\n" part
        Just raw -> do
          let result = showResult part raw
          Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Green]
          printf "  %v: [✔] The solution is:\n  %v\n" part result
          when upload $ do
            cfg <- parseCfgFile (oCfg opts)
            case cfgToken cfg of
              Nothing -> do
                Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red]
                printf "Can't upload solution: missing session token!\n"
              Just tok -> printf "Solution upload: not implemented\n"

data SolutionFailure = CantDecode | NoSuchPart | NoSolution !Char

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight e = maybe (Left e) Right

instance PrintfArg (Finite n) where
  formatArg (Finite n) = formatArg n
  parseFormat (Finite n) = parseFormat n

readMFinite :: forall n. KnownNat n => ReadM (Finite n)
readMFinite = eitherReader $ maybeToRight errMsg . (packFinite <=< readMaybe)
  where errMsg = printf "Must be an integer between 0 and %d." (natVal @n Proxy)