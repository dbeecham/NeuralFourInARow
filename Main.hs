import Control.Monad.State
import Control.Applicative
import Parser
import Square
import Neural
import Utilities
import Genetics
import System.IO
import System.Random

data World = World { population :: Population
                   , initialized :: Bool
                   } deriving Show

data Command = Create
            | Step Int
            | Inspect
            | Help
            | Quit deriving Show

command :: Parser Command
command = pinspect <|> pcreate <|> pstep <|> pquit <|> phelp

pinspect :: Parser Command
pinspect = do
    string "inspect"
    return Inspect

pcreate :: Parser Command
pcreate = do
    string "create" 
    return Create

pquit :: Parser Command
pquit = do
    string "quit" 
    return Quit

pstep :: Parser Command
pstep = do
    string "step" 
    many whitespace
    n <- integer
    return $ Step n

phelp :: Parser Command
phelp = do
    string "help"
    return Help

initial = World { population = [], initialized = False }

perform :: Command -> StateT World IO ()
perform Inspect = do
    world <- get
    if not (initialized world)
        then lift $ putStrLn "World not initialized yet."
        else do
            lift $ putStrLn $ "Population size: " ++ show (length (population world))
            lift $ putStrLn $ "Best individual: " ++ show (maximum (map fit (population world)))
    loop

perform Create = do
    pop <- lift $ randompopulation
    w <- get
    put $ w { population = pop, initialized = True }
    loop

perform Quit = do
    return ()

perform (Step n) = do
    world <- get
    if not (initialized world)
        then do
            lift $ putStrLn "World not initialized yet."
            loop
        else do
            g <- lift $ newStdGen
            let p = head $ drop n $ iterations g (population world)
            put $ world { population = p}
            loop

perform Help = do
    lift $ putStrLn "Available commands: create, step, inspect, help, quit."
    loop

loop :: StateT World IO ()
loop = do
    lift $ putStr "> "
    lift $ hFlush stdout
    input <- lift $ getLine
    case parse command input of
        (Just v, _) -> perform v
        (Nothing, _) -> do
            lift $ putStrLn "Unknown command."
            loop

main :: IO ()
main = do
    runStateT loop initial
    return ()
