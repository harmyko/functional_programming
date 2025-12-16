{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Lib1
import qualified Lib2
import qualified Lib3
import qualified Lib4

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar, readTVarIO)
import Control.Concurrent (Chan, forkIO, newChan, writeChan, threadDelay)
import Control.Monad.Trans.State.Strict (runState)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Types.Status (status400)
import System.IO (hFlush, stdout)
import Control.Exception (finally)

main :: IO ()
main = do
    putStrLn "Starting server..."
    
    -- Initialize state and storage channel
    stateVar <- newTVarIO Lib3.emptyState
    storageChan <- newChan
    
    -- Start storage loop in separate thread
    _ <- forkIO $ Lib3.storageOpLoop storageChan
    
    -- Load state from file
    putStrLn "Loading state from file..."
    loadResult <- Lib3.load storageChan stateVar
    case loadResult of
        Right () -> putStrLn "State loaded successfully"
        Left err -> putStrLn $ "Failed to load state: " ++ err
    
    -- Set up graceful shutdown
    let saveAndExit = do
            putStrLn "\nShutting down server..."
            saveResult <- Lib3.save storageChan stateVar
            case saveResult of
                Right () -> putStrLn "State saved successfully"
                Left err -> putStrLn $ "Failed to save state: " ++ err
            return ()
    
    -- Start periodic save thread
    _ <- forkIO $ periodicSave storageChan stateVar
    
    -- Run server with exception handling
    scotty 3000 (app stateVar) `finally` saveAndExit

-- Periodic save every 30 seconds
periodicSave :: Chan Lib3.StorageOp -> TVar Lib3.State -> IO ()
periodicSave chan stateVar = do
    threadDelay (30 * 1000000) -- 30 seconds
    _ <- Lib3.save chan stateVar
    periodicSave chan stateVar

app :: TVar Lib3.State -> ScottyM ()
app stateVar = do
    middleware logStdoutDev
    
    -- Health check endpoint
    get "/health" $ do
        text "Server is running"
    
    -- Command execution endpoint
    post "/execute" $ do
        rawBody <- body
        let commandStr = BL.unpack rawBody
        
        -- Parse command using Lib4 parser
        let parseResult = runState (runExceptT Lib4.parseCommand) commandStr
        case parseResult of
            (Left err, _) -> do
                status status400
                text $ pack $ "Parse error: " ++ err
            (Right cmd, _) -> do
                -- Execute command
                result <- liftIO $ executeCommand stateVar cmd
                text $ pack result

executeCommand :: TVar Lib3.State -> Lib1.Command -> IO String
executeCommand stateVar cmd = case cmd of
    Lib1.AddVehicle vehicle -> do
        atomically $ do
            state <- readTVar stateVar
            let newState = Lib3.addVehicle state vehicle
            writeTVar stateVar newState
        return "Vehicle added successfully"
    
    Lib1.FilterByPlate plate -> do
        state <- readTVarIO stateVar
        let result = Lib3.filterByPlate state plate
        case result of
            Just v -> return $ "Found vehicle: " ++ Lib3.showVehicle v
            Nothing -> return $ "No vehicle found with plate: " ++ plate
    
    Lib1.AddPassenger plate sex age -> do
        result <- atomically $ do
            state <- readTVar stateVar
            case Lib3.addPassenger state plate sex age of
                Left err -> return $ Left err
                Right newState -> do
                    writeTVar stateVar newState
                    return $ Right ()
        case result of
            Right () -> return "Passenger added successfully"
            Left err -> return $ "Error: " ++ err
    
    Lib1.CalculateAverageAge vehicle -> do
        let avg = Lib3.calculateAverageAge vehicle
        return $ "Average age: " ++ show avg
    
    Lib1.Sequence cmd1 cmd2 -> do
        result1 <- executeCommand stateVar cmd1
        result2 <- executeCommand stateVar cmd2
        return $ result1 ++ "\n" ++ result2
    
    Lib1.Dump Lib1.Examples -> do
        let examples = map Lib2.toCliCommand Lib1.examples
        return $ unlines ("Examples:" : map ("  " ++) examples)
