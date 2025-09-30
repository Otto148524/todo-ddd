module Main (main) where

import Application.TodoService
import Application.DTO.Facade

import Infrastructure.EventStore.InMemory
import Infrastructure.Web.Server

import Network.Wai.Handler.Warp

-- Helper function to initialize sample data
initializeSampleData :: AppConfig -> IO ()
initializeSampleData config = runAppIO config $ do
  let service = mkTodoService domainOps :: TodoService AppIO
  _ <- initiateTaskImpl service "ヘキサゴナルアーキテクチャを学ぶ"
  _ <- initiateTaskImpl service "イベントソーシングを理解する"
  _ <- initiateTaskImpl service "ドメイン駆動設計を実践する"
  return ()

main :: IO ()
main = do
  config <- initializeApp

  -- Initialize with some sample data
  initializeSampleData config

  putStrLn "========================================="
  putStrLn "Todo App Server (Event Sourcing + Hexagonal Architecture)"
  putStrLn "========================================="
  putStrLn "Server starting on http://localhost:8080"
  putStrLn ""
  putStrLn "Initial tasks created:"
  putStrLn "  ✓ ヘキサゴナルアーキテクチャを学ぶ"
  putStrLn "  ✓ イベントソーシングを理解する"
  putStrLn "  ✓ ドメイン駆動設計を実践する"
  putStrLn ""
  putStrLn "API Endpoints:"
  putStrLn "  GET  /api/tasks        - Get all tasks and statistics"
  putStrLn "  POST /api/tasks        - Create/Initiate new task"
  putStrLn "  POST /api/tasks/toggle - Toggle a task completion"
  putStrLn "  POST /api/tasks/delete - Delete task"
  putStrLn "  GET  /api/events       - Get event history"
  putStrLn "========================================="

  run 8080 (app config)
