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
  _ <- createTodoImpl service "ヘキサゴナルアーキテクチャを学ぶ"
  _ <- createTodoImpl service "イベントソーシングを理解する"
  _ <- createTodoImpl service "ドメイン駆動設計を実践する"
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
  putStrLn "Initial todos created:"
  putStrLn "  ✓ ヘキサゴナルアーキテクチャを学ぶ"
  putStrLn "  ✓ イベントソーシングを理解する"
  putStrLn "  ✓ ドメイン駆動設計を実践する"
  putStrLn ""
  putStrLn "API Endpoints:"
  putStrLn "  GET  /api/todos        - Get all todos and statistics"
  putStrLn "  POST /api/todos        - Create new todo"
  putStrLn "  POST /api/todos/toggle - Toggle todo completion"
  putStrLn "  POST /api/todos/delete - Delete todo"
  putStrLn "  GET  /api/events       - Get event history"
  putStrLn "========================================="

  run 8080 (app config)
