{-# LANGUAGE OverloadedStrings #-}
module CLI (runCLI) where

import DB
import DAO
import Models

import Database.MySQL.Simple (Connection)

import qualified Data.Text as T
import           Data.Text (Text)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(LineBuffering))
import Text.Read (readMaybe)

runCLI :: IO ()
runCLI = do
  hSetBuffering stdout LineBuffering
  withPool $ \pool -> withConn pool $ \conn -> menuLoop conn


menuLoop :: Connection -> IO ()
menuLoop conn = do
  putStrLn "==============================="
  putStrLn "  Інформаційні ресурси кафедри"
  putStrLn "==============================="
  putStrLn "--- Ресурси ---"
  putStrLn " 1) Список ресурсів"
  putStrLn " 2) Додати ресурс"
  putStrLn " 3) Пошук ресурсів"
  putStrLn " 4) Статистика"
  putStrLn " 5) Видалити ресурс"
  putStrLn " 6) Оновити ресурс (title/abstract/purpose/terms/opened_at/type)"
  putStrLn "--- Користувачі ---"
  putStrLn " 7) Список користувачів"
  putStrLn " 8) Додати користувача"
  putStrLn "--- Автори ---"
  putStrLn " 9) Список авторів"
  putStrLn "10) Додати автора"
  putStrLn "11) Видалити автора"
  putStrLn "12) Привʼязати автора до ресурсу"
  putStrLn "13) Показати авторів ресурсу"
  putStrLn "--- URL-и ресурсу ---"
  putStrLn "14) Додати URL до ресурсу"
  putStrLn "15) Показати URL-и ресурсу"
  putStrLn "16) Видалити URL"
  putStrLn "--- Використання ---"
  putStrLn "17) Записати дію використання (view/download/like/bookmark)"
  putStrLn "18) Топ популярності (зі вʼю v_resource_stats)"
  putStrLn "------------------------------------"
  putStrLn " 0) Вихід"
  choice <- askInt "Ваш вибір (0..18):"

  case choice of
    1 -> do
      rs <- listResources conn
      mapM_ (putStrLn . T.unpack . renderR) rs
      back >> menuLoop conn
    2 -> do
      t  <- askText "Назва ресурсу:"
      ty <- askInt  "type_id (див. resource_types):"
      rid <- insertResource conn t ty Nothing Nothing Nothing Nothing Nothing
      putStrLn $ "Додано ресурс id=" ++ show rid
      back >> menuLoop conn
    3 -> do
      q <- askText "Пошук:"
      rs <- searchResources conn q
      mapM_ (putStrLn . T.unpack . renderR) rs
      back >> menuLoop conn
    4 -> do
      ss <- listResourceStats conn
      mapM_ print ss
      back >> menuLoop conn
    5 -> do
      i <- askInt "ID ресурсу для видалення:"
      n <- deleteResource conn i
      putStrLn $ "Видалено рядків: " ++ show n
      back >> menuLoop conn
    6 -> do
      rid <- askInt "ID ресурсу для оновлення:"
      putStrLn "Нове значення або порожньо, щоб лишити без змін."
      nt  <- askMaybeText "title:"
      na  <- askMaybeText "abstract:"
      np  <- askMaybeText "purpose:"
      no  <- askMaybeText "opened_at (YYYY-MM-DD):"
      nud <- askMaybeInt  "usage_term_days:"
      nuT <- askMaybeText "usage_terms:"
      nty <- askMaybeInt  "type_id:"
      changed <- updateResourceMeta conn rid nt na np no nud nuT nty
      putStrLn $ "Оновлено колонок: " ++ show changed
      back >> menuLoop conn

    7 -> do
      us <- listUsers conn
      mapM_ print us
      back >> menuLoop conn
    8 -> do
      u  <- askText      "username:"
      em <- askMaybeText "email (порожньо = немає):"
      ro <- askText      "role (student/staff/admin):"
      uid <- insertUser conn u em ro
      putStrLn $ "Створено користувача id=" ++ show uid
      back >> menuLoop conn

    9 -> do
      as <- listAuthors conn
      mapM_ print as
      back >> menuLoop conn
    10 -> do
      n  <- askText      "full_name:"
      dp <- askMaybeText "dept (порожньо = немає):"
      aid <- insertAuthor conn n dp
      putStrLn $ "Створено автора id=" ++ show aid
      back >> menuLoop conn
    11 -> do
      aid <- askInt "ID автора для видалення:"
      n   <- deleteAuthor conn aid
      putStrLn $ "Видалено рядків: " ++ show n
      back >> menuLoop conn
    12 -> do
      rid <- askInt "ID ресурсу:"
      aid <- askInt "ID автора:"
      n   <- linkAuthorToResource conn rid aid
      putStrLn $ "Додано звʼязок (вставок): " ++ show n
      back >> menuLoop conn
    13 -> do
      rid <- askInt "ID ресурсу:"
      as  <- getAuthorsForResource conn rid
      if null as then putStrLn "Авторів не знайдено." else mapM_ print as
      back >> menuLoop conn

    14 -> do
      rid <- askInt      "ID ресурсу:"
      u   <- askText     "URL:"
      k   <- askMaybeText "kind (homepage/raw/etc):"
      uid <- addUrl conn rid u k
      putStrLn $ "Додано url id=" ++ show uid
      back >> menuLoop conn
    15 -> do
      rid <- askInt "ID ресурсу:"
      us  <- listUrlsForResource conn rid
      if null us then putStrLn "URL-ів немає." else mapM_ print us
      back >> menuLoop conn
    16 -> do
      urlId <- askInt "ID URL для видалення:"
      n     <- deleteUrl conn urlId
      putStrLn $ "Видалено рядків: " ++ show n
      back >> menuLoop conn

    17 -> do
      rid <- askInt "ID ресурсу:"
      uid <- askMaybeInt "user_id (порожньо = NULL):"
      act <- askAction
      n   <- insertUsageEvent conn rid uid act
      putStrLn $ "Записано подій: " ++ show n
      back >> menuLoop conn

    18 -> do
      ss <- listResourceStats conn
      putStrLn "Популярність (views/downloads/likes/bookmarks):"
      mapM_ print ss
      back >> menuLoop conn

    0 -> putStrLn "Bye!"
    _ -> do
      putStrLn "Невірний вибір."
      back >> menuLoop conn


askText :: String -> IO Text
askText msg = do
  putStrPrompt msg
  T.pack <$> getLine

askMaybeText :: String -> IO (Maybe Text)
askMaybeText msg = do
  putStrPrompt msg
  s <- getLine
  pure $ if null s then Nothing else Just (T.pack s)

askInt :: String -> IO Int
askInt msg = do
  putStrPrompt msg
  line <- getLine
  case readMaybe line :: Maybe Int of
    Just n  -> pure n
    Nothing -> putStrLn "Потрібно ціле число." >> askInt msg

askMaybeInt :: String -> IO (Maybe Int)
askMaybeInt msg = do
  putStrPrompt msg
  line <- getLine
  if null line then pure Nothing
  else case readMaybe line :: Maybe Int of
         Just n  -> pure (Just n)
         Nothing -> putStrLn "Потрібно ціле число або порожньо." >> askMaybeInt msg

askAction :: IO Text
askAction = do
  putStrLn "Оберіть дію: 1=view, 2=download, 3=like, 4=bookmark"
  n <- askInt "№:"
  case n of
    1 -> pure "view"
    2 -> pure "download"
    3 -> pure "like"
    4 -> pure "bookmark"
    _ -> putStrLn "Невірний вибір." >> askAction

back :: IO ()
back = do
  putStrPrompt "Enter → продовжити..."
  _ <- getLine
  pure ()

putStrPrompt :: String -> IO ()
putStrPrompt msg = do
  putStr (msg <> " ")
  hFlush stdout

renderR :: Resource -> Text
renderR r = T.intercalate " | "
  [ T.pack (show (rId r))
  , rTitle r
  , "type=" <> T.pack (show (rTypeId r))
  ]
