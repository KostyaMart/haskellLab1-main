{-# LANGUAGE OverloadedStrings #-}
module DAO
  ( -- Resources
    listResources
  , getResource
  , insertResource
  , updateResourceTitle
  , deleteResource
  , listResourceStats
  , searchResources
  , listUsers
  , insertUser
  , listAuthors
  , insertAuthor
  , deleteAuthor
  , linkAuthorToResource
  , getAuthorsForResource
  , addUrl
  , listUrlsForResource
  , deleteUrl
  , updateResourceMeta
  , insertUsageEvent
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Database.MySQL.Simple
                   ( Connection, query, query_, execute )
import           Database.MySQL.Simple.Types (Only (..))
import           Data.Int (Int64)

import           Models


getLastInsertId :: Connection -> IO Int
getLastInsertId conn = do
  rows <- query_ conn "SELECT LAST_INSERT_ID()" :: IO [Only Int64]
  case rows of
    Only i : _ -> pure (fromIntegral i)
    _          -> pure 0

rowToResource
  :: (Int, Text, Int, Maybe Text, Maybe Text, Maybe String, Maybe Int, Maybe Text)
  -> Resource
rowToResource (i, t, ty, ab, pu, _openedAtStr, udays, terms) =
  Resource i t ty ab pu Nothing udays terms

rowToResourceStats :: (Int, Text, Int, Int, Int, Int) -> ResourceStats
rowToResourceStats (i, t, v, d, l, b) = ResourceStats i t v d l b

rowToUser :: (Int, Text, Maybe Text, Text) -> User
rowToUser (i, u, e, r) = User i u e r

listResources :: Connection -> IO [Resource]
listResources conn = do
  rows <- query_ conn
    "SELECT id, title, type_id, abstract, purpose, DATE_FORMAT(opened_at,'%d-%m-%y'), usage_term_days, usage_terms \
    \FROM resources ORDER BY id DESC"
    :: IO [(Int, Text, Int, Maybe Text, Maybe Text, Maybe String, Maybe Int, Maybe Text)]
  pure (map rowToResource rows)

getResource :: Connection -> Int -> IO (Maybe Resource)
getResource conn rid = do
  rows <- query conn
    "SELECT id, title, type_id, abstract, purpose, DATE_FORMAT(opened_at,'%d-%m-%y'), usage_term_days, usage_terms \
    \FROM resources WHERE id = ?"
    (Only rid)
    :: IO [(Int, Text, Int, Maybe Text, Maybe Text, Maybe String, Maybe Int, Maybe Text)]
  pure $ case rows of
    []  -> Nothing
    x:_ -> Just (rowToResource x)

insertResource
  :: Connection
  -> Text -> Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text
  -> IO Int
insertResource conn title typeId abstract purpose openedAt usageTermDays usageTerms = do
  let q = "INSERT INTO resources (title, type_id, abstract, purpose, opened_at, usage_term_days, usage_terms) \
          \VALUES (?,?,?,?,STR_TO_DATE(?, '%d-%m-%y'),?,?)"
  _ <- execute conn q (title, typeId, abstract, purpose, openedAt, usageTermDays, usageTerms)
  getLastInsertId conn

updateResourceTitle :: Connection -> Int -> Text -> IO Int
updateResourceTitle conn rid newTitle =
  fromIntegral <$> execute conn "UPDATE resources SET title=? WHERE id=?" (newTitle, rid)

deleteResource :: Connection -> Int -> IO Int
deleteResource conn rid =
  fromIntegral <$> execute conn "DELETE FROM resources WHERE id=?" (Only rid)

listResourceStats :: Connection -> IO [ResourceStats]
listResourceStats conn = do
  rows <- query_ conn
    "SELECT resource_id, title, views, downloads, likes, bookmarks \
    \FROM v_resource_stats ORDER BY resource_id DESC"
    :: IO [(Int, Text, Int, Int, Int, Int)]
  pure (map rowToResourceStats rows)

searchResources :: Connection -> Text -> IO [Resource]
searchResources conn needle = do
  let like = T.concat ["%", needle, "%"]
  rows <- query conn
    "SELECT id, title, type_id, abstract, purpose, DATE_FORMAT(opened_at,'%d-%m-%y'), usage_term_days, usage_terms \
    \FROM resources WHERE title LIKE ? OR abstract LIKE ? OR purpose LIKE ? ORDER BY id DESC"
    (like, like, like)
    :: IO [(Int, Text, Int, Maybe Text, Maybe Text, Maybe String, Maybe Int, Maybe Text)]
  pure (map rowToResource rows)


listUsers :: Connection -> IO [User]
listUsers conn = do
  rows <- query_ conn "SELECT id, username, email, role FROM users ORDER BY id DESC"
    :: IO [(Int, Text, Maybe Text, Text)]
  pure (map rowToUser rows)

insertUser :: Connection -> Text -> Maybe Text -> Text -> IO Int
insertUser conn username email role = do
  _ <- execute conn "INSERT INTO users (username, email, role) VALUES (?,?,?)"
        (username, email, role)
  getLastInsertId conn


listAuthors :: Connection -> IO [Author]
listAuthors conn = do
  rows <- query_ conn "SELECT id, full_name, dept FROM authors ORDER BY id DESC"
    :: IO [(Int, Text, Maybe Text)]
  pure (map (\(i,n,d) -> Author i n d) rows)

insertAuthor :: Connection -> Text -> Maybe Text -> IO Int
insertAuthor conn fullName dept = do
  _ <- execute conn "INSERT INTO authors (full_name, dept) VALUES (?,?)" (fullName, dept)
  getLastInsertId conn

deleteAuthor :: Connection -> Int -> IO Int
deleteAuthor conn aid =
  fromIntegral <$> execute conn "DELETE FROM authors WHERE id=?" (Only aid)

linkAuthorToResource :: Connection -> Int -> Int -> IO Int
linkAuthorToResource conn rid aid =
  fromIntegral <$> execute conn
    "INSERT IGNORE INTO resource_authors (resource_id, author_id) VALUES (?,?)" (rid, aid)

getAuthorsForResource :: Connection -> Int -> IO [Author]
getAuthorsForResource conn rid = do
  rows <- query conn
    "SELECT a.id, a.full_name, a.dept \
    \FROM resource_authors ra JOIN authors a ON a.id = ra.author_id \
    \WHERE ra.resource_id = ? ORDER BY a.full_name"
    (Only rid)
    :: IO [(Int, Text, Maybe Text)]
  pure (map (\(i,n,d) -> Author i n d) rows)


addUrl :: Connection -> Int -> Text -> Maybe Text -> IO Int
addUrl conn rid url kind = do
  _ <- execute conn "INSERT INTO urls (resource_id, url, kind) VALUES (?,?,?)" (rid, url, kind)
  getLastInsertId conn

listUrlsForResource :: Connection -> Int -> IO [Url]
listUrlsForResource conn rid = do
  rows <- query conn
    "SELECT id, resource_id, url, kind FROM urls WHERE resource_id=? ORDER BY id DESC"
    (Only rid)
    :: IO [(Int, Int, Text, Maybe Text)]
  pure (map (\(i,r,u,k) -> Url i r u k) rows)

deleteUrl :: Connection -> Int -> IO Int
deleteUrl conn urlId =
  fromIntegral <$> execute conn "DELETE FROM urls WHERE id=?" (Only urlId)


updateResourceMeta
  :: Connection
  -> Int
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> Maybe Text
  -> Maybe Int
  -> IO Int
updateResourceMeta conn rid mt ma mp mo mud muT mty = do
  let q =
        "UPDATE resources SET \
        \  title            = COALESCE(?, title), \
        \  abstract         = COALESCE(?, abstract), \
        \  purpose          = COALESCE(?, purpose), \
        \  opened_at        = COALESCE(STR_TO_DATE(?, '%d-%m-%y'), opened_at), \
        \  usage_term_days  = COALESCE(?, usage_term_days), \
        \  usage_terms      = COALESCE(?, usage_terms), \
        \  type_id          = COALESCE(?, type_id) \
        \WHERE id = ?"
  fromIntegral <$> execute conn q (mt, ma, mp, mo, mud, muT, mty, rid)


insertUsageEvent :: Connection -> Int -> Maybe Int -> Text -> IO Int
insertUsageEvent conn rid mUid action =
  fromIntegral <$> execute conn
    "INSERT INTO usage_events (resource_id, user_id, action) VALUES (?,?,?)"
    (rid, mUid, action)
