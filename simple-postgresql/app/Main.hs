{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Lib

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]



connStr = "host=localhost dbname=brassq user=brassq password=test port=5433"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe ExternalTest)

        johnId <- insert $ Person "John Doe" $ Just 35
        janeId <- insert $ Person "Jane Doe" Nothing

        insert $ BlogPost "My fr1st p0st" johnId
        insert $ BlogPost "One more for good measure" johnId

        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])

        john <- get johnId
        liftIO $ print (john :: Maybe Person)

        delete janeId
        deleteWhere [BlogPostAuthorId ==. johnId]

{-
data Person = Person
    { personName :: !String
    , personAge :: !Int
    }
  deriving Show
-}
type ExternalTestId = Key ExternalTest
data ExternalTest = ExternalTest {anInt :: Int, aString :: String} deriving Show -- , atuple :: (Double, Double)}
instance PersistEntity ExternalTest where
    newtype Key ExternalTest = ExternalTestKey (BackendKey SqlBackend)
        deriving (PersistField, Show, Eq, Read, Ord)
    -- A Generalized Algebraic Datatype (GADT).
    -- This gives us a type-safe approach to matching fields with
    -- their datatypes.
    data EntityField ExternalTest typ where
        ETId   :: EntityField ExternalTest ExternalTestId
        ETAnInt :: EntityField ExternalTest Int
        ETString  :: EntityField ExternalTest String

    data Unique ExternalTest
    type PersistEntityBackend ExternalTest = SqlBackend

    toPersistFields (ExternalTest anint astring) =
        [ SomePersistField anint
        , SomePersistField astring
        ]

    fromPersistValues [intValue, stringValue] = ExternalTest
        <$> fromPersistValue intValue
        <*> fromPersistValue stringValue
    fromPersistValues _ = Left "Invalid fromPersistValues input"

    -- Information on each field, used internally to generate SQL statements
    persistFieldDef ETId = FieldDef
        (HaskellName "Id")
        (DBName "id")
        (FTTypeCon Nothing "ETId")
        SqlInt64
        []
        True
        NoReference
    persistFieldDef ETAnInt = FieldDef
        (HaskellName "anInt")
        (DBName "name")
        (FTTypeCon Nothing "Int")
        SqlString
        []
        True
        NoReference
    persistFieldDef ETString = FieldDef
        (HaskellName "aString")
        (DBName "age")
        (FTTypeCon Nothing "String")
        SqlInt64
        []
        True
        NoReference