{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Example.EC2
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString.Builder (hPutBuilder)
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.Monoid
import           Network.AWS.Data
import           Network.AWS.Batch
import           System.IO
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import Data.HashMap.Lazy


say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn


sendHelper :: AWSRequest a => a -> IO (Rs a) 
sendHelper req = do
    lgr <- newLogger Info stdout
    env <- newEnv Discover <&> set envLogger lgr
    runResourceT . runAWST env . within Ohio $ send req





-- name, parameter hashmap
jobDefReq :: Text -> HashMap Text Text -> Text -> RegisterJobDefinition
jobDefReq name params image = do 
    registerJobDefinition name Container & set rjdParameters params 
    & set rjdContainerProperties (Just container)
    where
        -- image location, vcpus, memory in MiB
        container = containerProperties image 2 2000


    
baeJobDef = jobDefReq "bae-job-def"  defaultParams "docker.darpa-brass.com/philip/brass-platform-alpine" 

defaultParams :: HashMap Text Text
defaultParams = jobParamsToHashMap (JobParams "Default" 45000 "Default")

data JobParams = JobParams {shellApi :: Text, thPort:: Int, params :: Text}

-- ugh. make internal function to actual job request
jobParamsToHashMap :: JobParams -> HashMap Text Text 
jobParamsToHashMap j = fromList [("shellApi", shellApi j), ("thPort", pack . show $ thPort j), ("params", params j)]



submitJob' :: JobParams ->  SubmitJob
submitJob' j = (submitJob "bae-job-4500" "bae-queue" "bae-job-def") & set sjParameters (jobParamsToHashMap j) 

-- One that takes the parameters in order
-- I should not use the triple boob. It is a bad thing. 
submitJob'' = ((.).(.).(.)) submitJob' JobParams




    {-insert "" . 
    insert "ApiKey" "Did Not Pass ApiKey"
    insert "" "" empty
-}

instanceOverview :: Region -> IO ()
instanceOverview r = do
    lgr <- newLogger Info stdout
    env <- newEnv Discover <&> set envLogger lgr

{-
    let pp x = mconcat
          [ "[instance:" <> build (x ^. insInstanceId) <> "] {"
          , "\n  public-dns = " <> build (x ^. insPublicDNSName)
          , "\n  tags       = " <> build (x ^. insTags . to show)
          , "\n  state      = " <> build (x ^. insState . isName . to toBS)
          , "\n}\n"
          ]
-}
    runResourceT . runAWST env . within r $ do
        res <- send describeComputeEnvironments
        say $ toText (show res)
        res  <- send (submitJob "bae-job-4500" "bae-queue" "bae-job-def") -- It would be nice to not have these hard coded
        liftIO $ putStrLn (show (view sjrsResponseStatus res))


main = instanceOverview Ohio
