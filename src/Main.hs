{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Docker.Client

data ProjectImage = ProjectImage {
  piProject :: Text,
  piStage :: Text,
  piImage :: Image
} deriving (Eq, Show)

projectImagesToCleanup :: [Image] -> [ProjectImage]
projectImagesToCleanup =
  concatMap snd . map splitNewest . groupBy projectStageEq . mapMaybe toReerImage
  where toReerImage img =
          let labels = imageLabels img
              maybeFind what = listToMaybe [value | (Label name value) <- labels, name == what]
              prj =  maybeFind "project"
              stg = maybeFind "stage"
           in ProjectImage <$> prj <*> stg <*> (Just img)
        projectStageEq a b = (piProject a == piProject b) && (piStage a == piStage b)
        splitNewest pis =
          let maxCreated = maximum $ map (imageCreated . piImage) $ pis
           in partition ((==) maxCreated . imageCreated . piImage) pis

imagesToCleanup :: (MonadIO m, MonadMask m) => DockerT m [ImageID]
imagesToCleanup =
  map (imageId . piImage) . projectImagesToCleanup . fromRight undefined <$> listImages defaultListOpts

cleanupImage :: (MonadIO m, MonadMask m) => ImageID -> DockerT m ()
cleanupImage imgId = do
  deleteImage imgId
  liftIO $ T.putStrLn $ T.intercalate " " ["Deleting", fromImageID imgId, "..."]

main :: IO ()
main = do
  T.putStrLn "Cleaning up the old images..."
  h <- unixHttpHandler "/var/run/docker.sock"
  runDockerT (defaultClientOpts, h) $ do
    trashImages <- imagesToCleanup
    forM trashImages cleanupImage
  T.putStrLn "Done cleaning up the images!"
