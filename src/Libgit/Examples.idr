module Libgit.Examples

import Control.Monad.Managed

import Libgit

-- Clone a repository and print some information about the success of the
-- operation.
export
testClone : String -> String -> String -> IO ()
testClone url localPath branch = do
  res <- withGit $ runManaged $ do
    eRes <- managedRepository (Clone (MkCloneOpts False branch) url localPath)
    let result = case eRes of
                   Left res => "Error: " ++ show res
                   Right _ => "Cloned repository"
    liftIO $ putStrLn result
  case res of
    Left err => putStrLn ("Error initializing: " ++ show err)
    Right _ => putStrLn "Success"
