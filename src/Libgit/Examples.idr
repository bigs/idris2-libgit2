module Libgit.Examples

import Libgit

export
testClone : String -> String -> String -> IO ()
testClone url localPath branch = do
  result <- runGitT $ do
    eRes <- clone (MkCloneOpts False branch) url localPath
    let result = case eRes of
                   Left res => "Error: " ++ show res
                   Right _ => "Cloned repository"
    liftIO $ putStrLn result
  putError result
  where
    putError : Either Int () -> IO ()
    putError (Left res) = putStrLn $ "Error in shutdown: " ++ show res
    putError (Right x) = putStrLn "shut down clean"
