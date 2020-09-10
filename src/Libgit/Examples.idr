module Libgit.Examples

import Control.Monad.Managed

import Libgit
import Libgit.FFI

-- Clone a repository and print some information about the success of the
-- operation.
export
testClone : String -> String -> String -> IO ()
testClone url localPath branch = do
  res <- withGit $ runManaged $ do
    eRes <- repository (GitRepositoryClone (MkCloneOpts False branch) url localPath)
    let result = case eRes of
                   Left res => "Error: " ++ show res
                   Right _ => "Cloned repository"
    liftIO $ putStrLn result
  case res of
    Left err => putStrLn ("Error initializing: " ++ show err)
    Right _ => putStrLn "Success"

-- Open a repository and reset its head to a given commit/tag
export
resetRepo : (path : String) -> (rev : String) -> IO ()
resetRepo path rev = do
  withGit $ runManaged $ do
    Right repo <- repository (GitRepositoryOpen path)
      | Left err => putStrLn ("Error opening repo: " ++ show err)
    Right (objTyp ** obj) <- revParse repo rev
      | Left err => putStrLn ("Error parsing revision: " ++ show err)
    case objTyp of
      GitObjectCommit => liftIO resetRepo
      GitObjectTag => liftIO resetRepo
      _ => liftIO (putStrLn "Wrong object type")
  pure ()
  where
    resetRepo : {auto repo : GitRepository}
             -> {auto typ : GitObjectType}
             -> {auto obj : GitObject typ}
             -> {auto 0 prf : IsCommitish typ}
             -> IO ()
    resetRepo {repo} {obj} = do
      0 <- liftIO (resetRepository repo obj GitResetHard)
        | err => putStrLn ("Error resetting repo: " ++ show err)
      putStrLn "Successfully reset repo"

-- Open a repository and fetch a remote
export
fetchRemote : (path : String) -> (remote : String) -> IO ()
fetchRemote path rev = do
  withGit $ runManaged $ do
    Right repo <- repository (GitRepositoryOpen path)
      | Left err => putError ("Error opening repo: " ++ show err)
    Right remote <- remote repo "origin"
      | Left err => putError ("Error looking up remote: " ++ show err)
    0 <- liftIO (remoteFetch' remote "Fetched from Idris")
      | err => putError ("Error fetching remote: " ++ show err)
    putStrLn "Fetch successful."
  pure ()
  where
    putError : HasIO io => String -> io ()
    putError msg = liftIO $ do
      putStrLn msg
      case lastError of
        Just (msg, _) => putStrLn msg
        Nothing => putStrLn "No git error present"