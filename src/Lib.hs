module Lib
  ( cmdFunc,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import System.Environment (lookupEnv)
import System.IO
import System.Directory (doesFileExist)
import Data.Maybe (fromMaybe)



switchRoleLink :: String
switchRoleLink =
  "https://us-east-1.console.aws.amazon.com/cloudformation/home?region=us-east-1#/stacks/create/review?"

readPreviousInput :: FilePath -> IO (Maybe String)
readPreviousInput filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then Just <$> readFile filePath
    else return Nothing

writeInput :: FilePath -> String -> IO ()
writeInput filePath input = writeFile filePath input

promptWithHistory :: String -> FilePath -> IO String
promptWithHistory prompt filePath = do
  previousInput <- readPreviousInput filePath
  let defaultMessage = maybe "" (\prev -> " (press enter to use \"" ++ prev ++ "\")") previousInput
  putStr $ prompt ++ defaultMessage ++ ": "
  hFlush stdout
  input <- getLine
  if null input
    then return $ fromMaybe "" previousInput
    else writeInput filePath input >> return input

addQueryParams :: String -> [(String, String)] -> String
addQueryParams url [] = url
addQueryParams url ((key, value) : rest)
  | null value = addQueryParams url rest
  | otherwise = addQueryParams (url ++ "&" ++ key ++ "=" ++ value) rest

cmdFunc :: IO ()
cmdFunc = do
  loadFile defaultConfig
  bucket <- lookupEnv "TEMPLATE_STORING_BUCKET"
  let bucketURL = case bucket of
        Just b -> "https://" ++ b ++ ".s3.us-east-1.amazonaws.com"
        Nothing -> error "TEMPLATE_STORING_BUCKET is not set in .env file."
  let baseStackName = "switch-role-stack"
  sourceAwsAccountId <- promptWithHistory "Enter Source AWS Account ID" "history/source-aws-account-id.txt"
  sourceIamUserName <- promptWithHistory "Enter Source IAM User Name" "history/source-iam-user-name.txt"
  targetAwsAccountId <- promptWithHistory "Enter Target AWS Account ID" "history/target-aws-account-id.txt"

  -- show all inputs
  putStrLn "============================="
  putStrLn "The following parameters will be used:"
  putStrLn $ "Source AWS Account ID: " ++ sourceAwsAccountId
  putStrLn $ "Source IAM User Name: " ++ sourceIamUserName
  putStrLn $ "Target AWS Account ID: " ++ targetAwsAccountId

  -- create links
  let adminStackName = "admin-" ++ baseStackName ++ "-for-" ++ sourceAwsAccountId ++ "-" ++ sourceIamUserName
  let viewerStackName = "viewer-" ++ baseStackName ++ "-for-" ++ sourceAwsAccountId ++ "-" ++ sourceIamUserName

  let adminStackLink = addQueryParams switchRoleLink [("templateURL", bucketURL ++ "/admin-switch-role.yaml"), ("stackName", adminStackName), ("param_SourceAWSAccountId", sourceAwsAccountId), ("param_SourceIAMUserName", sourceIamUserName), ("param_XXXTargetAWSAccountId", targetAwsAccountId), ("param_XXXBucketURL", bucketURL)]
  let viewerStackLink = addQueryParams switchRoleLink [("templateURL", bucketURL ++ "/viewer-switch-role.yaml"), ("stackName", viewerStackName), ("param_SourceAWSAccountId", sourceAwsAccountId), ("param_SourceIAMUserName", sourceIamUserName), ("param_XXXTargetAWSAccountId", targetAwsAccountId), ("param_XXXBucketURL", bucketURL)]

  -- show links
  putStrLn "============================="
  putStrLn $ "Admin Switch Role Stack: " ++ adminStackLink
  putStrLn $ "Viewer Switch Role Stack: " ++ viewerStackLink
