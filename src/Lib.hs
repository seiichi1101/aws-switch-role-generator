    module Lib
  ( cmdFunc,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import System.Environment (lookupEnv)
import System.IO
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)
import Data.Maybe (fromMaybe)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory)

switchRoleLink :: String
switchRoleLink =
  "https://us-east-1.console.aws.amazon.com/cloudformation/home?region=us-east-1#/stacks/create/review?"

readPreviousInput :: String -> String -> IO (Maybe String)
readPreviousInput fileDir fileName = do
  fileExists <- doesFileExist (fileDir ++ "/" ++ fileName)
  if fileExists
    then Just <$> readFile (fileDir ++ "/" ++ fileName)
    else return Nothing

writeInput :: String -> String -> String -> IO ()
writeInput fileDir fileName input = do
  dirExists <- doesDirectoryExist fileDir
  if not dirExists
    then createDirectory fileDir
    else return ()
  writeFile (fileDir ++ "/" ++ fileName) input
  

promptWithHistory :: String -> String -> String -> IO String
promptWithHistory prompt fileDir fileName = do
  previousInput <- readPreviousInput fileDir fileName
  let defaultMessage = maybe "" (\prev -> " (press enter to use \"" ++ prev ++ "\")") previousInput
  putStr $ prompt ++ defaultMessage ++ ": "
  hFlush stdout
  input <- getLine
  if null input
    then return $ fromMaybe "" previousInput
    else writeInput fileDir fileName input >> return input

addQueryParams :: String -> [(String, String)] -> String
addQueryParams url [] = url
addQueryParams url ((key, value) : rest)
  | null value = addQueryParams url rest
  | otherwise = addQueryParams (url ++ "&" ++ key ++ "=" ++ value) rest

cmdFunc :: IO ()
cmdFunc = do
  -- get dir path
  executablePath <- getExecutablePath
  let execDir = takeDirectory executablePath
  let dirPath = execDir ++ "/history"

  -- load .env
  onMissingFile (loadFile defaultConfig) (return ())
  bucket <- lookupEnv "TEMPLATE_STORING_BUCKET"
  case bucket of
        Just b -> writeInput dirPath "template-storing-bucket.txt" b
        Nothing -> return ()

  -- prompt for inputs
  templateStoringBucketName <- promptWithHistory "Enter Template Storing Bucket Name" dirPath "template-storing-bucket.txt"
  let templateBucketURL = "https://" ++ templateStoringBucketName ++ ".s3.us-east-1.amazonaws.com"
  sourceAwsAccountId <- promptWithHistory "Enter Source AWS Account ID" dirPath "source-aws-account-id.txt"
  sourceIamUserName <- promptWithHistory "Enter Source IAM User Name" dirPath "source-iam-user-name.txt"
  targetAwsAccountId <- promptWithHistory "Enter Target AWS Account ID" dirPath "target-aws-account-id.txt"
  let baseStackName = "switch-role-stack"
  -- show all inputs
  putStrLn "============================="
  putStrLn "The following parameters will be used:"
  putStrLn $ "Source AWS Account ID: " ++ sourceAwsAccountId
  putStrLn $ "Source IAM User Name: " ++ sourceIamUserName
  putStrLn $ "Target AWS Account ID: " ++ targetAwsAccountId

  -- create links
  let adminStackName = "admin-" ++ baseStackName ++ "-for-" ++ sourceAwsAccountId ++ "-" ++ sourceIamUserName
  let viewerStackName = "viewer-" ++ baseStackName ++ "-for-" ++ sourceAwsAccountId ++ "-" ++ sourceIamUserName

  let adminStackLink = addQueryParams switchRoleLink [("templateURL", templateBucketURL ++ "/admin-switch-role.yaml"), ("stackName", adminStackName), ("param_SourceAWSAccountId", sourceAwsAccountId), ("param_SourceIAMUserName", sourceIamUserName), ("param_XXXTargetAWSAccountId", targetAwsAccountId), ("param_XXXBucketURL", templateBucketURL)]
  let viewerStackLink = addQueryParams switchRoleLink [("templateURL", templateBucketURL ++ "/viewer-switch-role.yaml"), ("stackName", viewerStackName), ("param_SourceAWSAccountId", sourceAwsAccountId), ("param_SourceIAMUserName", sourceIamUserName), ("param_XXXTargetAWSAccountId", targetAwsAccountId), ("param_XXXBucketURL", templateBucketURL)]

  -- show links
  putStrLn "============================="
  putStrLn $ "Admin Switch Role Stack: " ++ adminStackLink
  putStrLn $ "Viewer Switch Role Stack: " ++ viewerStackLink
