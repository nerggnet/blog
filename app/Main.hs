{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Imports

import Control.Monad.Trans.Maybe
import Data.Typeable
import Web.Spock
import Web.Spock.Config
import qualified Database.MongoDB as DB
import qualified Database.MongoDB.Connection as DB
import qualified Database.MongoDB.Query as DB
import Lucid
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.Bson
import Network.HTTP.Types.Status
  
-- Model

data BlogPost =
  BlogPost { blogPostName :: T.Text
           , blogPostBody :: T.Text
           , blogPostImage :: (T.Text, T.Text)   -- (src, alt)
           , blogPostLinks :: [(T.Text, T.Text)] -- [(href, text)]
           }

blogPostToDocument :: BlogPost -> Document
blogPostToDocument b = [ "blogPostName" := (String (blogPostName b))
                       , "blogPostBody" := (String (blogPostBody b))
                       , "blogPostImageSrc" := (String (fst (blogPostImage b)))
                       , "blogPostImageAlt" := (String (snd (blogPostImage b)))
                       ] ++
                       [ (T.pack ("blogPostLinkHref" ++ (show n))) := (String (fst ((blogPostLinks b) !! (n-1)))) | n <- [1..(length (blogPostLinks b))] ] ++
                       [ (T.pack ("blogPostLinkText" ++ (show n))) := (String (snd href)) | href <- (blogPostLinks b), n <- [1..(length (blogPostLinks b))] ]
--                       [ (T.pack ("blogPostLinkText" ++ (show n))) := (String (snd ((blogPostLinks b) !! (n-1)))) | n <- [1..(length (blogPostLinks b))] ]

sampleBlogPosts :: [BlogPost]
sampleBlogPosts =
  let ba = BlogPost "First Blog Post" "How can I insert data into the data structure" ("http://tenggren.net/product.jpg", "Product") [("http://google.com", "Google"), ("http://svt.se", "SVT")]
      bb = BlogPost "Second Blog Post" "Getting warm" ("http://tenggren.net/kajak.jpg", "Kajak") [("http://haskell.org", "Haskell"), ("http://spock.li", "Spock")]
  in [ba, bb]

data Project =
  Project { projectName        :: T.Text
          , projectDescription :: T.Text
          , projectAuthors     :: T.Text
          }
  deriving (Show, Eq, Typeable)

projectToDocument :: Project -> Document
projectToDocument p = [ "projectName"        := (String (projectName p))
                      , "projectDescription" := (String (projectDescription p))
                      , "projectAuthors"     := (String (projectAuthors p))
                      ]

documentToProject :: Document -> Project
documentToProject document =
  let pName = "projectName" `at` document
      pDesc = "projectDescription" `at` document
      pAuth = "projectAuthors" `at` document
  in Project pName pDesc pAuth

-- Database Actions

fetchAllProjects :: DB.Pipe -> IO [Project]
fetchAllProjects pipe = do
  ps <- DB.access pipe DB.master "projects" run
  return $ map documentToProject ps
  where
    run = getAllProjects
    
getAllProjects :: DB.Action IO [DB.Document]
getAllProjects = DB.rest =<< DB.find (DB.select [] "projects") {DB.sort = ["projectName" =: (1 :: Int)]}

insertProject :: Project -> DB.Pipe -> IO ()
insertProject project pipe = do
  DB.access pipe DB.master "projects" run
  return ()
  where
    run = DB.insert "projects" (projectToDocument project)

-- Spock Actions

getProjects :: SpockAction DB.Pipe session state ()
getProjects = do
  allProjects <- runQuery fetchAllProjects
  lucid $ pageTemplate $ do
    h1_ "Projects"
    renderProjects allProjects
    link "/add-project" "Add Your Project!"

postProject :: SpockAction DB.Pipe session state ()
postProject = do
  maybeProject <- projectFromPOST
  case maybeProject of
    Nothing -> do
      lucid (p_ "A project was not submitted.")
      setStatus status400
    Just project -> do
      runQuery (insertProject project)
      redirect "/"

projectFromPOST :: SpockAction database session state (Maybe Project)
projectFromPOST = runMaybeT $ do
  name <- MaybeT $ param "name"
  desc <- MaybeT $ param "description"
  auth <- MaybeT $ param "author"
  return $ Project name desc auth

addProjectForm :: SpockAction database session state ()
addProjectForm =
  lucid $
    pageTemplate $ 
      form_ [ method_ "post", action_ "/projects" ] $ do
        p_ $ do
          label_ "Project  "
          input_ [ name_ "name"]
        p_ $ do
          label_ "Description  "
          input_ [ name_ "description" ]
        p_ $ do
          label_ "Author  "
          input_ [ name_ "author"]
        input_ [ type_ "submit", value_ "Add project"]

lucid :: Html () -> SpockAction database session state ()
lucid document = html (LT.toStrict (renderText document))

-- Spock Initialization and Routing

main :: IO ()
main = do
    sessionCfg <- defaultSessionCfg ()
    spockCfg <- defaultSpockCfg sessionCfg dbConn initialState
    runSpock 8080 $ spock spockCfg app

app :: SpockM DB.Pipe session state ()
app = do
  get "/" getProjects
  post "/projects" postProject
  get "/add-project" addProjectForm

initialState :: ()
initialState = ()

dbConn :: PoolOrConn DB.Pipe
dbConn =
  let pConfig  = PoolCfg 1 10 20
      cBuilder = ConnBuilder { cb_createConn  = DB.connect $ DB.host "127.0.0.1"
                             , cb_destroyConn = DB.close
                             , cb_poolConfiguration = pConfig
                             }
  in PCConn cBuilder

-- Lucid View 

projectToRow :: Project -> Html ()
projectToRow project =
  tr_ $ do
    td_ (toHtml (projectName project))
    td_ (toHtml (projectDescription project))
    td_ (toHtml (projectAuthors project))

renderProjects :: [Project] -> Html ()
renderProjects projects =
  table_ $ do
    thead_ $ do
      tr_ $ do
        th_ "Name"
        th_ "Description"
        th_ "Authors"
      tbody_ (foldMap projectToRow projects)


pageTemplate :: Html () -> Html ()
pageTemplate contents =
  doctypehtml_ (do head_ (do title_ "nerggnet / tenggren"
                             meta_ [charset_ "utf-8"]
                             link_ [ rel_ "stylesheet"
                                   , type_ "text/css"
                                   , href_ "//tenggren.net/stylesheet.css"
                                   ])
                   body_ contents)

link :: T.Text -> Html () -> Html ()
link url caption = a_ [href_ url] caption

