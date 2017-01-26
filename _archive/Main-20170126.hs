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
  deriving (Show, Eq, Typeable)

blogPostToDocument :: BlogPost -> Document
blogPostToDocument b = [ "blogPostName" := String (blogPostName b)
                       , "blogPostBody" := String (blogPostBody b)
                       , "blogPostImageSrc" := String (fst (blogPostImage b))
                       , "blogPostImageAlt" := String (snd (blogPostImage b))
                       ] ++ blogPostLinksToDocument (blogPostLinks b) 0

blogPostLinksToDocument :: [(T.Text, T.Text)] -> Int -> Document
blogPostLinksToDocument [] _     = []
blogPostLinksToDocument (l:ls) i = [ T.pack ("blogPostLinkHref" ++ show i) := String (fst l)
                                   , T.pack ("blogPostLinkText" ++ show i) := String (snd l)
                                   ] ++ blogPostLinksToDocument ls (i+1)

documentToBlogPost :: Document -> BlogPost
documentToBlogPost document =
  let bName = "blogPostName" `at` document
      bBody = "blogPostBody" `at` document
      bImageSrc = "blogPostImageSrc" `at` document
      bImageAlt = "blogPostImageAlt" `at` document
  in BlogPost bName bBody (bImageSrc, bImageAlt) (documentToBlogPostLinks document 0)

documentToBlogPostLinks :: Document -> Int -> [(T.Text, T.Text)]
documentToBlogPostLinks document index =
  let bLinkHref = document !? T.pack ("blogPostLinkHref" ++ show index)
      bLinkText = document !? T.pack ("blogPostLinkText" ++ show index)
  in case (bLinkHref, bLinkText) of
    (Nothing, _) -> []
    (_, Nothing) -> []
    (Just h, Just t) -> (h, t) : documentToBlogPostLinks document (index+1)

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
projectToDocument p = [ "projectName"        := String (projectName p)
                      , "projectDescription" := String (projectDescription p)
                      , "projectAuthors"     := String (projectAuthors p)
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

fetchAllBlogPosts :: DB.Pipe -> IO [BlogPost]
fetchAllBlogPosts pipe = do
  ps <- DB.access pipe DB.master "blogPosts" run
  return $ map documentToBlogPost ps
  where
    run = getAllBlogPosts

getAllProjects :: DB.Action IO [DB.Document]
getAllProjects = DB.rest =<< DB.find (DB.select [] "projects") {DB.sort = ["projectName" =: (1 :: Int)]}

getAllBlogPosts :: DB.Action IO [DB.Document]
getAllBlogPosts = DB.rest =<< DB.find (DB.select [] "blogPosts") {DB.sort = ["blogPostName" =: (1 :: Int)]}

insertProject :: Project -> DB.Pipe -> IO ()
insertProject project pipe = do
  DB.access pipe DB.master "projects" run
  return ()
  where
    run = DB.insert "projects" (projectToDocument project)

insertBlogPost :: BlogPost -> DB.Pipe -> IO ()
insertBlogPost blogPost pipe = do
  DB.access pipe DB.master "blogPosts" run
  return ()
  where
    run = DB.insert "blogPosts" (blogPostToDocument blogPost)

-- Spock Actions

getProjects :: SpockAction DB.Pipe session state ()
getProjects = do
  allProjects <- runQuery fetchAllProjects
  lucid $ pageTemplate $ do
    h1_ "Projects"
    renderProjects allProjects
    link "/add-project" "Add Your Project!"

getBlogPosts :: SpockAction DB.Pipe session state ()
getBlogPosts = do
  allBlogPosts <- runQuery fetchAllBlogPosts
  lucid $ pageTemplate $ do
    h1_ "Blog"
    renderBlogPosts allBlogPosts
    link "/add-blog-post" "Add Your Blog Post!"

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

postBlogPost :: SpockAction DB.Pipe session state ()
postBlogPost = do
  maybeBlogPost <- blogPostFromPOST
  case maybeBlogPost of
    Nothing -> do
      lucid (p_ "A blog post was not submitted.")
      setStatus status400
    Just blogPost -> do
      runQuery (insertBlogPost blogPost)
      redirect "/"

projectFromPOST :: SpockAction database session state (Maybe Project)
projectFromPOST = runMaybeT $ do
  name <- MaybeT $ param "name"
  desc <- MaybeT $ param "description"
  auth <- MaybeT $ param "author"
  return $ Project name desc auth

blogPostFromPOST :: SpockAction database session state (Maybe BlogPost)
blogPostFromPOST = runMaybeT $ do
  name <- MaybeT $ param "name"
  body <- MaybeT $ param "body"
  imageSrc <- MaybeT $ param "imageSrc"
  imageAlt <- MaybeT $ param "imageAlt" -- :: MaybeT (ActionCtxT () (WebStateM database session state)) T.Text
  linkHref0 <- MaybeT $ param "linkHref0"
  linkText0 <- MaybeT $ param "linkText0"
  linkHref1 <- MaybeT $ param "linkHref1"
  linkText1 <- MaybeT $ param "linkText1"
  linkHref2 <- MaybeT $ param "linkHref2"
  linkText2 <- MaybeT $ param "linkText2"
  linkHref3 <- MaybeT $ param "linkHref3"
  linkText3 <- MaybeT $ param "linkText3"
  linkHref4 <- MaybeT $ param "linkHref4"
  linkText4 <- MaybeT $ param "linkText4"
  return $ BlogPost name body (imageSrc, imageAlt) [(linkHref0, linkText0), (linkHref1, linkText1), (linkHref2, linkText2), (linkHref3, linkText3), (linkHref4, linkText4)]

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

addBlogPostForm :: SpockAction database session state ()
addBlogPostForm =
  lucid $
    pageTemplate $
      form_ [ method_ "post", action_ "/blog-posts" ] $ do
        p_ $ do
          label_ "Blog Post Name "
          input_ [ name_ "name" ]
        p_ $ do
          label_ "Body "
          input_ [ name_ "body"]
        p_ $ do
          label_ "Image Src "
          input_ [ name_ "imageSrc" ]
        p_ $ do
          label_ "Image Alt "
          input_ [ name_ "imageAlt" ]
        p_ $ do
          label_ "Link Href 0 "
          input_ [ name_ "linkHref0" ]
        p_ $ do
          label_ "Link Text 0 "
          input_ [ name_ "linkText0" ]
        p_ $ do
          label_ "Link Href 1 "
          input_ [ name_ "linkHref1" ]
        p_ $ do
          label_ "Link Text 1 "
          input_ [ name_ "linkText1" ]
        p_ $ do
          label_ "Link Href 2 "
          input_ [ name_ "linkHref2" ]
        p_ $ do
          label_ "Link Text 2 "
          input_ [ name_ "linkText2" ]
        p_ $ do
          label_ "Link Href 3 "
          input_ [ name_ "linkHref3" ]
        p_ $ do
          label_ "Link Text 3 "
          input_ [ name_ "linkText3" ]
        p_ $ do
          label_ "Link Href 4 "
          input_ [ name_ "linkHref4" ]
        p_ $ do
          label_ "Link Text 4 "
          input_ [ name_ "linkText4" ]
        input_ [ type_ "submit", value_ "Add Blog Post" ]

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

appBlog :: SpockM DB.Pipe session state ()
appBlog = do
  get "/" getBlogPosts
  post "/blog-posts" postBlogPost
  get "/add-blog-post" addBlogPostForm

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

blogPostToRow :: BlogPost -> Html ()
blogPostToRow blogPost =
  tr_ $ do
    td_ (toHtml (blogPostName blogPost))
    td_ (toHtml (blogPostBody blogPost))
    td_ (toHtml (T.pack ("(" ++ show (fst (blogPostImage blogPost)) ++ ", " ++ show (snd (blogPostImage blogPost)) ++ ")")))

renderProjects :: [Project] -> Html ()
renderProjects projects =
  table_ $
    thead_ $ do
      tr_ $ do
        th_ "Name"
        th_ "Description"
        th_ "Authors"
      tbody_ (foldMap projectToRow projects)

renderBlogPosts :: [BlogPost] -> Html ()
renderBlogPosts blogPosts =
  table_ $
    thead_ $ do
      tr_ $ do
        th_ "Name"
        th_ "Body"
        th_ "Image"
        th_ "Links"
      tbody_ (foldMap blogPostToRow blogPosts)

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
