{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mappend, mempty, mconcat)
import qualified Data.Map as M

import Hakyll

main :: IO ()
main = hakyllWith config $ do
    -- Copy Google Webmaster tools site verification file
    match "static/google3074ca16b9074992.html" $ do
        route   $ constRoute "google3074ca16b9074992.html"
        compile $ copyFileCompiler

    -- .htaccess
    match "static/htaccess" $ do
        route   $ constRoute ".htaccess"
        compile $ copyFileCompiler

    -- Compile {less}
    match "style.less" $ do
        route   $ setExtension ".css"
        -- lessc can't read from stdin
        compile $ getResourceString >>> unixFilter "lessc" ["style.less", "-x"]

    -- Copy fonts
    match "font/*" $ do
        route   $ idRoute
        compile $ copyFileCompiler

    -- Copy Javascript
    match "js/*" $ do
        route   $ idRoute
        compile $ copyFileCompiler

    -- Copy images
    match "img/*" $ do
        route   $ idRoute
        compile $ copyFileCompiler

    -- Render posts
    group "feed" $ do
        match allPosts $ do
            route   $ setExtension ".html"
            compile $ myPageCompiler
                >>> applyTemplateCompilers ["feeditem"]
                >>> relativizeUrlsCompiler

    match (allPosts `mappend` inGroup Nothing) $ do
        route   $ setExtension ".html"
        compile $ myPageCompiler
            >>> applyTemplateCompilers ["post", "default"]
            >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ do
        route   $ idRoute
        compile $ readPageCompiler
            >>> myMetaA
            >>> arr (setField "homeclass" "active")
            >>> arr (setField "homehref" nullLink)
            >>> arr (setField "title" "Posts")
            >>> requireAllA (allPosts  `mappend` inGroup Nothing) addPostList
            >>> arr applySelf
            >>> applyTemplateCompilers ["default"]
            >>> relativizeUrlsCompiler

    -- Colophon
    match "colophon.html" $ do
        route   $ idRoute
        compile $ readPageCompiler
            >>> myMetaA
            >>> arr (setField "colophonclass" "active")
            >>> arr (setField "colophonhref" nullLink)
            >>> arr (setField "title" "Colophon")
            >>> applyTemplateCompilers ["default"]
            >>> relativizeUrlsCompiler

    -- 404
    match "404.shtml" $ do
        route   $ idRoute
        compile $ readPageCompiler
            >>> myMetaA
            >>> arr (setField "robots" "noindex, nofollow, noarchive, nocache")
            >>> arr (setField "title" "404")
            >>> applyTemplateCompilers ["default"]
            >>> relativizeUrlsCompiler

    -- Compile templates
    match "templates/*.html" $ compile templateCompiler

    -- Compile RSS feed
    match "atom.xml" $ route idRoute
    create "atom.xml" $
        requireAll_ (allPosts `mappend` inGroup (Just "feed"))
        >>> mapCompiler (arr $ copyBodyToField "description")
        >>> renderAtom feedConfiguration

-- | Helper functions
--

-- Consistent convention for links that don't go anywhere
-- FIXME: find a way to do this with <button> tags
nullLink :: String
nullLink = "javascript:void(0)"

-- Default setup is for individual post pages
myMetaA = arr (trySetField "robots" "index, follow")
    >>> arr (trySetField "homeclass" "")
    >>> arr (trySetField "colophonclass" "")
    >>> arr (trySetField "homehref" "/index.html")
    >>> arr (trySetField "colophonhref" "/colophon.html")
    >>> arr (trySetField "author" "Akshay Shah")
    >>> arr (renderDateField "date" "%e %B %Y" "Unknown Date")

myPageCompiler :: Compiler Resource (Page String)
myPageCompiler = readPageCompiler
    >>> addDefaultFields
    >>> myMetaA
    >>> arr applySelf
    >>> pageRenderPandoc

allPosts :: Pattern (Page String)
allPosts = parseGlob "posts/*/*.markdown"

applyTemplateCompilers :: [String] -> Compiler (Page String) (Page String)
applyTemplateCompilers [] = arr id
applyTemplateCompilers (x:xs) = applyTemplateCompiler templ >>> applyTemplateCompilers xs
    where templ = parseIdentifier ("templates/" ++ x ++ ".html")

pair :: [Page String] -> [(Page String, Page String)]
pair (x1:x2:xs) = (x1, x2) : pair xs
pair [x1] = [(x1, fromBody "")]
pair _ = []

-- Auxiliary compiler: given a list of posts and a template, combine
-- posts pairwise using the template and return another list of posts
--
applyPostRow :: [Page String] -> Template -> [Page String] 
applyPostRow posts template = do 
        -- Take a pair of two pages 
        (p1, p2) <- pair posts 
        -- Create a combined page 
        let p = fromMap $ M.fromList 
                    [("post1", pageBody p1), ("post2", pageBody p2)] 
        -- Return the rendered, combined page 
        return $ applyTemplate template p 

-- Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr recentFirst 
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> require "templates/postrow.html" applyPostRow
        >>> arr mconcat
        >>> arr pageBody

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Data Hacker, MD"
    , feedDescription = "Thoughts from a data scientist."
    , feedAuthorName  = "Akshay Shah"
    , feedAuthorEmail = "akshay@datahackermd.com"
    , feedRoot        = "http://datahackermd.com"
    }

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "rsync --checksum --delete -ave 'ssh -p 21098' _site/* roliri@datahackermd.com:public_html" }
