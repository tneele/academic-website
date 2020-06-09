{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import Yesod.Auth.Hardcoded
import Yesod.Auth.Message   (AuthMessage(InvalidLogin))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Data.Digest.Pure.SHA (sha256,showDigest)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Projects"
                    , menuItemRoute = ProjectsR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Publications"
                    , menuItemRoute = PublicationsR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Contact"
                    , menuItemRoute = ContactR
                    , menuItemAccessCallback = True
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_bootstrap_colours_css
            addStylesheet $ StaticR css_material_icons_css
            addStylesheet $ StaticR css_sticky_footer_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized ProjectsR _ = return Authorized
    isAuthorized PublicationsR _ = return Authorized
    isAuthorized ContactR _ = return Authorized

    isAuthorized (EditPublicationR _) _ = isAuthenticated
    isAuthorized AddPublicationR _ = isAuthenticated

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO app _source level =
        return $ appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

setPageName :: Text -> Widget
setPageName title = do
    app <- getYesod
    setTitle  $ toHtml $ title ++ (" - " :: Text) ++ (appSiteName $ appSettings app)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

data SiteManager = SiteManager
    { manUsername :: Text
    , manPassword :: Text
    -- ^ The password field stored the SHA256 hash of the username appended with a colon and the plaintext password
    }
    deriving Show

siteManagers :: [SiteManager]
siteManagers = [SiteManager "thomas" "334d63a3627d1aa6bcea49dc8a208cec03e1c3c38f65df4e974ffcd0126e3321"]

lookupUser :: Text -> Maybe SiteManager
lookupUser username = find (\m -> manUsername m == username) siteManagers

validPassword :: Text -> Text -> Bool
validPassword u p = case lookupUser u of
    Just m -> manPassword m == (pack . showDigest . sha256 . TLE.encodeUtf8 . TL.fromStrict $ u ++ ":" ++ p)
    Nothing -> False

instance YesodAuth App where
    type AuthId App = Text

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True


    authenticate Creds{..} =
        return $ case lookupUser credsIdent of
            Nothing -> UserError InvalidLogin
            Just m -> Authenticated $ manUsername m
    --authenticate creds = runDB $ do
    --    x <- getBy $ UniqueUser $ credsIdent creds
    --    case x of
    --        Just (Entity uid _) -> return $ Authenticated uid
    --        Nothing -> Authenticated <$> insert User
    --            { userIdent = credsIdent creds
    --            , userPassword = Nothing
    --            }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins app = [authHardcoded] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

    authHttpManager = error "authHttpManager"

    authLayout widget = liftHandler $ defaultLayout [whamlet|
        <div .container>
            <h1>Login
            ^{widget} |]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance YesodAuthPersist App where
    type AuthEntity App = SiteManager

    getAuthEntity = return . lookupUser

instance YesodAuthHardcoded App where
    validatePassword u p = return $ validPassword u p
    doesUserNameExist = return . isJust . lookupUser


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
