{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Yesod.Auth.SmbClient (authSmbClient) where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import System.Authenticate.SmbClient (loginSmbClient)
import Text.Hamlet (hamlet)
import Yesod.Auth (
    AuthPlugin(..),
    Creds(..),
    Route(..),
    loginErrorMessage,
    setCreds,
    YesodAuth(..))
import Yesod.Core (
    lift,
    liftIO,
    notFound,
    toWidget)
import Yesod.Form (
    iopt,
    runInputPost,
    textField)

pid = "posixpam"

-- |The smbclient authentication plugin.
authSmbClient :: YesodAuth m =>
                 Text -- ^ Server
              -> Text -- ^ Domain
              -> AuthPlugin m
authSmbClient server domain =
    AuthPlugin pid dispatch login
  where
    dispatch "POST" [] = do
        input <- lift $ runInputPost $ (,)
                 <$> iopt textField "ident"
                 <*> iopt textField "password"
        case input of
            (Just ident, Just password) -> do
                auth <- validate ident password
                either failed success auth
            _ ->
                failed undefined
    dispatch _ _ = notFound

    validate ident password =
        liftIO $ loginSmbClient server domain ident password

    success ident =
        lift $ setCreds True $ Creds pid ident []
    failed _ =
        loginErrorMessage LoginR "Login failed."

    url = PluginR pid []
    login toMaster =
        toWidget [hamlet|
$newline never
    <div #pamlogin>
        <form method=post action=@{toMaster url} .form-horizontal>
            <div .control-group>
                <label .control-label>
                    Username
                <div .controls>
                    <input type=text name=ident required>
            <div .control-group>
                <label .control-label>
                    Password
                <div .controls>
                    <input type=password name=password required>
            <div .form-actions>
                <input type=submit .btn .btn-primary value="Login">|]
