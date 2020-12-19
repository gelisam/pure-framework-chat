{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, ViewPatterns #-}
module Main where

import Data.List.Index

import ImperativeVty
import PureFramework.TUI
import PureFramework.TUI.TextPicture


type Username = String
type Message = (Username, String)

data Chat = Chat
  { chatEditbox :: String
  }

initialChat :: Chat
initialChat = Chat ""

readEditbox :: Chat -> String
readEditbox = chatEditbox

modifyEditbox :: (String -> String) -> Chat -> Chat
modifyEditbox f chat@(Chat {..}) = chat
  { chatEditbox = f chatEditbox
  }

wrapText :: Int -> String -> [String]
wrapText w s
  | length s <= w
    = [s]
  | otherwise
    = take w s : wrapText w (drop w s)

-- take as much space as needed for the edit box, then fill as much of
-- the recent history as will fit.
renderChat :: Chat -> [Message] -> (Int, Int) -> TextPicture
renderChat chat msgs (ww, hh)
  = border (0, 0) (maxX, dividerY)
 <> border (0, dividerY) (maxX, maxY)
 <> Translated (2, dividerY+1) (textBlock wrappedEditbox)
 <> Translated (2, 1) (renderMessages msgs (w, dividerY-1))
  where
    maxX = ww-1
    maxY = hh-1
    w = ww - 4  -- leave space for the a bit of padding
    wrappedEditbox = wrapText w ("> " ++ readEditbox chat ++ "_")
    editboxHeight = length wrappedEditbox
    dividerY = maxY - editboxHeight - 1

renderMessages :: [Message] -> (Int, Int) -> TextPicture
renderMessages msgs (w, h)
  = textBlock block
  where
    renderMessage (username, msg) = username ++ ": " ++ msg
    blocks = fmap (wrapText w . renderMessage) msgs
    block = reverse . take h . concatMap reverse $ blocks


-- we don't really need this layer of indirection, but it makes the signature
-- of handleUsernameFormKey easier to understand in the blog post.
data UsernameForm = UsernameForm
  { formUsername :: Username
  }

initialUsernameForm :: UsernameForm
initialUsernameForm = UsernameForm "user"

readUsername :: UsernameForm -> Username
readUsername (UsernameForm username) = username

modifyUsername :: (Username -> Username) -> UsernameForm -> UsernameForm
modifyUsername f (UsernameForm username) = UsernameForm (f username)

renderUsernameForm :: UsernameForm -> (Int, Int) -> TextPicture
renderUsernameForm (UsernameForm username)
  = centeredTextBlock [ "Please pick a username."
                      , ""
                      , "> " ++ username ++ "_"
                      ]

handleEditboxKey :: Key -> Maybe (String -> String)
handleEditboxKey = \case
  KBS
    -- backspace; erase the last character
    -> Just $ \s -> take (length s - 1) s
  KChar c
    -- add a character
    -> Just (++ [c])
  _ -- unrecognized, let the event bubble up
    -> Nothing

data Screen = Screen
  { render    :: Int -> (Int, Int) -> TextPicture
  , handleKey :: Int -> Key -> Maybe Screen
  }

data Status
  = Chosen Username
  | Choosing UsernameForm

hasChosen :: Status -> Bool
hasChosen = \case
  Chosen _
    -> True
  _ -> False

hasEveryoneChosen :: [Status] -> Maybe [Username]
hasEveryoneChosen = \case
  [] -> do
    pure []
  Chosen username : statuses -> do
    (username :) <$> hasEveryoneChosen statuses
  _ -> do
    Nothing

initialScreen :: Int  -- ^ user count
              -> Screen
initialScreen n = usernameScreen (replicate n (Choosing initialUsernameForm))

usernameScreen :: [Status]  -- one for each user
               -> Screen
usernameScreen statuses = Screen
  { render    = \clientNumber
             -> case statuses !! clientNumber of
                  Chosen _
                    -> centeredTextBlock
                         [ "Waiting for others to"
                         , "pick a username as well..."
                         ]
                  Choosing usernameForm
                    -> renderUsernameForm usernameForm
  , handleKey = \clientNumber
             -> case statuses !! clientNumber of
                  Chosen _
                    -> \case
                         KEsc -> do
                           -- quit
                           Nothing
                         _ -> do
                           pure $ usernameScreen statuses
                  Choosing usernameForm
                    -> \case
                         KEsc -> do
                           -- quit
                           Nothing
                         KEnter -> do
                           -- the current user picked a username
                           let statuses' = setAt clientNumber
                                                 (Chosen $ readUsername usernameForm)
                                                 statuses
                           case hasEveryoneChosen statuses' of
                             Just usernames -> do
                               pure $ chatLoopScreen (fmap (,initialChat) usernames) []
                             Nothing -> do
                               pure $ usernameScreen statuses'
                         (handleEditboxKey -> Just f) -> do
                           -- edit the username
                           let statuses' = setAt clientNumber
                                                 (Choosing $ modifyUsername f usernameForm)
                                                 statuses
                           pure $ usernameScreen statuses'
                         _ -> do
                           pure $ usernameScreen statuses
  }

chatLoopScreen :: [(Username, Chat)] -> [Message] -> Screen
chatLoopScreen chats msgs = Screen
  { render    = \clientNumber
             -> let (_, chat) = chats !! clientNumber
             in renderChat chat msgs
  , handleKey = \clientNumber
             -> let (username, chat) = chats !! clientNumber
             in \case
                  KEsc -> do
                    -- quit
                    Nothing
                  KEnter -> do
                    -- send the edit box's message, clear the edit box
                    let msg = (username, readEditbox chat)
                    let msgs' = msg : msgs
                    let chat' = modifyEditbox (const "") chat
                    let chats' = setAt clientNumber (username, chat') chats
                    pure $ chatLoopScreen chats' msgs'
                  (handleEditboxKey -> Just f) -> do
                    -- delegate to the edit box
                    let chat' = modifyEditbox f chat
                    let chats' = setAt clientNumber (username, chat') chats
                    pure $ chatLoopScreen chats' msgs
                  _ -> do
                    pure $ chatLoopScreen chats msgs
  }

chatTUI :: Int  -- ^ user count
        -> IO ()
chatTUI userCount = multiplayTUI (initialScreen userCount) (userCount - 1) render handleKey

main :: IO ()
main = chatTUI 2
