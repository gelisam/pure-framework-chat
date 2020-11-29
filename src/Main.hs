{-# LANGUAGE LambdaCase, RecordWildCards, ViewPatterns #-}
module Main where

import ImperativeVty
import PureFramework.TUI


textBlock :: [String] -> TextPicture
textBlock ss
  = mconcat [ Translated (0, y) (Text s)
            | (y, s) <- zip [0..] ss
            ]

centeredTextBlock :: [String] -> (Int, Int) -> TextPicture
centeredTextBlock ss (ww, hh)
  = Translated (x, y) (textBlock ss)
  where
    w = maximum (0 : fmap length ss)
    h = length ss
    x = (ww - w) `div` 2
    y = (hh - h) `div` 2

-- (x1,y1)-----------+
--    |              |
--    |              |
--    +-----------(x2,y2)
border :: (Int, Int) -> (Int, Int) -> TextPicture
border (x1,y1) (x2,y2)
  = Translated (x1, y1) fullRow
 <> mconcat [ Translated (x1, y) emptyRow
            | y <- [y1+1..y2-1]
            ]
 <> Translated (x1, y2) fullRow
 where
   fullRow = Text ("+" ++ replicate (x2-x1-1) '-' ++ "+")
   emptyRow = Text "|" <> Translated (x2-x1, 0) (Text "|")


type Username = String

data Chat = Chat
  { chatMessages :: [(Username, String)]  -- most recent first
  , chatEditbox :: String
  }

initialChat :: Chat
initialChat = Chat [] ""

addMessage :: Username -> String -> Chat -> Chat
addMessage username msg chat@(Chat {..}) = chat
  { chatMessages = (username, msg) : chatMessages
  }

listMessages :: Chat -> [(Username, String)]  -- most recent first
listMessages = chatMessages

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
renderChat :: Chat -> (Int, Int) -> TextPicture
renderChat chat (ww, hh)
  = border (0, 0) (maxX, dividerY)
 <> border (0, dividerY) (maxX, maxY)
 <> Translated (2, dividerY+1) (textBlock wrappedEditbox)
 <> Translated (2, 1) (renderMessages (listMessages chat) (w, dividerY-1))
  where
    maxX = ww-1
    maxY = hh-1
    w = ww - 4  -- leave space for the a bit of padding
    wrappedEditbox = wrapText w ("> " ++ readEditbox chat ++ "_")
    editboxHeight = length wrappedEditbox
    dividerY = maxY - editboxHeight - 1

renderMessages :: [(Username, String)] -> (Int, Int) -> TextPicture
renderMessages messages (w, h)
  = textBlock block
  where
    renderMessage (username, msg) = username ++ ": " ++ msg
    blocks = fmap (wrapText w . renderMessage) messages
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
  { render    :: (Int, Int) -> TextPicture
  , handleKey :: Key -> Maybe Screen
  }

initialScreen :: Screen
initialScreen = usernameScreen initialUsernameForm

usernameScreen :: UsernameForm -> Screen
usernameScreen usernameForm = Screen
  { render    = renderUsernameForm usernameForm
  , handleKey = \case
      KEsc
        -- quit
        -> Nothing
      KEnter
       -- the user picked a username; proceed to the chat loop
        -> Just $ chatLoopScreen (readUsername usernameForm) initialChat
      (handleEditboxKey -> Just f)
        -- edit the username
        -> Just $ usernameScreen
                $ modifyUsername f usernameForm
      _ -> Just $ usernameScreen usernameForm
  }

chatLoopScreen :: Username -> Chat -> Screen
chatLoopScreen username chat = Screen
  { render    = renderChat chat
  , handleKey = \case
      KEsc
        -- quit
        -> Nothing
      KEnter
        -- add the edit box's message, clear the edit box
        -> Just $ chatLoopScreen username
                $ modifyEditbox (const "")
                $ addMessage username (readEditbox chat)
                $ chat
      (handleEditboxKey -> Just f)
        -- delegate to the edit box
        -> Just $ chatLoopScreen username
                $ modifyEditbox f
                $ chat
      _ -> Just $ chatLoopScreen username chat
  }

main :: IO ()
main = playTUI initialScreen render handleKey
