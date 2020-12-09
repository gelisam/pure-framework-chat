{-# LANGUAGE LambdaCase, RecordWildCards, ViewPatterns #-}
module Main where

import Data.Set (Set)
import qualified Data.Set as Set

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
  { render    :: [Message] -> (Int, Int) -> TextPicture
  , handleKey :: Key -> Maybe ([Message], Screen)
  }

initialScreen :: Screen
initialScreen = usernameScreen initialUsernameForm

usernameScreen :: UsernameForm -> Screen
usernameScreen usernameForm = Screen
  { render    = \_ -> renderUsernameForm usernameForm
  , handleKey = \case
      KEsc
        -- quit
        -> Nothing
      KEnter
       -- the user picked a username; proceed to the chat loop
        -> pure
         $ pure
         $ chatLoopScreen (readUsername usernameForm) initialChat
      (handleEditboxKey -> Just f)
        -- edit the username
        -> pure
         $ pure
         $ usernameScreen
         $ modifyUsername f usernameForm
      _ -> pure
         $ pure
         $ usernameScreen usernameForm
  }

chatLoopScreen :: Username -> Chat -> Screen
chatLoopScreen username chat = Screen
  { render    = renderChat chat
  , handleKey = \case
      KEsc
        -- quit
        -> Nothing
      KEnter
        -- send the edit box's message, clear the edit box
        -> Just ( [(username, readEditbox chat)]
                , chatLoopScreen username
                $ modifyEditbox (const "")
                $ chat
                )
      (handleEditboxKey -> Just f)
        -- delegate to the edit box
        -> pure
         $ pure
         $ chatLoopScreen username
         $ modifyEditbox f
         $ chat
      _ -> pure
         $ pure
         $ chatLoopScreen username chat
  }

data ClientModel = ClientModel
  { clientMessages :: [Message]  -- most recent first
  , clientScreen   :: Screen
  }

data ServerModel = ServerModel
  { serverClients  :: Set ClientNumber
  , serverMessages :: [Message]  -- most recent first
  , serverScreen   :: Screen
  }

sendMessageToAll
  :: ServerModel
  -> Message
  -> ([SendToClient Message], ServerModel)
sendMessageToAll serverModel@(ServerModel {..}) msg
  = ( [ SendToClient clientNumber msg
      | clientNumber <- Set.toList serverClients
      ]
    , serverModel { serverMessages = msg : serverMessages }
    )

sendMessagesToAll
  :: ServerModel
  -> [Message]
  -> ([SendToClient Message], ServerModel)
sendMessagesToAll serverModel = \case
  []
    -> ([], serverModel)
  msg : msgs
    -> let (out1, serverModel') = sendMessageToAll serverModel msg
           (out2, serverModel'') = sendMessagesToAll serverModel' msgs
       in (out1 ++ out2, serverModel'')

main :: IO ()
main = clientServerTUI (pure clientInit) clientRender
                         clientHandleKey clientHandleToClient
                       serverInit serverRender
                         serverHandleKey serverHandleToServer
  where
    clientInit :: ClientModel
    clientInit = ClientModel
      { clientMessages = []
      , clientScreen   = initialScreen
      }

    serverInit :: ServerModel
    serverInit = ServerModel
      { serverClients  = Set.empty
      , serverMessages = []
      , serverScreen   = initialScreen
      }

    clientRender :: ClientModel -> (Int, Int) -> TextPicture
    clientRender (ClientModel {..})
      = render clientScreen clientMessages

    serverRender :: ServerModel -> (Int, Int) -> TextPicture
    serverRender (ServerModel {..})
      = render serverScreen serverMessages

    clientHandleKey
      :: ClientModel
      -> Key
      -> Maybe ([Message], ClientModel)
    clientHandleKey clientModel key = do
      let screen = clientScreen clientModel
      (out, screen') <- handleKey screen key
      let clientModel' = clientModel { clientScreen = screen' }
      pure (out, clientModel')

    serverHandleKey
      :: ServerModel
      -> Key
      -> Maybe ([SendToClient Message], ServerModel)
    serverHandleKey serverModel@(ServerModel {..}) key = do
      (msgs, screen') <- handleKey serverScreen key
      let serverModel' = serverModel { serverScreen = screen' }
      pure $ sendMessagesToAll serverModel' msgs

    clientHandleToClient
      :: ClientModel
      -> Maybe Message
      -> Maybe ([Message], ClientModel)
    clientHandleToClient clientModel maybeMessage = do
      let messages = clientMessages clientModel
      message <- maybeMessage
      let messages' = message : messages
      let clientModel' = clientModel { clientMessages = messages' }
      Just ([], clientModel')

    serverHandleToServer
      :: ServerModel
      -> ServerEvent Message
      -> Maybe ([SendToClient Message], ServerModel)
    serverHandleToServer serverModel = \case
      ClientConnected clientNumber -> do
        let clients = serverClients serverModel
        let clients' = Set.insert clientNumber clients
        let serverModel' = serverModel { serverClients = clients' }
        pure ([], serverModel')
      ClientDisconnected clientNumber -> do
        let clients = serverClients serverModel
        let clients' = Set.delete clientNumber clients
        let serverModel' = serverModel { serverClients = clients' }
        pure ([], serverModel')
      MessageFromClient _ msg -> do
        pure $ sendMessageToAll serverModel msg
