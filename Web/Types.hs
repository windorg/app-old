module Web.Types where

import Application.Orphans
import Control.Monad (fail)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Generated.Types
import IHP.LoginSupport.Types
import IHP.ModelSupport
import IHP.Prelude
import qualified Optics

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)

data BoardController
  = BoardsAction
  | NewBoardAction
  | ShowBoardAction {boardId :: !(Id Board)}
  | CreateBoardAction
  | EditBoardAction {boardId :: !(Id Board)}
  | UpdateBoardAction {boardId :: !(Id Board)}
  | DeleteBoardAction {boardId :: !(Id Board)}
  deriving (Eq, Show, Data)

data CardController
  = ShowCardAction {cardId :: !(Id Card)}
  | CreateCardAction {boardId :: !(Id Board)}
  | EditCardAction {cardId :: !(Id Card)}
  | UpdateCardAction {cardId :: !(Id Card)}
  | DeleteCardAction {cardId :: !(Id Card)}
  deriving (Eq, Show, Data)

data CardUpdateController
  = ShowCardUpdateAction {cardUpdateId :: !(Id CardUpdate)}
  | CreateCardUpdateAction {cardId :: !(Id Card)}
  | EditCardUpdateAction {cardUpdateId :: !(Id CardUpdate)}
  | UpdateCardUpdateAction {cardUpdateId :: !(Id CardUpdate)}
  | DeleteCardUpdateAction {cardUpdateId :: !(Id CardUpdate)}
  deriving (Eq, Show, Data)

data UserController
  = NewUserAction
  | ShowUserAction {userId :: !(Id User)}
  | CreateUserAction
  | EditUserAction {userId :: !(Id User)}
  | UpdateUserAction {userId :: !(Id User)}
  | DeleteUserAction {userId :: !(Id User)}
  | UpdateFollowUserAction {userId :: !(Id User)}
  | UpdateUnfollowUserAction {userId :: !(Id User)}
  deriving (Eq, Show, Data)

data SessionsController
  = NewSessionAction
  | CreateSessionAction
  | DeleteSessionAction
  deriving (Eq, Show, Data)

data InboxController
  = ShowInboxAction
  deriving (Eq, Show, Data)

data FeedController
  = ShowFeedAction
  deriving (Eq, Show, Data)

instance HasNewSessionUrl User where
  newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data ReplySource = ReplySourceCard {cardId :: Id Card} | ReplySourceInbox
  deriving (Eq, Read, Show, Data)

data ReplyController
  = NewReplyAction {cardUpdateId :: Id CardUpdate, replySourceSerialized :: Text}
  | CreateReplyAction {cardUpdateId :: Id CardUpdate, replySourceSerialized :: Text}
  | EditReplyAction {replyId :: !(Id Reply), replySourceSerialized :: Text}
  | UpdateReplyAction {replyId :: !(Id Reply), replySourceSerialized :: Text}
  | -- Things have to start with "Update" etc or else wrong HTTP methods are deduced for them (e.g. GET for
    -- 'MarkReplyAsReadAction')
    UpdateMarkReplyAsReadAction {replyId :: !(Id Reply), replySourceSerialized :: Text}
  | DeleteReplyAction {replyId :: !(Id Reply), replySourceSerialized :: Text}
  deriving (Eq, Show, Data)

data Visibility
  = -- | Absolutely everybody can see
    VisibilityPublic
  | -- | Nobody but the author can see
    VisibilityPrivate
  deriving (Eq, Show)

instance FromJSON Visibility where
  parseJSON = Aeson.withText "Visibility" $ \case
    "public" -> pure VisibilityPublic
    "private" -> pure VisibilityPrivate
    s -> fail ("unknown visibility: " <> cs s)

instance ToJSON Visibility where
  toJSON = \case
    VisibilityPublic -> toJSON ("public" :: Text)
    VisibilityPrivate -> toJSON ("private" :: Text)

---

data ReplySettings = ReplySettings
  { visibility :: Visibility
  }
  deriving (Show, Generic)

instance FromJSON ReplySettings

instance ToJSON ReplySettings

instance Optics.LabelOptic "settings_" Optics.A_Lens Reply Reply ReplySettings ReplySettings where
  labelOptic =
    Optics.lens (get #settings) (flip (set #settings))
      Optics.% Optics.iso
        ( \x -> case Aeson.fromJSON x of
            Aeson.Success y -> y
            _ -> error "#settings_ could not parse the ReplySettings field"
        )
        toJSON

Optics.makeFieldLabelsNoPrefix ''ReplySettings

---

data CardUpdateSettings = CardUpdateSettings
  { -- | Whether the cardupdate is private or public
    visibility :: Visibility,
    -- | Who is following the thread (e.g. all users that replied to it). Does not include the card owner.
    subscribers :: Set (Id User)
  }
  deriving (Show, Generic)

instance FromJSON CardUpdateSettings

instance ToJSON CardUpdateSettings

instance Optics.LabelOptic "settings_" Optics.A_Lens CardUpdate CardUpdate CardUpdateSettings CardUpdateSettings where
  labelOptic =
    Optics.lens (get #settings) (flip (set #settings))
      Optics.% Optics.iso
        ( \x -> case Aeson.fromJSON x of
            Aeson.Success y -> y
            _ -> error "#settings_ could not parse the CardUpdateSettings field"
        )
        toJSON

Optics.makeFieldLabelsNoPrefix ''CardUpdateSettings

---

data CardSettings = CardSettings
  { visibility :: Visibility
  }
  deriving (Show, Generic)

instance FromJSON CardSettings

instance ToJSON CardSettings

instance Optics.LabelOptic "settings_" Optics.A_Lens Card Card CardSettings CardSettings where
  labelOptic =
    Optics.lens (get #settings) (flip (set #settings))
      Optics.% Optics.iso
        ( \x -> case Aeson.fromJSON x of
            Aeson.Success y -> y
            _ -> error "#settings_ could not parse the CardSettings field"
        )
        toJSON

Optics.makeFieldLabelsNoPrefix ''CardSettings

---

data BoardSettings = BoardSettings
  { visibility :: Visibility
  }
  deriving (Show, Generic)

instance FromJSON BoardSettings

instance ToJSON BoardSettings

instance Optics.LabelOptic "settings_" Optics.A_Lens Board Board BoardSettings BoardSettings where
  labelOptic =
    Optics.lens (get #settings) (flip (set #settings))
      Optics.% Optics.iso
        ( \x -> case Aeson.fromJSON x of
            Aeson.Success y -> y
            e -> error "#settings_ could not parse the BoardSettings field"
        )
        toJSON

Optics.makeFieldLabelsNoPrefix ''BoardSettings

---

data FeedItem 
  -- There might be more later (e.g. FeedItemCard)
  = FeedItemCardUpdate CardUpdate
  deriving (Show, Generic)