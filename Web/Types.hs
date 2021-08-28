module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types
import IHP.LoginSupport.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data BoardController
    = BoardsAction
    | NewBoardAction
    | ShowBoardAction { boardId :: !(Id Board) }
    | CreateBoardAction
    | EditBoardAction { boardId :: !(Id Board) }
    | UpdateBoardAction { boardId :: !(Id Board) }
    | DeleteBoardAction { boardId :: !(Id Board) }
    deriving (Eq, Show, Data)

data CardController
    = ShowCardAction { cardId :: !(Id Card) }
    | CreateCardAction { boardId :: !(Id Board) }
    | EditCardAction { cardId :: !(Id Card) }
    | UpdateCardAction { cardId :: !(Id Card) }
    | DeleteCardAction { cardId :: !(Id Card) }
    deriving (Eq, Show, Data)

data CardUpdateController
    = ShowCardUpdateAction { cardUpdateId :: !(Id CardUpdate) }
    | CreateCardUpdateAction { cardId :: !(Id Card) }
    | EditCardUpdateAction { cardUpdateId :: !(Id CardUpdate) }
    | UpdateCardUpdateAction { cardUpdateId :: !(Id CardUpdate) }
    | DeleteCardUpdateAction { cardUpdateId :: !(Id CardUpdate) }
    deriving (Eq, Show, Data)

data UserController
    = UsersAction
    | NewUserAction
    | ShowUserAction { userId :: !(Id User) }
    | CreateUserAction
    | EditUserAction { userId :: !(Id User) }
    | UpdateUserAction { userId :: !(Id User) }
    | DeleteUserAction { userId :: !(Id User) }
    deriving (Eq, Show, Data)

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)

instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User