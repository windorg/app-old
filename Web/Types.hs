module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

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
    = CardsAction
    | NewCardAction
    | ShowCardAction { cardId :: !(Id Card) }
    | CreateCardAction
    | EditCardAction { cardId :: !(Id Card) }
    | UpdateCardAction { cardId :: !(Id Card) }
    | DeleteCardAction { cardId :: !(Id Card) }
    deriving (Eq, Show, Data)
