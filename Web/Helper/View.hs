module Web.Helper.View 
( renderOwnBoard,
  renderOthersBoard,
  renderUserPageBoard,
  lockIcon,
  space,
  -- Crumbs
  userCrumb,
  boardCrumb,
  cardCrumb,
  -- Gravatars
  gravatarSmall,
) where

import Web.View.Prelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import Named
import Data.Digest.Pure.MD5 (md5)

renderOwnBoard :: Board -> Html
renderOwnBoard board = [hsx|
    <div class={"woc-board card mt-3 mb-3 " <> if private then "woc-board-private" else "" :: Text}>
        <div class="card-body">
            <h3>
                {when private (lockIcon <> space)}
                <a href={ShowBoardAction (get #id board)}>{get #title board}</a>
                {renderBoardEditButton board}
                {renderBoardDeleteButton board}
            </h3>
        </div>
    </div>
|]
  where
    private = case board ^. #settings_ % #visibility of
        VisibilityPublic -> False
        VisibilityPrivate -> True

renderBoardEditButton board = [hsx|
  <a
    class="btn btn-sm btn-outline-info"
    style="margin-left:.5rem; padding:.125rem .25rem; font-size:.5rem;"
    href={EditBoardAction (get #id board)}
  >
    Edit
  </a>
  |]

renderBoardDeleteButton board = [hsx|
  <a
    class="btn btn-sm btn-outline-danger js-delete"
    style="padding:.125rem .25rem; font-size:.5rem;"
    href={DeleteBoardAction (get #id board)}
  >
    Delete
  </a>
  |]

renderOthersBoard :: (Board, "handle" :! Text, "displayName" :! Text) -> Html
renderOthersBoard (board, Arg handle, Arg displayName) = [hsx|
    <div class={"woc-board card mt-3 mb-3 " <> if private then "woc-board-private" else "" :: Text}>
        <div class="card-body">
            <h3>
                {when private (lockIcon <> space)}
                <a class="text-muted" href={ShowBoardAction (get #id board)}>{get #title board}</a>
            </h3>
            <a href={ShowUserAction (get #ownerId board)}>
                <span>
                    <span class="mr-2">{displayName}</span>
                    <em>@{handle}</em>
                </span>
            </a>
        </div>
    </div>
|]
  where
    private = case board ^. #settings_ % #visibility of
        VisibilityPublic -> False
        VisibilityPrivate -> True

-- | Like renderOthersBoard but without the names.
renderUserPageBoard :: Board -> Html
renderUserPageBoard board = [hsx|
    <div class={"woc-board card mt-3 mb-3 " <> if private then "woc-board-private" else "" :: Text}>
        <div class="card-body">
            <h3>
                {when private (lockIcon <> space)}
                <a href={ShowBoardAction (get #id board)}>{get #title board}</a>
            </h3>
        </div>
    </div>
|]
  where
    private = case board ^. #settings_ % #visibility of
        VisibilityPublic -> False
        VisibilityPrivate -> True

lockIcon :: Html
lockIcon = [hsx|<span>🔒</span>|]

space :: Html
space = [hsx|<span>{"\xa0" :: Text}</span>|]

---

userCrumb
  :: "active" :! Bool
  -> User
  -> Html
userCrumb (Arg active) user = [hsx|
  <li class={"breadcrumb-item " <> if active then "active" else "" :: Text}>
    {if active then text else link text}
  </li>
|]
  where
    link x = [hsx|<a href={ShowUserAction (get #id user)}>{x}</a>|]
    text = [hsx|<em>@{get #handle user}</em>|]

boardCrumb
  :: "active" :! Bool
  -> Board
  -> Html
boardCrumb (Arg active) board = [hsx|
  <li class={"breadcrumb-item " <> if active then "active" else "" :: Text}>
    {when private (lockIcon <> space)}
    {if active then text else link text}
  </li>
|]
  where
    link x = [hsx|<a href={ShowBoardAction (get #id board)}>{x}</a>|]
    text = [hsx|{get #title board}|]
    private = case board ^. #settings_ % #visibility of
        VisibilityPublic -> False
        VisibilityPrivate -> True

cardCrumb
  :: "active" :! Bool
  -> Card
  -> Html
cardCrumb (Arg active) card = [hsx|
  <li class={"breadcrumb-item " <> if active then "active" else "" :: Text}>
    {when private (lockIcon <> space)}
    {if active then text else link text}
  </li>
|]
  where
    link x = [hsx|<a href={ShowCardAction (get #id card)}>{x}</a>|]
    text = [hsx|{get #title card}|]
    private = case card ^. #settings_ % #visibility of
        VisibilityPublic -> False
        VisibilityPrivate -> True

---

gravatarHash :: Text -> Text
gravatarHash = show . md5 . LBS.fromStrict . T.encodeUtf8 . T.toLower . T.strip

gravatarSmall :: Text -> Html
gravatarSmall email = [hsx|
  <img class="userpic userpic-small" 
       width="32"
       src={"https://www.gravatar.com/avatar/" <> gravatarHash email <> "?s=64&d=mp"}/>
  |]