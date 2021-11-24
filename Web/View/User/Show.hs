module Web.View.User.Show where

import Web.Helper.View
import Web.View.Prelude

data ShowView = ShowView
    { user :: User,
      boards :: [Board],
      -- Nothing if there's no current user
      followed :: Maybe Bool
    }

instance View ShowView where
    beforeRender ShowView{..} = do
        setTitle $ format "{} @{} / wind of change" (get #displayName user) (get #handle user)
        setOGTitle $ format "{} @{}" (get #displayName user) (get #handle user)

    html ShowView{..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                {userCrumb (($) #active True) user}
            </ol>
        </nav>
        <h1>
            <span class="mr-3">{get #displayName user}</span>
            <em>@{get #handle user}</em>
            {followUnfollow}
        </h1>
        <div class="row-cols-1 row-cols-md2">{forEach boards renderUserPageBoard}</div>
    |]
      where
        followUnfollow = case followed of
            Nothing -> mempty
            Just False ->
                [hsx|
            <form class="d-inline" method="POST" action={UpdateFollowUserAction (get #id user)}>
              <button class="ml-3 btn btn-outline-primary btn-sm">Follow</button>
            </form>
            |]
            Just True ->
                [hsx|
            <form class="d-inline" method="POST" action={UpdateUnfollowUserAction (get #id user)}>
              <button class="ml-3 btn btn-outline-secondary btn-sm">Unfollow</button>
            </form>
            |]
