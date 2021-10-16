module Web.Controller.Feed where

import Web.Controller.Prelude
import Web.Controller.Authorization
import Web.View.Feed.Show
import Web.ViewTypes
import Optics (view)

-- | Last 14 days in newest->oldest order
getFeedItems :: (?context::ControllerContext, ?modelContext :: ModelContext) => IO [FeedItem]
getFeedItems = do
  followedUsers <- query @FollowedUser
    |> filterWhere (#subscriberId, currentUserId)
    |> fetch
    <&> map (get #followedUserId)
  feedItems <- query @CardUpdate
    -- TODO: this should be authorId but we don't have authorId yet.. although maybe it shouldn't be that?
    |> filterWhereIn (#ownerId, followedUsers)
    |> filterWhereSql (#createdAt, ">= current_timestamp - interval '14 days'")
    |> orderByDesc #createdAt
    |> fetch
    >>= filterM (userCanView @CardUpdate . get #id)
    <&> map FeedItemCardUpdate
  pure feedItems

instance Controller FeedController where
    beforeAction = ensureIsUser

    action ShowFeedAction = do
      feedItems <- mapM fetchFeedItemV =<< getFeedItems
      render FeedView{..}
