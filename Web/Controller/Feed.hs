module Web.Controller.Feed where

import Web.Controller.Prelude
import Web.Controller.Authorization
import Web.View.Feed.Show
import Web.ViewTypes
import Optics (view)
import Named

-- | Last N days in newest->oldest order
getFeedItems 
  :: (?context::ControllerContext, ?modelContext :: ModelContext)
  => "days" :! Int
  -> IO [FeedItem]
getFeedItems (Arg days) = do
  ensureIsUser
  followedUsers <- query @FollowedUser
    |> filterWhere (#subscriberId, currentUserId)
    |> fetch
    <&> map (get #followedUserId)
  feedItems <- query @CardUpdate
    -- TODO: this should be authorId but we don't have authorId yet.. although maybe it shouldn't be that?
    |> filterWhereIn (#ownerId, followedUsers)
    |> filterWhereSql (#createdAt, format ">= current_timestamp - interval '{} days'" days)
    |> orderByDesc #createdAt
    |> fetch
    >>= filterM (userCanView @CardUpdate . get #id)
    <&> map FeedItemCardUpdate
  pure feedItems

instance Controller FeedController where
    beforeAction = ensureIsUser

    action ShowFeedAction {days} = do
      let days' = clip (1, 31) $ fromMaybe 3 days
      feedItems <- mapM fetchFeedItemV =<< getFeedItems (#days days')
      render FeedView {feedItems, days = days'}

-- clip (1, 10) 3 == max 1 (min 10 3) == 3
-- clip (1, 10) 100 = max 1 (min 10 100) == 10
clip :: (Ord a) => (a, a) -> a -> a
clip (min_, max_) a = max min_ $ min max_ $ a