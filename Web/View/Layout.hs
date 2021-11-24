module Web.View.Layout where

import Application.Helper.View
import Generated.Types
import IHP.Controller.RequestContext
import IHP.Environment
import IHP.PageHead.Types
import IHP.ViewPrelude
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes
import Web.Types

data LayoutView = LayoutView
    { inboxCount :: Maybe Int
    }

socialTags :: Html
socialTags =
    [hsx|
  {ogTitleOrDefault "wind of change"}
  {ogDescriptionOrDefault "keep going"}
  <meta property="og:site_name" content="wind of change">
  <meta property="og:image" content="https://windofchange.me/favicon-square.png">

  <meta property="twitter:card" content="summary">
  <meta property="twitter:title" content={ogTitle}>
  <meta property="twitter:description" content={ogDescription}>
  <meta property="twitter:image" content="https://windofchange.me/favicon-square.png">
  |]
  where
    ogTitle = case maybeFromFrozenContext @OGTitle of
        Just (OGTitle title) -> title
        Nothing -> "wind of change"
    ogDescription = case maybeFromFrozenContext @OGDescription of
        Just (OGDescription desc) -> desc
        Nothing -> "keep going"

defaultLayout :: LayoutView -> Html -> Html
defaultLayout LayoutView{..} inner =
    H.docTypeHtml ! A.lang "en" $
        [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>{pageTitleOrDefault "wind of change"}</title>
    {descriptionOrDefault "keep going"}
    {socialTags}
</head>
<body>
    <div class="container mt-4">
        <div class="d-flex justify-content-end align-items-center align-self-center mb-3">
            <div style="position:relative">
                <a href="/" class="stretched-link text-reset text-decoration-none">
                    <img src="/favicon-large.png" width="50" class="mr-2 woc-logo-icon">
                    <span class="woc-logo-text">wind of change</span>
                </a>
            </div>
            <div class="d-flex" style="flex: 1"></div>
            <div id="changelog-trigger" class="d-flex align-items-center align-self-center mr-2" style="cursor:pointer">
                <div class="text-primary">News</div>
                <div id="changelog-badge" style="height:32px;width:32px;min-height:1px;"></div>
            </div>
            {feed}
            {inbox}
            <div>{loginOrLogout}</div>
        </div>
        {renderFlashMessages}
        {inner}
        {modal}
    </div>
    <footer class="container py-4">
        <div class="text-center text-muted small">
            made by <a href="https://twitter.com/availablegreen">Artyom Kazak</a> • built with <a href="https://ihp.digitallyinduced.com/">IHP</a> • favicon by <a href="https://loading.io/">loading.io</a>
        </div>
    </footer>
</body>
|]
  where
    inbox = case inboxCount of
        Just count ->
            let badge
                    | count == 0 = [hsx|<span id="inbox-badge" class="ml-2 badge badge-secondary">0</span>|]
                    | otherwise = [hsx|<span id="inbox-badge" class="ml-2 badge badge-danger">{count}</span>|]
             in [hsx|<div class="mr-4"><a href={ShowInboxAction}>Inbox{badge}</a></div>|]
        Nothing -> mempty
    feed = case currentUserOrNothing of
        Just _ -> [hsx|<div class="mr-4"><a href={ShowFeedAction Nothing}>Feed</a></div>|]
        Nothing -> mempty
    loginOrLogout = case currentUserOrNothing of
        Just _ ->
            [hsx|
          <form class="d-inline" method="POST" data-disable-javascript-submission="true" action={LogoutAction}>
              <button class="btn btn-link p-0" type="submit">Logout</button>
          </form>
          |]
        Nothing ->
            [hsx|
          <a href={LoginOrSignupAction}>Login or sign up</a>
          |]

-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production
-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed
-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details

stylesheets :: Html
stylesheets =
    [hsx|
        <link
          rel="stylesheet"
          href="https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,300;0,400;0,600;0,700;0,800;1,300;1,400;1,600;1,700;1,800&display=swap"
        />
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts =
    [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-3.6.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>

        <!-- Called in app.js -->
        <script src="https://cdnjs.cloudflare.com/ajax/libs/bootbox.js/5.5.2/bootbox.min.js"></script>
        <script type="module" src={assetPath "/app.js"}></script>
    |]

devScripts :: Html
devScripts =
    [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"}></script>
    |]

metaTags :: Html
metaTags =
    [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <link rel="manifest" href="/manifest.json">
    <meta name="apple-mobile-web-app-capable" content="yes">
    <meta name="apple-mobile-web-app-status-bar-style" content="default">
    <link rel="icon" href="favicon.png">
    <link rel="apple-touch-icon" href="favicon-large.png">
    <meta name="theme-color" content="#ffffff">
    {autoRefreshMeta}
|]
