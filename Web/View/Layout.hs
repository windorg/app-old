module Web.View.Layout where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Generated.Types
import IHP.Controller.RequestContext
import Web.Types
import Web.Routes
import Application.Helper.View

data LayoutView = LayoutView {
    inboxCount :: Maybe Int
}

defaultLayout :: LayoutView -> Html -> Html
defaultLayout LayoutView{..} inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>{pageTitleOrDefault "wind of change"}</title>
</head>
<body>
    <div class="container mt-4">
        <div class="d-flex justify-content-end align-items-center align-self-center mb-3">
            <div id="changelog-trigger" class="d-flex align-items-center align-self-center mr-2" style="cursor:pointer">
                <div class="text-primary">News</div>
                <div id="changelog-badge" style="height:32px;width:32px;min-height:1px;"></div>
            </div>
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
          let badge | count == 0 = [hsx|<span id="inbox-badge" class="ml-2 badge badge-secondary">0</span>|]
                    | otherwise  = [hsx|<span id="inbox-badge" class="ml-2 badge badge-danger">{count}</span>|]
          in [hsx|<div class="mr-4"><a href={ShowInboxAction}>Inbox{badge}</a></div>|]
        Nothing -> mempty
    loginOrLogout = case currentUserOrNothing of
        Just _ -> [hsx|<a class="js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>|]
        Nothing -> [hsx|
          <a class="mr-3" href={NewSessionAction}>Login</a>
          <a href={NewUserAction}>Sign up</a>
          |]

-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production
-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed
-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        <script src="//unpkg.com/@ungap/custom-elements"></script>
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
        <script src="https://cdnjs.cloudflare.com/ajax/libs/autosize.js/5.0.1/autosize.min.js"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/bootbox.js/5.5.2/bootbox.min.js"></script>
        <script type="module" src={assetPath "/app.js"}></script>
    |]

devScripts :: Html
devScripts = [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"}></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <link rel="manifest" href="/manifest.json">
    <meta name="apple-mobile-web-app-capable" content="yes">
    <meta name="apple-mobile-web-app-status-bar-style" content="default">
    <link rel="icon" href="favicon.png">
    <link rel="apple-touch-icon" href="favicon-large.png">
    <meta name="theme-color" content="#ffffff">
    <meta property="og:title" content="wind of change"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="https://windofchange.me"/>
    <meta property="og:description" content="keep going"/>
    {autoRefreshMeta}
|]
