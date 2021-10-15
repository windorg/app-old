module Web.View.User.Show where

import Web.View.Prelude
import Web.Helper.View

data ShowView = ShowView { 
    user :: User,
    boards :: [Board]
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active">{get #displayName user}</li>
            </ol>
        </nav>
        <h1>
            <span class="mr-3">{get #displayName user}</span>
            <em>@{get #handle user}</em>
        </h1>
        <div class="row-cols-1 row-cols-md2">{forEach boards renderUserPageBoard}</div>
    |]
