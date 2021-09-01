module Web.View.User.Show where
import Web.View.Prelude

data ShowView = ShowView { user :: User }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active">Show User</li>
            </ol>
        </nav>
        <h1>Show User</h1>
        <p>{user}</p>
    |]
