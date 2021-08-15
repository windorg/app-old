module Web.View.User.Index where
import Web.View.Prelude

data IndexView = IndexView { user :: [User] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={UsersAction}>Users</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewUserAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>User</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach user renderUser}</tbody>
            </table>
        </div>
    |]


renderUser :: User -> Html
renderUser user = [hsx|
    <tr>
        <td>{user}</td>
        <td><a href={ShowUserAction (get #id user)}>Show</a></td>
        <td><a href={EditUserAction (get #id user)} class="text-muted">Edit</a></td>
        <td><a href={DeleteUserAction (get #id user)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
