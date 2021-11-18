# [wind of change](https://windofchange.me) — social note-taking

[**windofchange.me**](https://windofchange.me) is a social note-taking webapp written in Haskell with the [IHP framework](https://ihp.digitallyinduced.com/). You can see the [public boards](https://windofchange.me/Boards) without having to sign up.

I chose IHP because I wanted to write the app entirely by myself and I'm averse to learning new languages. I have some [reservations](https://windofchange.me/ShowCard?cardId=5bde1f3a-2b30-4085-bc04-b421eb3051ce) about IHP's typesafety, but I can't deny that building the app was easy and much less painful than any web technology I used in the past.

Initially I didn't want to open-source it because I didn't want to treat the code as a product (instead I wanted to treat the product as a product), but now I thought that maybe the code might be useful to somebody.

This code is licensed under MIT. You don't need to sign a copyright assignment if you want to contribute, but keep in mind that I might un-publish the repository at any time.

- `Application/Schema.sql` — DB schema
- `Web/Controller/` — backend logic
- `Web/View/` — HTML views (it's a serverside rendered web app)
