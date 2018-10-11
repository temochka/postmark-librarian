# Postmark Librarian

Librarian is a toy project I built when evaluating the viability of using Haskell for building robust command-line tools for my routine tasks at [Postmark](https://postmarkapp.com). Check out my [blog post](https://temochka.com/blog/posts/2017/03/30/haskell-at-work.html) that details my experience building this little tool that downloads messages from the Postmark API.

## Build & Run

You'll need to have [Stack](https://haskellstack.org) installed.

Clone this repo and set things up:

``` bash
git clone https://github.com/temochka/postmark-librarian.git
cd postmark-librarian
stack setup
```

Build and run:

``` bash
stack build
stack exec librarian
```

Follow the instructions displayed by the binary.
