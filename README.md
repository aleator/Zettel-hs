# Zettel -- A simplified command line ZettelKasten manager

Do you like working from terminal? Perhaps you're a vim user?
If that is the case, here is a simple program for maintaining 
your zettelkasten for you.

## What does it do?

It creates and manages links for a collection of plain text
zettelkasten files. The main use for the program is to manage
links between the individual notes, which is the most toilsome
part of maintaining a plain text zettelkasten.

```
ZKHS -- simple text based zettelkasten system

Usage: Zettel COMMAND
  Manipulate zettelkasten

Available options:
  -h,--help                Show this help text

Available commands:
  create                   Create unlinked zettel
  link                     Link zettels
  find                     Find zettels
  resolve                  Resolve references in zettels
  export                   Export zettels as JSON
  elucidate                Suggest improvements in ZettelKasten
  neighbourhood            Zettels linkwise near to this one
  body                     Extract zettel body, ie. text without headers and
                           links
  references               Extract references from a Zettel
  addreferences            Add references to a Zettel
  auto-fill                Fill missing wikilinks and references from origin
```

For further details, pass `--help` as argument for each command (e.g., `Zettel link --help`)

## What do the Zettels look like?

Here is an example from my collection:

```
The binomial distribution
--------------------------------------------------------------------------------

           ┌────────────────────────────────────────┐
           │              ⎛n⎞                       │
           │ B(k; n, p) = ⎜ ⎟ * p^k * (1 - p)^(n-k) │
           │              ⎝k⎠                       │
           └────────────────────────────────────────┘

"What is the probability to get k successes in n trials, each with probability
p of success?"

cf. `pbinom` in r.

----- External references ------------------------------------------------------
[1]: Binomial distribution - Wikipedia [WWW Document], n.d. URL https://en.wikipedia.org/wiki/Binomial_distribution (accessed 2.20.20).

--------------------------------------------------------------------------------
Tags: statistics
Links: 
67257A06-4634-40EA-A788-011771081C40-book-bayesian-statistics-the-fun-way Origin
--------------------------------------------------------------------------------
```



## Status

This is a program build by me and for me. It misbehaves every now and then,
but since I and it are on familiar terms, it doesn't really matter.

But, if you decide to use it, this might matter a lot. Unless you like
debugging and fixing issues, it might be better for you to write your
own personal system instead.

## Supported features

* Structured zettels
* External (bibliographic) references
* Links with descriptive text
* [wikilike links], which can be embedded in the text
* Navigation and searching
* Some link structure based tools, like neighbourhood search, which
  I find really useful
* Neat 'elucidate' command, which causes the program to prompt you to
  improve something. Currently it asks you to link unlinked zettels, but
  in the future it will likely ask you to split up long zettels or 
  return to zettels that you have marked as WIP or TODO.

## Bad features

* You can't use symbols or linebreaks in [wikilike links]. Sorry.
* If you abort fzf, the vim integration can do whatever it likes.
  It often likes to close a random buffer.
* Zettels with broken structure cause commands to fail silently in
  vim. But they are easy to find using `Zettel find -q 'anything'`,
  so I've not bothered to do anything about this.

## Vim integration

There is a somewhat badly behaving [vim integration](zettel.vim). Add
`source <path where you put it>/zettel.vim` to your init.vim or vimrc to
enable it. Here is a quick user quide

## Commands

`:ZFill` : Automatically *fill* in links and refs from origin zettel
`:Zlnk`  : Spawn fzf to gather *links* for this zettel
`:Zf`    : Do *full text search* on arqument. (See [tantivy])

## Mappings (prefix with localleader)
    
### Manipulation
`zl`    : Add *Links* to call zettel 
`zw`    : Add *WikiLinks* to zettel 
`zs`    : *Split visual selection* to a new zettel (see ZFill above)

### Creation & Navigation
`zr`    : *Navigate to wikilink*. Creates the link if it doesn't
        exist

## Navigation (& creation)
`zf`    : *Find* a zettel (type a new title, or press <ctrl-n> to 
        create a new zettel)
`zg`    : *Fuzzy find*, but limit to word under cursor (TODO: Do full
        text find for this!)
`zf`    : *Fuzzy find* zettel by title

## Network local discovery 
`zn`    : Show *neighbourhood* of the zettel
`zt`    : Show *origin chain* of the zettel

Zettels can be created using the command `:Zext 'title for new zettel'`. This
also creates a bidirectional link between currently viewed zettel and the new
one, called the Origin-link.  

If you want an unlinked zettel, the command `:Zcre 'title for new zettel'`
creates one.

You can also create new zettels by invoking find and then typing a title which
produces no find results. This is the fastest way.

## Shell integration

There isn't a shell integration. Here are some common commands
you might use:

* To edit zettels you could do `Zettel find --search <term>|xargs -o nvim -O`

* To create an unlined note, run `Zettel create --title <title>|xargs -o nvim`

## Installation

You need to install the excellent [`fzf`](https://github.com/junegunn/fzf) and
[`rg`](https://github.com/BurntSushi/ripgrep) programs first.  Also, the search
command can use [`tantivy-cli`](https://github.com/tantivy-search/tantivy-cli)
for full text searches.

Then install
[stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). Then
clone this repository and issue `stack install` inside the repository. Then go
have a coffee and a sandwich while the program builds.

After the program has been built, create the directory `~/zettel/` and
create your first zettel by `Zettel create --title <title>|xargs -o nvim`.

