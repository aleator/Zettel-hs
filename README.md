# Zettel -- A simplified command line ZettelKasten manager

Do you like working from terminal? Perhaps you're a vim user?
If that is the case, here is a simple program for maintaining 
your zettelkasten for you.

## What does it do?

It creates and manages links for a collection of plain text
zettelkasten files. The main use for the program is to manage
links between the individual notes, which is the most toilsome
part of maintaining a zettelkasten using plain text notes.

```
$ Zettel --help
Usage: Zettel (COMMAND | COMMAND | COMMAND | COMMAND | COMMAND)
  Manipulate zettelkasten

Available options:
  -h,--help                Show this help text

Available commands:
  create                   Create unlinked zettel
  link                     Link zettels
  extend                   Create new zettel and link it to original
  find                     Find zettels
  clique                   Build cliques by cross linking selected zettels
```

For further details, pass `--help` as argument for each command (e.g., `Zettel link --help`)

## Vim integration

There isn't one, but you can use Zettel from Vim. For example:

* Add links for current note, do `:term Zettel link -- --origin %:t --search <keyword>`

* Extend a note, e.g.,  create and link to previous, do
  `:!Zettel extend -- --origin %:t --title <new-title>`

## Shell integration

There isn't a shell integration either. Here are some common commands
you might use:

* To edit zettels you could do `Zettel find --search <term>|xargs -o nvim -O`

* To create an unlined note, run `Zettel create --title <title>|xargs -o nvim`

## Installation

You need to install the excellent [`fzf`](https://github.com/junegunn/fzf) and
[`rg`](https://github.com/BurntSushi/ripgrep) programs first. 

Then install
[stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). Then
clone this repository and issue `stack install` inside the repository. Then go
have a coffee and a sandwich while the program builds.

After the program has been built, create the directory `~/zettel/` and
create your first zettel by `Zettel create --title <title>|xargs -o nvim`.

