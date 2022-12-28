.PHONY: install tree

install:
	stow -vv --no-folding bash git emacs spin

tree:
	tree -R -a -I .git .


# stow --adopt *
# git restore .

# Stow will create a symlink and overwrite the files inside your
# repository and git will undo the changes and return to the original
# files but the symlinks will stay there.
