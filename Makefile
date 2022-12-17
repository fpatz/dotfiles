.PHONY: install

install:
	stow -vvv --no-folding bash git emacs


# stow --adopt *
# git restore .

# Stow will create a symlink and overwrite the files inside your
# repository and git will undo the changes and return to the original
# files but the symlinks will stay there.
