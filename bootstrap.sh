#!/usr/bin/env bash
set -euo pipefail


PYENV_LOCATION=~/.pyenv
PYENV_REPO=https://github.com/pyenv/pyenv.git
PYENV_PYTHON_GLOBAL=3.11.1
DOTFILES_LOCATION=~/.dotfiles
DOTFILES_REPO=https://github.com/fpatz/dotfiles.git


clone_if_not_there () {
    if [ ! -d $2 ]; then
        echo "==> cloning $1 to $2"
        git clone $1 $2
    fi
}


setup_pyenv () {
    clone_if_not_there $PYENV_REPO $PYENV_LOCATION
    (
        cd $PYENV_LOCATION;
        src/configure;
        make -C src
    )
    # add .pyenv to PATH
    PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
    echo "==> Python $PYENV_PYTHON_GLOBAL"
    pyenv install -s $PYENV_PYTHON_GLOBAL
    pyenv global $PYENV_PYTHON_GLOBAL
}


setup_dotfiles () {
    clone_if_not_there $DOTFILES_REPO $DOTFILES_LOCATION
    echo "==> stowing dotfiles"
    rm ~/.bashrc ~/.profile ~/.bash_logout
    (
        cd $DOTFILES_LOCATION;
        stow -v --no-folding bash git emacs spin
    )
}


bootstrap_emacs () {
    echo "==> bootstrapping Emacs configuration"
    emacs --script ~/.emacs.d/init.el
}

if type apt > /dev/null; then
    echo "==> this is a Debian-like system that has 'apt'"
    echo "==> updating the package database ..."
    sudo tee /etc/apt/sources.list.d/non-free.list > /dev/null <<EOF
# Add non-free
deb http://deb.debian.org/debian/ bullseye non-free
deb-src http://deb.debian.org/debian/ bullseye non-free
deb http://security.debian.org/debian-security bullseye-security non-free
deb-src http://security.debian.org/debian-security bullseye-security non-free
deb http://deb.debian.org/debian/ bullseye-updates non-free
deb-src http://deb.debian.org/debian/ bullseye-updates non-free
EOF
    sudo apt update
    echo "==> installing dev packages ..."
    sudo apt install -y \
         stow emacs emacs-common-non-dfsg \
         git fonts-firacode tree ripgrep jq \
         pax-utils pandoc \
         cmake build-essential libtool-bin libssl-dev \
         zlib1g-dev libbz2-dev libreadline-dev \
         libsqlite3-dev curl libncursesw5-dev xz-utils \
         tk-dev libxml2-dev libxmlsec1-dev libffi-dev \
         liblzma-dev libsasl2-dev python3-dev \
         libldap2-dev

    echo "==> installing TeX ..."
    sudo apt install -y texlive-full
fi

setup_dotfiles

setup_pyenv

bootstrap_emacs
