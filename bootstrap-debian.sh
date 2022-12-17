# - bootstrap a Debian system

PACKAGES=\
        stow emacs git fonts-firacode cmake build-essential libtool-bin \
        libssl-dev zlib1g-dev libbz2-dev \
        libreadline-dev libsqlite3-dev curl libncursesw5-dev \
        xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev \
        liblzma-dev libsasl2-dev python3-dev libldap2-dev 

sudo apt update
sudo apt install $PACKAGES

git clone https://github.com/pyenv/pyenv.git ~/.pyenv
cd ~/.pyenv && src/configure && make -C src

cat <<EOF >> .profile
# add .pyenv to PATH
if [ -d "$HOME/.pyenv/bin" ] ; then
    PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
fi
EOF

pyenv install 3.11.1
pyenv global 3.11.1

git clone git@git.contact.de:frank/dotemacs.git ~/.emacs.d
emacs --script .emacs.d/init.el 

sudo apt-get remove --purge libreoffice*
sudo apt-get clean
sudo apt-get autoremove
