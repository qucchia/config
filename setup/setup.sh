echo -e "\nqucchia's config setup"
export CONFIG_DIR=$HOME/Documents/config
export SETUP_DIR=$CONFIG_DIR/setup

echo -e "\n\nSetting keyboard mapping..."
xmodmap $CONFIG_DIR/layout/.Xmodmap

echo -e "\nRemoving preinstalled applications..."
sudo apt purge geany thonny -y
sudo apt autoremove -y

sudo ln -s $SETUP_DIR/firefox.list /etc/apt/sources.list.d/firefox.list
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3B4FE6ACC0B21F32
sudo ln -s $SETUP_DIR/99bionic-updates /etc/apt/preferences.d/99bionic-updates

echo -e "\n\nUpdating package sources..."
sudo apt update
echo -e "\n\nUpgrading packages..."
sudo apt upgrade -y

echo -e "\n\nInstalling Emacs..."
sudo apt install -y emacs

echo -e "\n\nInstalling Firefox..."
sudo apt install -y firefox

echo -e "\n\nInstalling pass..."
sudo apt install -y pass

echo -e "\n\nInstalling isync (mbsync) and mu4e..."
sudo apt install -y isync mu4e

echo -e "\n\nInstalling Tor..."
sudo apt install tor -y
sudo mv /etc/tor/torrc /etc/tor/torrc-backup
sudo ln -f $SETUP_DIR/torrc /etc/tor/torrc

echo -e "\n\nRunning Nodesource script..."
curl -sL https://deb.nodesource.com/setup_16.x | sudo -E bash -
echo -e "\n\nInstalling Node.js and NPM..."
sudo apt install -y nodejs

echo -e "\n\nUpdating NPM..."
sudo npm i -g npm
echo -e "\n\nInstalling TypeScript..."
sudo npm i -g typescript typescript-language-server
echo -e "\n\nInstalling Prettier..."
sudo npm i -g prettier

echo -e "\n\nDownloading and building Neovim..."
sudo apt install -y ninja-build gettext libtool libtool-bin autoconf automake cmake g++ pkg-config unzip curl doxygen
cd /tmp
git clone https://github.com/neovim/neovim.git
cd neovim
git checkout stable
make
sudo make install

mkdir -p ~/.config/nvim
ln -s $CONFIG_DIR/init.nvim ~/.config/nvim/init.nvim 

# Vim-plug
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

echo -e "\n\nInstalling Rustup..."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

echo -e "\n\nDonwloading and building Alacritty..."
sudo apt install cmake pkg-config libfreetype6-dev libfontconfig1-dev libxcb-xfixes0-dev libxkbcommon-dev python3 gzip -y
cd /tmp
git clone https://github.com/alacritty/alacritty
cd alacritty

# Build
cargo build --release

# Terminfo
sudo tic -xe alacritty,alacritty-direct extra/alacritty.info

# Desktop Entry
sudo cp target/release/alacritty /usr/local/bin # or anywhere else in $PATH
sudo cp extra/logo/alacritty-term.svg /usr/share/pixmaps/Alacritty.svg
sudo desktop-file-install extra/linux/Alacritty.desktop
sudo update-desktop-database

# Manual Page
sudo mkdir -p /usr/local/share/man/man1
gzip -c extra/alacritty.man | sudo tee /usr/local/share/man/man1/alacritty.1.gz > /dev/null
gzip -c extra/alacritty-msg.man | sudo tee /usr/local/share/man/man1/alacritty-msg.1.gz > /dev/null

# Bash completions
mkdir -p ~/.bash_completion
cp extra/completions/alacritty.bash ~/.bash_completion/alacritty
echo "source ~/.bash_completion/alacritty" >> ~/.bashrc

mkdir -p ~/.config/alacritty
ln -s $CONFIG_DIR/alacritty.yml ~/.config/alacritty/alacritty.yml

git config --global user.name qucchia
git config --global user.email "qucchia0@gmail.com"
git config --global init.defaultBranch main

cd $CONFIG_DIR
git remote set-url origin git@codeberg.org:qucchia/config

echo -e "\n\nInstalling fonts..."

mkdir -p ~/.local/share/fonts
cd /tmp
git clone -q https://github.com/adobe-fonts/source-code-pro.git
cp source-code-pro/TTF/*.ttf ~/.local/share/fonts
fc-cache -f

ln -s $CONFIG_DIR/emacs/init.el ~/.emacs.d/init.el
sudo ln -f $CONFIG_DIR/exwm/EXWM.desktop /usr/share/xsessions/EXWM.desktop

echo "sh ~/Documents/config/exwm/start-exwm.sh" > ~/.xsession

echo -e "\n\n\n"
cat $SETUP_DIR/final-setup.txt
