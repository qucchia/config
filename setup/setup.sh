export CONFIG_DIR=$HOME/Documents/config
SETUP_DIR=$CONFIG_DIR/setup

sudo apt purge geany thonny -y
# sudo apt purge chromium-browser
sudo apt autoremove -y

# Add Firefox source
sudo echo "deb http://ports.ubuntu.com/ubuntu-ports bionic-updates main" >> /etc/apt/sources.list
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3B4FE6ACC0B21F32
sudo ln $SETUP_DIR/99bionic-updates /etc/apt/preferences.d/99bionic-updates

sudo apt update
sudo apt upgrade -y

sudo apt install emacs isync mu4e pass firefox -y

sudo apt install tor -y
sudo ln -f $SETUP_DIR/torrc /etc/tor/torrc

curl -sL https://deb.nodesource.com/setup_16.x | sudo -E bash -
sudo apt install -y nodejs
sudo npm i -g typescript typescript-language-server prettier

git config --global user.name qucchia
git config --global user.email "qucchia0@gmail.com"
git config --global init.defaultBranch main
cat $SETUP_DIR/.authinfo-model >> ~/.authinfo
echo "To get forge working properly see https://magit.vc/manual/ghub/Creating-and-Storing-a-Token.html#Creating-and-Storing-a-Token"

ssh-keygen -t ed25519 -C "qucchia0@gmail.com"
echo "Copy the key above and add it to your codeberg.org account to get SSH working."
cd $CONFIG_DIR
git remote set-url origin git@codeberg.org:qucchia/config

mkdir -p ~/Projects ~/Mail
git clone git@codeberg.org:qucchia/life ~/Documents/life
git clone git@codeberg.org:qucchia/school ~/Documents/school
git clone git@codeberg.org:qucchia/mafia-bot ~/Projects/mafia-bot
git clone git@codeberg.org:qbangle/qbangle ~/Projects/qbangle

mkdir -p ~/.local/share/fonts
cd /tmp
git clone -q https://github.com/adobe-fonts/source-code-pro.git
cp source-code-pro/TTF/*.ttf ~/.local/share/fonts
fc-cache -f -v

ln -s $CONFIG_DIR/emacs/init.el ~/.emacs.d/init.el
sudo ln -f $CONFIG_DIR/exwm/EXWM.desktop /usr/share/xsessions/EXWM.desktop

echo "sh ~/Documents/config/exwm/start-exwm.sh" > ~/.xsession
bash ~/.xsession

emacsclient $SETUP_DIR/FinalSetup.org
