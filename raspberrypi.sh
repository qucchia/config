# Remove preinstalled applications
sudo apt purge geany thonny -y
sudo apt autoremove -y

# Install applications
sudo apt update
sudo apt upgrade
sudo apt install emacs -y
sudo apt install isync mu4e pass -y

# Set keyboard
echo "xkbcomp ~/Documents/config/layout/.Xkeymap \$DISPLAY" >> ~/.xinitrc
bash ~/.xinitrc

# Set up git
git config --global user.name qucchia
git config --global user.email "qucchia0@gmail.com"
cat ~/Documents/config/.authinfo-model >> ~/.authinfo
echo "To get forge working properly see https://magit.vc/manual/ghub/Creating-and-Storing-a-Token.html#Creating-and-Storing-a-Token"

# Install fonts
mkdir -p ~/.local/share/fonts
cd /tmp
git clone -q https://github.com/adobe-fonts/source-code-pro.git
cp source-code-pro/TTF/*.ttf ~/.local/share/fonts
fc-cache -f -v

# Set up GitHub SSH
ssh-keygen -t ed25519 -C "qucchia0@gmail.com"
echo "Copy the key above and add it to your codeberg.org account to get SSH working."

# Allow config repo to use SSH
cd ~/Documents/config
git remote set-url origin git@codeberg.org:qucchia/config

# Make folders
mkdir ~/Projects ~/Mail

# Clone repos
git clone git@codeberg.org:qucchia/life ~/Documents/life
git clone git@codeberg.org:qucchia/school ~/Documents/school
git clone git@codeberg.org:qucchia/mafia-bot ~/Projects/mafia-bot

# Set up Emacs
ln -s ~/Documents/config/emacs/init.el ~/.emacs.d/init.el

# Set up EXWM
sudo ln -f ~/Documents/config/exwm/EXWM.desktop /usr/share/xsessions/EXWM.desktop

# Finally: Open Emacs!
emacs ~/Documents/config/RaspberryPiSetup.org -l ~/Documents/config/exwm/desktop.el -l ~/Documents/config/emacs/setup.el -mm
