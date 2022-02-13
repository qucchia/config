# Remove preinstalled applications
sudo apt purge geany thonny -y
sudo apt autoremove -y

# Add ungoogled-chromium and gh sources
echo 'deb http://download.opensuse.org/repositories/home:/ungoogled_chromium/Debian_Bullseye/ /' | sudo tee /etc/apt/sources.list.d/home-ungoogled_chromium.list > /dev/null
curl -s 'https://download.opensuse.org/repositories/home:/ungoogled_chromium/Debian_Bullseye/Release.key' | gpg --dearmor | sudo tee /etc/apt/trusted.gpg.d/home-ungoogled_chromium.gpg > /dev/null
curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/etc/apt/trusted.gpg.d/githubcli-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/trusted.gpg.d/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null

# Install applications
sudo apt update
sudo apt upgrade
sudo apt install emacs ungoogled-chromium gh -y

# Set keyboard
echo "xkbcomp ~/Documents/config/layout/.Xkeymap \$DISPLAY" >> ~/.xinitrc
bash ~/.xinitrc

# Set up git
git config --global github.user qucchia
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
echo "You may be asked for the authenticity of GitHub's fingerprint. See https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/githubs-ssh-key-fingerprints for GitHub's supported fingerprints."
gh auth login

# Allow config repo to use SSH
cd ~/Documents/config
git remote set-url origin git@github.com:qucchia/config

# Clone repos
gh repo clone qucchia/life ~/Documents/life
mkdir ~/Projects
gh repo clone qucchia/mafia-bot-new ~/Projects/mafia-bot-new

# Set up Emacs
ln ~/Documents/config/emacs/init.el ~/.emacs.d/init.el

# Set up EXWM
sudo ln -f ~/Documents/config/exwm/EXWM.desktop /usr/share/xsessions/EXWM.desktop

# Finally: Open Emacs!
emacs ~/Documents/config/RaspberryPiSetup.org -l ~/Documents/config/emacs/setup.el -mm
