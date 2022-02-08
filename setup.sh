# This file sets up my configuration on a new computer.

config_dir=`dirname $0`

read -p "Install Guix? [y/N] " yesno

if [ "$yesno" = y ] 
then
  cd /tmp
  wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
  chmod +x guix-install.sh
  sudo ./guix-install.sh

  echo $'GUIX_PROFILE="$HOME/.guix-profile"\n. "$GUIX_PROFILE/etc/profile"' >> ~/.bash_profile
fi

read -p "Set keyboard layout? [y/N] " yesno

if [ "$yesno" = y ] 
then
  echo $'1. Setting keyboard layout...\n'
  cp `dirname $0`/.Xmodmap ~/.Xmodmap
  xmodmap ~/.Xmodmap
fi

read -p "Set up git? [y/N] " yesno

if [ "$yesno" = y ] 
then
  git config --global github.user qucchia
  git config --global user.name qucchia
  git config --global user.email "qucchia0@gmail.com"
  if [ ! -f ~/.authinfo ]; then
     cp .authinfo-model ~/.authinfo
     echo "To get forge working properly see https://magit.vc/manual/ghub/Creating-and-Storing-a-Token.html#Creating-and-Storing-a-Token"
  fi
fi

read -p "Install fonts? [y/N] " yesno

if [ "$yesno" = y ] 
  mkdir -p ~/.local/share/fonts
  cd /tmp
  git clone -q https://github.com/adobe-fonts/source-code-pro.git
  cp source-code-pro/TTF/*.ttf ~/.local/share/fonts
  fc-cache -f -v
fi

read -p "Set up GitHub SSH? [y/N] " yesno

if [ "$yesno" = y ] 
  ssh-keygen -t ed25519 -C "qucchia0@gmail.com"
  echo "You may be asked for the authenticity of GitHub's fingerprint. See https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/githubs-ssh-key-fingerprints for GitHub's supported fingerprints."
  gh auth login
fi

