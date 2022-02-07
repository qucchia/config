# This file sets up my configuration on a new computer.

config_dir=`dirname $0`

# 1. Set keyboard layout

echo $'1. Setting keyboard layout...\n'
cp `dirname $0`/full.xmodmap ~/.Xmodmap
xmodmap ~/.Xmodmap

# 2. Setup git and forge

echo $'\n2. Setting up git...\n'

git config --global github.user qucchia
git config --global user.name qucchia
git config --global user.email "qucchia0@gmail.com"

if [ ! -f ~/.authinfo ]; then
   cp .authinfo-model ~/.authinfo
   echo "To get forge working properly see https://magit.vc/manual/ghub/Creating-and-Storing-a-Token.html#Creating-and-Storing-a-Token"
fi

# 3. Install fonts

echo $'\n3. Installing fonts...\n'

mkdir -p ~/.local/share/fonts
cd /tmp
git clone -q https://github.com/adobe-fonts/source-code-pro.git
cp source-code-pro/TTF/*.ttf ~/.local/share/fonts
fc-cache -f -v

echo $'\nDone!'
