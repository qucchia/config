# This file sets up my configuration on a new computer.

# 1. Set keyboard layout according to user's preference.

prompt_layout() {
    read -p "Please choose a default keyboard layout, normal or chromebook [normal]: " layout
}

prompt_layout
until [[ $layout == "normal" ]] || [[ $layout == "chromebook" ]] || [[ -z "$layout" ]]; do
    prompt_layout
done

if [[ -z "$layout" ]]; then
    layout="normal"
fi

xmodmap_file="./${layout}.xmodmap"
xmodmap $xmodmap_file
cp $xmodmap_file ~/.Xmodmap

# 2. Setup git and forge

git config --global github.user qucchia

if [ ! -f ~/.authinfo ]; then
   cp .authinfo-model ~/.authinfo
   echo "To get forge working properly see https://magit.vc/manual/ghub/Creating-and-Storing-a-Token.html#Creating-and-Storing-a-Token"
fi
